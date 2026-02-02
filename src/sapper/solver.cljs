(ns sapper.solver2
  (:require
   [clojure.string :as str]
   [sapper.core :as core])
  (:require-macros
   [sapper.macros :refer [cond+ either]]))

(def w)
(def h)
(def total-flags)
(def idxs)
(def neighbours)
(def neighbours-cache (js/Map.))
(def known)
(def candidates)
(def active-constraints)

(defclass Problem
  (field field)
  (field flagged)
  (field unknown)
  (field states)
  (field changes)
  (constructor [_ a b c d e]
    (set! field   a)
    (set! flagged b)
    (set! unknown c)
    (set! states  d)
    (set! changes e)))

(defn count-chars [arr indices ch]
  (loop [res 0
         idx 0]
    (cond+
      (>= idx (alength indices))
      res

      (identical? ch (aget arr (aget indices idx)))
      (recur (inc res) (inc idx))

      :else
      (recur res (inc idx)))))

(defn emojify [s]
  (-> s
    (str/replace "F" "🚩")
    (str/replace "?" "❓")
    (str/replace "." "⬜")
    (str/replace "0" "0️⃣")
    (str/replace "1" "1️⃣")
    (str/replace "2" "2️⃣")
    (str/replace "3" "3️⃣")
    (str/replace "4" "4️⃣")
    (str/replace "5" "5️⃣")
    (str/replace "6" "6️⃣")
    (str/replace "7" "7️⃣")
    (str/replace "8" "8️⃣")
    (str/replace "S" "🟩")
    (str/replace "D" "🟥")))

(defn field-str [field]
  (str "\n\n" (str/join "\n" (map #(emojify (str/join %)) (partition w field)))))

(defn string-with [s i ch]
  (str (.slice s 0 i) ch (.slice s (inc i))))

(defn clone-problem [{:keys [field flagged unknown states changes]}]
  (Problem. (.slice field) flagged unknown (js/Map. states) (.slice changes)))

(defn with-ch [problem i ch]
  (let [{:keys [field flagged unknown changes]} problem]
    (aset field i ch)
    (set! (.-unknown problem) (dec unknown))
    (.push changes [i ch])
    (when (identical? "F" ch)
      (set! (.-flagged problem) (inc flagged)))
    problem))

;; [*] Count amount of mines
(defn cs-total-init [{:keys [field states]}]
  (assoc! states :total
    {:flagged (count-chars field idxs "F")
     :unknown (count-chars field idxs ".")}))

(defn cs-total-check-incr [{:keys [field states changes]}]
  (let [[flagged unknown]
        (loop [flagged    (:flagged (:total states))
               unknown    (:unknown (:total states))
               change-idx 0]
          (if (>= change-idx (alength changes))
            [flagged unknown]
            (let [[_idx ch] (nth changes change-idx)]
              (cond
                (identical? "F" ch) (recur (inc flagged) (dec unknown) (inc change-idx))
                (identical? "?" ch) (recur      flagged  (dec unknown) (inc change-idx))
                :else               (throw (js/Error. (str "Unepxected change" _idx ch)))))))]
    (assoc! states :total {:flagged flagged :unknown unknown})
    (and
      ;; did not placed too many flags
      (<= flagged total-flags)
      ;; have enough space to get to total
      (>= (+ flagged unknown) total-flags))))

(defn cs-total-check [{:keys [flagged unknown]}]
  (and
    ;; did not placed too many flags
    (<= flagged total-flags)
    ;; have enough space to get to total
    (>= (+ flagged unknown) total-flags)))

;; [V] Nothing special
(defn cs-vanilla-init [{:keys [field states]}]
  (let [vanilla-flags      (js/Map.)
        vanilla-unknown    (js/Map.)
        vanilla-neighbours (js/Map.)]
    ;; For each numbered cell, count surrounding F and .
    (doseq [i known
            :let [nbs (aget neighbours i)]]
      (assoc! vanilla-flags i (count-chars field nbs "F"))
      (assoc! vanilla-unknown i (count-chars field nbs ".")))
    ;; For each ".", find numbered cells surrounding it
    (doseq [i candidates
            :let [nbs (aget neighbours i)]]
      (assoc! vanilla-neighbours i (filterv #(<= "0" (aget field %) "8") nbs)))
    (assoc! states
      :vanilla/flags vanilla-flags
      :vanilla/unknown vanilla-unknown
      :vanilla/neighbours vanilla-neighbours)))

(defn cs-vanilla-check-incr [{:keys [field states changes] :as problem}]
  (let [vanilla-flags      (js/Map. (.get states :vanilla/flags))
        vanilla-unknown    (js/Map. (.get states :vanilla/unknown))
        vanilla-neighbours (.get states :vanilla/neighbours)
        affected           (js/Set.)]
    (and
      (every? true?
        (for [[idx ch] changes
              num-idx  (or (.get vanilla-neighbours idx) [])
              :let [value    (parse-long (aget field num-idx))
                    flags    (.get vanilla-flags num-idx)
                    unknown  (.get vanilla-unknown num-idx)
                    flags'   (cond-> flags (identical? "F" ch) inc)
                    unknown' (dec unknown)
                    _        (assoc! vanilla-flags   num-idx flags')
                    _        (assoc! vanilla-unknown num-idx unknown')]]
          (and
            ;; did not placed too many flags
            (<= flags' value)
            ;; have enough space to get to total
            (>= (+ flags' unknown') value))))
      (assoc! states
        :vanilla/flags vanilla-flags
        :vanilla/unknown vanilla-unknown))))

(defn cs-vanilla-check [{:keys [field]}]
  (every?
    (fn [i]
      (let [value   (parse-long (aget field i))
            nbs     (aget neighbours i)
            fs      (count-chars field nbs "F")
            unknown (count-chars field nbs ".")]
        (and
          ;; did not placed too many flags
          (<= fs value)
          ;; have enough space to get to total
          (>= (+ fs unknown) value))))
    known))

;; [Q] There must be at least 1 mine in every 2x2 area
(defn cs-quad-check [{:keys [field]}]
  (every?
    (fn [[x y]]
      (or
        (either identical? "F" "." (aget field (+ (* y w) x)))
        (either identical? "F" "." (aget field (+ (* y w) (+ x 1))))
        (either identical? "F" "." (aget field (+ (* (+ y 1) w) x)))
        (either identical? "F" "." (aget field (+ (* (+ y 1) w) (+ x 1))))))
    (for [x (range (dec w))
          y (range (dec h))]
      [x y])))

;; [C] All mines are orthogonally or diagonally connected
(defn cs-connected-check [{:keys [field]}]
  (let [flag-indices (filterv #(either identical? "F" "." (aget field %)) idxs)]
    ;; DFS through F or . cells
    (let [start   (first flag-indices)
          visited (js/Set. [start])
          queue   [start]]
      (loop []
        (when-some [current (.pop queue)]
          (doseq [nb    (aget neighbours current)
                  :when (not (.has visited nb))
                  :let  [ch (aget field nb)]
                  :when (either identical? "F" "." ch)]
            (conj! visited nb)
            (conj! queue nb))
          (recur)))
      (every? #(.has visited %) flag-indices))))

;; [T] Flags may not form row of three orthogonally or diagonally
(defn check-triplet [field x y dx1 dy1 dx2 dy2]
  (let [x1 (+ x dx1)  y1 (+ y dy1)
        x2 (+ x dx2)  y2 (+ y dy2)]
    (and
      (>= x1 0) (< x1 w) (>= y1 0) (< y1 h)
      (>= x2 0) (< x2 w) (>= y2 0) (< y2 h)
      (identical? "F" (aget field (+ x1 (* y1 w))))
      (identical? "F" (aget field (+ x2 (* y2 w)))))))

(defn cs-anti-triplet-check-incr [{:keys [field changes]}]
  (reduce
    (fn [_ [idx ch]]
      (if (identical? "?" ch)
        true
        (let [y (quot idx w)
              x (mod idx w)]
          (if (or
                ;; horizontal: start, middle, end
                (check-triplet field x y  1  0  2  0)
                (check-triplet field x y -1  0  1  0)
                (check-triplet field x y -2  0 -1  0)
                ;; vertical: start, middle, end
                (check-triplet field x y  0  1  0  2)
                (check-triplet field x y  0 -1  0  1)
                (check-triplet field x y  0 -2  0 -1)
                ;; diagonal \: start, middle, end
                (check-triplet field x y  1  1  2  2)
                (check-triplet field x y -1 -1  1  1)
                (check-triplet field x y -2 -2 -1 -1)
                ;; diagonal /: start, middle, end
                (check-triplet field x y  1 -1  2 -2)
                (check-triplet field x y -1  1  1 -1)
                (check-triplet field x y -2  2 -1  1))
            (reduced false)
            true))))
    true changes))

(defn cs-anti-triplet-check [{:keys [field]}]
      (reduce
        (fn [_ idx]
          (let [y (quot idx w)
                x (mod idx w)]
            (if (and
                  (identical? "F" (aget field idx))
                  (or
                    ;; FFF
                    ;; ...
                    ;; ...
                    (and
                      (< (+ x 2) w)
                      (identical? "F" (aget field (+ (+ x 1) (* y w))))
                      (identical? "F" (aget field (+ (+ x 2) (* y w)))))

                    ;; F..
                    ;; F..
                    ;; F..
                    (and
                      (< (+ y 2) h)
                      (identical? "F" (aget field (+ x (* (+ y 1) w))))
                      (identical? "F" (aget field (+ x (* (+ y 2) w)))))

                    ;; F..
                    ;; .F.
                    ;; ..F
                    (and
                      (< (+ x 2) w)
                      (< (+ y 2) h)
                      (identical? "F" (aget field (+ (+ x 1) (* (+ y 1) w))))
                      (identical? "F" (aget field (+ (+ x 2) (* (+ y 2) w)))))

                    ;; ..F
                    ;; .F.
                    ;; F..
                    (and
                      (>= x 2)
                      (< (+ y 2) h)
                      (identical? "F" (aget field (+ (- x 1) (* (+ y 1) w))))
                      (identical? "F" (aget field (+ (- x 2) (* (+ y 2) w)))))))
              (reduced false)
              true)))
        true idxs))

(def constraints
  {:total        {;; :init  cs-total-init
                  :check cs-total-check}
   :vanilla      {;; :init  cs-vanilla-init
                  :check cs-vanilla-check}
   :quad         {:check cs-quad-check}
   :connected    {:check cs-connected-check}
   :anti-triplet {:check cs-anti-triplet-check}})

(defn auto-open [problem]
  (loop [known-idx 0
         problem   problem]
    (cond+
      (>= known-idx (alength known))
      problem

      :let [i       (aget known known-idx)
            {:keys [field]} problem
            value   (parse-long (aget field i))
            nbs     (aget neighbours i)
            unknown (count-chars field nbs ".")]
      ;; nothing to open
      (identical? 0 unknown)
      (recur (inc known-idx) problem)

      :let [fs (count-chars field nbs "F")]

      ;; all flagged, can open the rest
      (identical? fs value)
      (recur 0 (reduce #(if (identical? "." (aget field %2)) (with-ch %1 %2 "?") %1) (clone-problem problem) nbs))

      ;; can flag the rest
      (identical? (- value fs) unknown)
      (recur 0 (reduce #(if (identical? "." (aget field %2)) (with-ch %1 %2 "F") %1) (clone-problem problem) nbs))

      :else
      (recur (inc known-idx) problem))))

(defn auto-finish [problem]
  (let [{:keys [field flagged unknown]} problem]
    (cond+
      ;; can open the rest
      (identical? flagged total-flags)
      (reduce #(if (identical? "." (aget field %2))
                 (with-ch %1 %2 "?")
                 %1)
        (clone-problem problem) idxs)

      ;; can flag the rest
      (identical? (- total-flags flagged) unknown)
      (reduce #(if (identical? "." (aget field %2))
                 (with-ch %1 %2 "F")
                 %1)
        (clone-problem problem) idxs))))

(defn best-candidate [{:keys [field] :as problem}]
  (let [ratings (js/Map.)]
    (doseq [i     known
            :let  [nbs    (aget neighbours i)
                   value  (parse-long (aget field i))
                   flags  (count-chars field nbs "F")
                   rating (- value flags)]
            ni    nbs
            :when (identical? "." (aget field ni))]
      (assoc! ratings ni (min (or (.get ratings ni) ##Inf) rating)))
    (apply min-key #(or (.get ratings %) ##Inf) (filter #(identical? "." (aget field %)) candidates))))

(defn solve-impl [problem]
  ; (println "exploring" (field-str problem))
  (cond+
    ;; short-circuit if doesn't fit already
    (not (every? #((:check %) problem) active-constraints))
    nil

    :do (set! (.-changes problem) [])

    ;; leaf -- all explored -- that fits
    (identical? 0 (.-unknown problem))
    problem

    :let [problem' (auto-open problem)]

    (not= (.-field problem') (.-field problem))
    (do
      ; (println "auto-open" (field-str problem) (field-str problem'))
      (recur problem'))

    :let [problem' (auto-finish problem)]

    problem'
    (do
      ; (println "auto-finish" (field-str problem) (field-str problem'))
      (recur problem'))

    :let [candidate (best-candidate problem)]

    (nil? candidate)
    nil

    :else
    (or
      (solve-impl (with-ch (clone-problem problem) candidate "F"))
      (solve-impl (with-ch (clone-problem problem) candidate "?")))))

(defn solve [field-w field-h field-flags rules input]
  (set! w field-w)
  (set! h field-h)
  (set! total-flags field-flags)
  (set! active-constraints (mapv constraints rules))
  (set! idxs (vec (shuffle (range (* w h)))))
  (set! neighbours
    (or
      (.get neighbours-cache (js/JSON.stringify [w h]))
      (let [arr []]
        (doseq [y (range h)
                x (range w)]
          (aset arr (+ (* y w) x)
            (vec
              (sort
                (for [dx  [-1 0 1]
                      dy  [-1 0 1]
                      :let [x' (+ x dx)
                            y' (+ y dy)]
                      :when (and
                              (not= [x y] [x' y'])
                              (>= x' 0)
                              (>= y' 0)
                              (< x' w)
                              (< y' h))]
                  (+ (* y' w) x'))))))
        arr)))
  (.set neighbours-cache (js/JSON.stringify [w h]) neighbours)
  (let [field    (.split (str/replace input #"\s" "") "")
        flagged  (count (filterv #(identical? "F" (aget field %)) idxs))
        unknown  (count (filterv #(identical? "." (aget field %)) idxs))
        problem  (Problem. field flagged unknown (js/Map.) [])]
    (set! known
      (->> idxs
        (filterv #(<= "0" (aget field %) "8"))))
    (set! candidates
      (filterv #(identical? "." (aget field %)) idxs))
    (doseq [cs active-constraints]
      (when-some [init (:init cs)]
        (init problem)))
    (:field (solve-impl problem))))

(defn test []
  #_(let [problem "..3.....
                 .......3
                 ........
                 .4..3...
                 .....5..
                 ........
                 ..3.....
                 ..2....."
        _ (println "Warming up (CLJS)...")
        t0    (js/performance.now)
        iters 100
        _ (dotimes [_ iters]
            (solve 8 8 26 [:total :vanilla :anti-triplet] problem))
        _ (println "warmup" (-> (- (js/performance.now) t0) (/ iters)) "ms / solve," iters "iters")
        _ (println "Solving (CLJS)...")
        t0        (js/performance.now)
        iters 100
        _ (dotimes [_ iters]
            (solve 8 8 26 [:total :vanilla :anti-triplet] problem))
        _ (println "solve" (-> (- (js/performance.now) t0) (/ iters)) "ms / solve," iters "iters")])

  (doseq [[w h f id problem] [[3 3 4 "[V]3x3-4-test"
                                 ".2.
                                .?.
                                1.F"]
                                [5 5 10 "[V]5x5-10-ZZZZ"
                                 ".....
                                .8...
                                ...20
                                23332
                                001.."]
                                [5 5 10 "[V]5x5-10-10181"
                                 "..2..
                                .3...
                                .3...
                                ...2.
                                ...2."]
                                #_[6 6 14 "[V]6x6-14-10388"
                                   "1.....
                                1.....
                                .....3
                                2.....
                                ...55?
                                ..4.?."]
                                #_[6 6 14 "[V]6x6-14-10740"
                                   "......
                                .6....
                                .5....
                                ......
                                .3....
                                1....0"]
                                #_[7 7 20 "[V]7x7-20-11447"
                                   "?3...1.
                                .......
                                ..4.4..
                                .4..4..
                                .5.....
                                3......
                                ....0.."]
                                #_[8 8 26 "[V]8x8-26-10145"
                                   "..2..3..
                                ........
                                .....2..
                                ..7.....
                                ........
                                .4......
                                .5......
                                ..?.0..."]
                                [8 8 26 "[V]8x8-26-1388D"
                                 "........
                                .....6..
                                2.....4.
                                2.2....1
                                ........
                                ........
                                2?.42...
                                0111?.3."]
                                #_[5 5 10 "[Q]5x5-10-10009"
                                   "..3..
                                .....
                                .3...
                                .4..2
                                1...2"]
                                #_[8 8 26 "[Q]8x8-26-10355"
                                   ".1.2.2..
                                ........
                                ....4.5.
                                ......4.
                                .....5..
                                ........
                                2....3..
                                .......2"]
                                [5 5 10 "[C]5x5-10-test"
                                   "..33.
                                ...4.
                                .....
                                .....
                                .1..."]
                                #_[8 8 26 "[C]8x8-26-10757"
                                   "..1.....
                                .......?
                                ...1....
                                .....4.4
                                ........
                                .....?..
                                ...1....
                                ........"]
                                [5 5 10 "[T]5x5-10-7222"
                                   ".....
                                ....4
                                .....
                                .....
                                1.3.."]
                                #_[8 8 26 "[T]8x8-26-10817"
                                   "..3.....
                                .......3
                                ........
                                .4..3...
                                .....5..
                                ........
                                ..3.....
                                ..2....."]]
            :let [t0        (js/performance.now)
                  problem   (str/replace problem #"\s" "")

                  dangerous []
                  safe      []]]
      (doseq [:let  []
              y     (range h)
              x     (range w)
              :let  [i (+ (* y w) x)]
              :when (identical? "." (nth problem i))]
        (let [problem' (string-with problem i "?")]
          (when-not (solve w h f (core/puzzle-rules id) problem')
            (conj! dangerous i)))
        (let [problem' (string-with problem i "F")]
          (when-not (solve w h f (core/puzzle-rules id) problem')
            (conj! safe i))))

      (let [hinted (as-> problem %
                     (reduce #(string-with %1 %2 "S") % safe)
                     (reduce #(string-with %1 %2 "D") % dangerous))]
        (println id "/" (-> (js/performance.now) (- t0) (* 1000) (js/Math.round) (/ 1000)) "ms" (field-str hinted)))))

#_(test)
