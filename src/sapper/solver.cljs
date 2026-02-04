(ns sapper.solver2
  (:require
   [clojure.string :as str]
   [sapper.core :as core])
  (:require-macros
   [sapper.macros :refer [cond+ either]]))

;; Field cell values: 0-8 = open cells with mine count
(def FLAG 9)    ;; F - flagged cell
(def SECRET 10) ;; ? - confirmed safe cell
(def CLOSED 11) ;; . - unknown/closed cell

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
  #_(field states)
  #_(field changes)
  (constructor [_ a b c d e]
    (set! field   a)
    (set! flagged b)
    (set! unknown c)
    #_(set! states  d)
    #_(set! changes e)))

(defn count-val [arr indices val]
  (loop [res 0
         idx 0]
    (cond+
      (>= idx (alength indices))
      res

      (identical? val (aget arr (aget indices idx)))
      (recur (inc res) (inc idx))

      :else
      (recur res (inc idx)))))

(def emojis
  [;; 0-8: open cells
   "0️⃣" "1️⃣" "2️⃣" "3️⃣" "4️⃣" "5️⃣" "6️⃣" "7️⃣" "8️⃣"
   ;; 9: FLAG, 10: SECRET, 11: CLOSED, 12: SAFE, 13: DANGER
   "🚩" "❓" "⬜" "🟩" "🟥"])

(defn cell-str [val]
  (aget emojis val))

(defn field-str [field]
  (str "\n\n" (str/join "\n" (map #(str/join (map cell-str %)) (partition w field)))))

(defn string-with [s i ch]
  (str (.slice s 0 i) ch (.slice s (inc i))))

(defn clone-problem [{:keys [field flagged unknown states changes]}]
  (Problem. (js/Uint8Array. field) flagged unknown #_(js/Map. states) #_(.slice changes)))

(defn with-val [problem i val]
  (let [{:keys [field flagged unknown changes]} problem]
    (aset field i val)
    (when (identical? FLAG val)
      (set! (.-flagged problem) (inc flagged)))
    (set! (.-unknown problem) (dec unknown))
    #_(.push changes [i val])
    problem))

;; [*] Count amount of mines
(defn cs-total-init [{:keys [field states]}]
  (assoc! states :total
    {:flagged (count-val field idxs FLAG)
     :unknown (count-val field idxs CLOSED)}))

(defn cs-total-check-incr [{:keys [field states changes]}]
  (let [[flagged unknown]
        (loop [flagged    (:flagged (:total states))
               unknown    (:unknown (:total states))
               change-idx 0]
          (if (>= change-idx (alength changes))
            [flagged unknown]
            (let [[_idx val] (nth changes change-idx)]
              (cond
                (identical? FLAG val)   (recur (inc flagged) (dec unknown) (inc change-idx))
                (identical? SECRET val) (recur      flagged  (dec unknown) (inc change-idx))
                :else                   (throw (js/Error. (str "Unepxected change" _idx val)))))))]
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
      (assoc! vanilla-flags i (count-val field nbs FLAG))
      (assoc! vanilla-unknown i (count-val field nbs CLOSED)))
    ;; For each CLOSED, find numbered cells surrounding it
    (doseq [i candidates
            :let [nbs (aget neighbours i)]]
      (assoc! vanilla-neighbours i (filterv #(<= 0 (aget field %) 8) nbs)))
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
        (for [[idx val] changes
              num-idx   (or (.get vanilla-neighbours idx) [])
              :let [value    (aget field num-idx)
                    flags    (.get vanilla-flags num-idx)
                    unknown  (.get vanilla-unknown num-idx)
                    flags'   (cond-> flags (identical? FLAG val) inc)
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
      (let [value   (aget field i)
            nbs     (aget neighbours i)
            fs      (count-val field nbs FLAG)
            unknown (count-val field nbs CLOSED)]
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
        (either identical? FLAG CLOSED (aget field (+ (* y w) x)))
        (either identical? FLAG CLOSED (aget field (+ (* y w) (+ x 1))))
        (either identical? FLAG CLOSED (aget field (+ (* (+ y 1) w) x)))
        (either identical? FLAG CLOSED (aget field (+ (* (+ y 1) w) (+ x 1))))))
    (for [x (range (dec w))
          y (range (dec h))]
      [x y])))

;; [C] All mines are orthogonally or diagonally connected
(defn cs-connected-check [{:keys [field]}]
  (let [flag-indices (filterv #(either identical? FLAG CLOSED (aget field %)) idxs)]
    ;; DFS through FLAG or CLOSED cells
    (let [start   (first flag-indices)
          visited (js/Set. [start])
          queue   [start]]
      (loop []
        (when-some [current (.pop queue)]
          (doseq [nb    (aget neighbours current)
                  :when (not (.has visited nb))
                  :let  [val (aget field nb)]
                  :when (either identical? FLAG CLOSED val)]
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
      (identical? FLAG (aget field (+ x1 (* y1 w))))
      (identical? FLAG (aget field (+ x2 (* y2 w)))))))

(defn cs-anti-triplet-check-incr [{:keys [field changes]}]
  (reduce
    (fn [_ [idx val]]
      (if (identical? SECRET val)
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
  (loop [i 0]
    (cond+
      (>= i (alength idxs))
      true

      :let [idx (aget idxs i)
            y   (quot idx w)
            x   (mod idx w)]

      (and
        (identical? FLAG (aget field idx))
        (or
          ;; FFF
          ;; ...
          ;; ...
          (and
            (< (+ x 2) w)
            (identical? FLAG (aget field (+ (+ x 1) (* y w))))
            (identical? FLAG (aget field (+ (+ x 2) (* y w)))))

          ;; F..
          ;; F..
          ;; F..
          (and
            (< (+ y 2) h)
            (identical? FLAG (aget field (+ x (* (+ y 1) w))))
            (identical? FLAG (aget field (+ x (* (+ y 2) w)))))

          ;; F..
          ;; .F.
          ;; ..F
          (and
            (< (+ x 2) w)
            (< (+ y 2) h)
            (identical? FLAG (aget field (+ (+ x 1) (* (+ y 1) w))))
            (identical? FLAG (aget field (+ (+ x 2) (* (+ y 2) w)))))

          ;; ..F
          ;; .F.
          ;; F..
          (and
            (>= x 2)
            (< (+ y 2) h)
            (identical? FLAG (aget field (+ (- x 1) (* (+ y 1) w))))
            (identical? FLAG (aget field (+ (- x 2) (* (+ y 2) w)))))))
      false

      :else
      (recur (inc i)))))

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
            value   (aget field i)
            nbs     (aget neighbours i)
            unknown (count-val field nbs CLOSED)]
      ;; nothing to open
      (identical? 0 unknown)
      (recur (inc known-idx) problem)

      :let [fs (count-val field nbs FLAG)]

      ;; all flagged, can open the rest
      (identical? fs value)
      (recur 0 (reduce #(if (identical? CLOSED (aget field %2)) (with-val %1 %2 SECRET) %1) (clone-problem problem) nbs))

      ;; can flag the rest
      (identical? (- value fs) unknown)
      (recur 0 (reduce #(if (identical? CLOSED (aget field %2)) (with-val %1 %2 FLAG) %1) (clone-problem problem) nbs))

      :else
      (recur (inc known-idx) problem))))

(defn auto-finish [problem]
  (let [{:keys [field flagged unknown]} problem]
    (cond+
      ;; can open the rest
      (identical? flagged total-flags)
      (reduce #(if (identical? CLOSED (aget field %2))
                 (with-val %1 %2 SECRET)
                 %1)
        (clone-problem problem) idxs)

      ;; can flag the rest
      (identical? (- total-flags flagged) unknown)
      (reduce #(if (identical? CLOSED (aget field %2))
                 (with-val %1 %2 FLAG)
                 %1)
        (clone-problem problem) idxs))))

(defn best-candidate [{:keys [field] :as problem}]
  (loop [min-rating ##Inf
         min-index  nil]
    (doseq [i     known
            :let  [nbs    (aget neighbours i)
                   value  (aget field i)
                   flags  (count-val field nbs FLAG)
                   rating (- value flags)]]
      (when (< rating min-rating)
        (when-some [ni (core/find #(identical? CLOSED (aget field %)) nbs)]
          (set! min-rating rating)
          (set! min-index ni))))
    (or
      min-index
      (core/find #(identical? CLOSED (aget field %)) candidates))))

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
      (solve-impl (with-val (clone-problem problem) candidate FLAG))
      (solve-impl (with-val (clone-problem problem) candidate SECRET)))))

(defn parse-cell [ch]
  (case ch
    "F" FLAG
    "?" SECRET
    "." CLOSED
    (parse-long ch)))

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
  (let [chars    (.split (str/replace input #"\s" "") "")
        field    (js/Uint8Array. (map parse-cell chars))
        flagged  (count (filterv #(identical? FLAG (aget field %)) idxs))
        unknown  (count (filterv #(identical? CLOSED (aget field %)) idxs))
        problem  (Problem. field flagged unknown (js/Map.) [])]
    (set! known
      (->> idxs
        (filterv #(<= 0 (aget field %) 8))))
    (set! candidates
      (filterv #(identical? CLOSED (aget field %)) idxs))
    (doseq [cs active-constraints]
      (when-some [init (:init cs)]
        (init problem)))
    (:field (solve-impl problem))))

(defn test []
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
                dangerous #{}
                safe      #{}]]
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

    (let [hinted (mapv
                   (fn [ch i]
                     (cond
                       (<= "0" ch "8")         (parse-long ch)
                       (identical? "F" ch)     FLAG
                       (identical? "?" ch)     SECRET
                       (contains? safe i)      12
                       (contains? dangerous i) 13
                       (identical? "." ch)     CLOSED))
                   problem
                   (range))]
      (println id "/" (-> (js/performance.now) (- t0) (* 1000) (js/Math.round) (/ 1000)) "ms" (field-str hinted)))))

(defn bench []
  (doseq [[w h f id problem]
          [[8 8 26 "[V]8x8-26-1388D"
            "........
             .....6..
             2.....4.
             2.2....1
             ........
             ........
             2?.42...
             0111?.3."]
           [8 8 26 "[Q]8x8-26-10355"
            ".1.2.2..
             ........
             ....4.5.
             ......4.
             .....5..
             ........
             2....3..
             .......2"]
           [8 8 26 "[C]8x8-26-10757"
            "..1.....
             .......?
             ...1....
             .....4.4
             ........
             .....?..
             ...1....
             ........"]
           [8 8 26 "[T]8x8-26-10817"
            "..3.....
             .......3
             ........
             .4..3...
             .....5..
             ........
             ..3.....
             ..2....."]]]
    (let [rules (core/puzzle-rules id)
          _     (println "Benching" id "...")
          t0    (js/performance.now)
          iters 100
          _     (dotimes [_ iters]
                  (solve w h f rules problem))
          _     (println "  Warmup" (-> (- (js/performance.now) t0) (/ iters)) "ms / solve," iters "iters")
          t0        (js/performance.now)
          iters 1000
          _     (dotimes [_ iters]
                  (solve w h f rules problem))
          _     (println "   Solve" (-> (- (js/performance.now) t0) (/ iters)) "ms / solve," iters "iters")])))

#_(test)
#_(bench)
