(ns sapper.solver2
  (:require
   [clojure.string :as str]
   [sapper.core :as core])
  (:require-macros
   [sapper.macros :refer [cond+ either]]))

;; Field cell values: 0-8 = open cells with mine count
(def FLAG 9)     ;; F - flagged cell
(def OPEN 10)    ;; ? - confirmed safe cell
(def UNKNOWN 11) ;; . - unknown/UNKNOWN cell
(def SAFE 12)
(def DANGER 13)

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
  (field flag_indices)
  (field states)
  (field changes)
  (constructor [_ a b c d e f]
    (set! field   a)
    (set! flagged b)
    (set! unknown c)
    (set! flag_indices d)
    (set! states  e)
    (set! changes f)))

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
   ;; 9: FLAG, 10: OPEN, 11: UNKNOWN, 12: SAFE, 13: DANGER
   "🚩" "❓" "⬜" "🟩" "🟥"])

(defn cell-str [val]
  (aget emojis val))

(defn field-str [field]
  (str "\n\n" (str/join "\n" (map #(str/join (map cell-str %)) (partition w field)))))

(defn string-with [s i ch]
  (str (.slice s 0 i) ch (.slice s (inc i))))

(defn clone-problem [{:keys [field flagged unknown flag_indices states changes]}]
  (Problem. (js/Uint8Array. field) flagged unknown (.slice flag_indices) (js/Map. states) (.slice changes)))

(defn with-val [problem i val]
  (let [{:keys [field flagged unknown flag_indices changes]} problem]
    (aset field i val)
    (when (identical? FLAG val)
      (set! (.-flagged problem) (inc flagged))
      (.push flag_indices i))
    (set! (.-unknown problem) (dec unknown))
    (.push changes [i val])
    problem))

;; [*] Count amount of mines
(defn cs-total-check [{:keys [flagged unknown]}]
  (and
    ;; did not placed too many flags
    (<= flagged total-flags)
    ;; have enough space to get to total
    (>= (+ flagged unknown) total-flags)))

;; [V] Nothing special
(defn cs-vanilla-check [{:keys [field]}]
  (every?
    (fn [i]
      (let [value   (aget field i)
            nbs     (aget neighbours i)
            fs      (count-val field nbs FLAG)
            unknown (count-val field nbs UNKNOWN)]
        (and
          ;; did not placed too many flags
          (<= fs value)
          ;; have enough space to get to total
          (>= (+ fs unknown) value))))
    known))

;; [Q] There must be at least 1 mine in every 2x2 area
(defn cs-quad-check [{:keys [field]}]
  (loop [y 0
         x 0]
    (cond+
      (>= x (- w 1))
      (recur (inc y) 0)

      (>= y (- h 1))
      true

      (and
        (not (either identical? FLAG UNKNOWN (aget field (+ (* y w) x))))
        (not (either identical? FLAG UNKNOWN (aget field (+ (* y w) (+ x 1)))))
        (not (either identical? FLAG UNKNOWN (aget field (+ (* (+ y 1) w) x))))
        (not (either identical? FLAG UNKNOWN (aget field (+ (* (+ y 1) w) (+ x 1))))))
      false

      :else
      (recur y (inc x)))))

;; [C] All mines are orthogonally or diagonally connected
(defn cs-connected-check [{:keys [field]}]
  (let [flag-indices (filterv #(either identical? FLAG UNKNOWN (aget field %)) idxs)]
    ;; DFS through FLAG or UNKNOWN cells
    (let [start   (first flag-indices)
          visited (js/Set. [start])
          queue   [start]]
      (loop []
        (when-some [current (.pop queue)]
          (doseq [nb    (aget neighbours current)
                  :when (not (.has visited nb))
                  :let  [val (aget field nb)]
                  :when (either identical? FLAG UNKNOWN val)]
            (conj! visited nb)
            (conj! queue nb))
          (recur)))
      (every? #(.has visited %) flag-indices))))

;; [T] Flags may not form row of three orthogonally or diagonally
(defn cs-anti-triplet-check [{:keys [field flag_indices]}]
  (loop [i 0]
    (cond+
      (>= i (alength flag_indices))
      true

      :let [idx (aget flag_indices i)
            y   (quot idx w)
            x   (mod idx w)]

      ;; FFF
      ;; ...
      ;; ...
      (and
        (< (+ x 2) w)
        (identical? FLAG (aget field (+ (+ x 1) (* y w))))
        (identical? FLAG (aget field (+ (+ x 2) (* y w)))))
      false

      ;; F..
      ;; F..
      ;; F..
      (and
        (< (+ y 2) h)
        (identical? FLAG (aget field (+ x (* (+ y 1) w))))
        (identical? FLAG (aget field (+ x (* (+ y 2) w)))))
      false

      ;; F..
      ;; .F.
      ;; ..F
      (and
        (< (+ x 2) w)
        (< (+ y 2) h)
        (identical? FLAG (aget field (+ (+ x 1) (* (+ y 1) w))))
        (identical? FLAG (aget field (+ (+ x 2) (* (+ y 2) w)))))
      false

      ;; ..F
      ;; .F.
      ;; F..
      (and
        (>= x 2)
        (< (+ y 2) h)
        (identical? FLAG (aget field (+ (- x 1) (* (+ y 1) w))))
        (identical? FLAG (aget field (+ (- x 2) (* (+ y 2) w)))))
      false

      :else
      (recur (inc i)))))

(def constraints
  {:total        {:check cs-total-check}
   :vanilla      {:check cs-vanilla-check}
   :quad         {:check cs-quad-check}
   :connected    {:check cs-connected-check}
   :anti-triplet {; :init  cs-anti-triplet-init-incr
                  :check cs-anti-triplet-check}})

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
            unknown (count-val field nbs UNKNOWN)]
      ;; nothing to open
      (identical? 0 unknown)
      (recur (inc known-idx) problem)

      :let [fs (count-val field nbs FLAG)]

      ;; all flagged, can open the rest
      (identical? fs value)
      (recur 0 (reduce #(if (identical? UNKNOWN (aget field %2)) (with-val %1 %2 OPEN) %1) (clone-problem problem) nbs))

      ;; can flag the rest
      (identical? (- value fs) unknown)
      (recur 0 (reduce #(if (identical? UNKNOWN (aget field %2)) (with-val %1 %2 FLAG) %1) (clone-problem problem) nbs))

      :else
      (recur (inc known-idx) problem))))

(defn auto-finish [problem]
  (let [{:keys [field flagged unknown]} problem]
    (cond+
      ;; can open the rest
      (identical? flagged total-flags)
      (reduce #(if (identical? UNKNOWN (aget field %2))
                 (with-val %1 %2 OPEN)
                 %1)
        (clone-problem problem) idxs)

      ;; can flag the rest
      (identical? (- total-flags flagged) unknown)
      (reduce #(if (identical? UNKNOWN (aget field %2))
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
        (when-some [ni (core/find #(identical? UNKNOWN (aget field %)) nbs)]
          (set! min-rating rating)
          (set! min-index ni))))
    (or
      min-index
      (core/find #(identical? UNKNOWN (aget field %)) candidates))))

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
      (solve-impl (with-val (clone-problem problem) candidate OPEN)))))

(defn parse-cell [ch]
  (case ch
    "F" FLAG
    "?" OPEN
    "." UNKNOWN
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
  (let [chars        (.split (str/replace input #"\s" "") "")
        field        (js/Uint8Array. (map parse-cell chars))
        flagged      (count (filterv #(identical? FLAG (aget field %)) idxs))
        unknown      (count (filterv #(identical? UNKNOWN (aget field %)) idxs))
        flag_indices (filterv #(identical? FLAG (aget field %)) idxs)
        problem      (Problem. field flagged unknown flag_indices (js/Map.) [])]
    (set! known
      (->> idxs
        (filterv #(<= 0 (aget field %) 8))))
    (set! candidates
      (filterv #(identical? UNKNOWN (aget field %)) idxs))
    (when (every? identity
            (for [cs active-constraints
                  :when (:init cs)]
              ((:init cs) problem)))
      (:field (solve-impl problem)))))

(defn test []
  (doseq [[w h f id problem] [#_[3 3 4 "[V]3x3-4-test"
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
                              [8 8 26 "[Q]8x8-26-10355"
                               ".1.2.2..
                                ........
                                ....4.5.
                                ......4.
                                .....5..
                                ........
                                2....3..
                                .......2"]
                              #_[5 5 10 "[C]5x5-10-test"
                                 "..33.
                                ...4.
                                .....
                                .....
                                .1..."]
                              [8 8 26 "[C]8x8-26-10757"
                               "..1.....
                                .......?
                                ...1....
                                .....4.4
                                ........
                                .....?..
                                ...1....
                                ........"]
                              [8 8 26 "[C]8x8-26-12801"
                               "..1.....
                                .3.2....
                                .3......
                                ......3.
                                ........
                                ...4..3.
                                ........
                                .....2.."]
                              #_[5 5 10 "[T]5x5-10-7222"
                                 ".....
                                ....4
                                .....
                                .....
                                1.3.."]
                              [8 8 26 "[T]8x8-26-10817"
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
                       (identical? "?" ch)     OPEN
                       (contains? safe i)      SAFE
                       (contains? dangerous i) DANGER
                       (identical? "." ch)     UNKNOWN))
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
           [8 8 26 "[C]8x8-26-12801"
            "........
             .3.2....
             .3......
             ......3.
             ........
             ...4..3.
             ........
             .....2.."]
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
          iters 100
          _     (dotimes [_ iters]
                  (solve w h f rules problem))
          _     (println "   Solve" (-> (- (js/performance.now) t0) (/ iters)) "ms / solve," iters "iters")])))

#_(test)
#_(bench)
