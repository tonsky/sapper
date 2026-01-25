(ns sapper.solver2
  (:require
   [clojure.string :as str])
  (:require-macros
   [sapper.macros :refer [cond+]]))

(def w)
(def h)
(def flags)
(def idxs)
(def neighbours)
(def known)
(def constrained)
(def t0 (js/performance.now))

(defclass Problem
  (field field)
  (field flagged)
  (field unknown)
  (constructor [_ a b c]
    (set! field   a)
    (set! flagged b)
    (set! unknown c)))

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

(defn clone-problem [{:keys [field flagged unknown]}]
  (Problem. (.slice field) flagged (js/Set. unknown)))

(defn with-ch [problem i ch]
  (let [{:keys [field flagged unknown]} problem]
    (aset field i ch)
    (.delete unknown i)
    (when (identical? "F" ch)
      (set! (.-flagged problem) (inc flagged)))
    problem))

(def constraints
  [
   ;; total flags
   (fn [{:keys [flagged unknown]}]
     (and
       ;; did not placed too many flags
       (<= flagged flags)
       ;; have enough space to get to total
       (>= (+ flagged (count unknown)) flags)))

   ;; vanilla rules
   (fn [{:keys [field]}]
     (every?
       (fn [i]
         (let [value (parse-long (aget field i))
               nbs   (get neighbours i)
               fs    (count-chars field nbs "F")]
           (and
             ;; did not placed too many flags
             (<= fs value)
             ;; have enough space to get to total
             (let [unknown (count-chars field nbs ".")]
               (>= (+ fs unknown) value)))))
       known))
   ])

(defn auto-open [problem]
  (loop [known-idx 0
         problem   problem]
    (cond+
      (>= known-idx (alength known))
      problem

      :let [i       (aget known known-idx)
            {:keys [field]} problem
            value   (parse-long (aget field i))
            nbs     (get neighbours i)
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
      (identical? flagged flags)
      (reduce #(with-ch %1 %2 "?") (clone-problem problem) unknown)

      ;; can flag the rest
      (identical? (- flags flagged) (count unknown))
      (reduce #(with-ch %1 %2 "F") (clone-problem problem) unknown)

      ;; all open cells are unconstrained
      (every? #(not (identical? "." (aget field %))) constrained)
      (->> idxs
        (filter #(identical? "." (aget field %)))
        (take (- flags flagged))
        (reduce #(with-ch %1 %2 "F") (clone-problem problem))))))

(defn solve-impl [problem]
  (cond+
    ;; short-circuit if doesn't fit already
    (not (every? #(% problem) constraints))
    nil

    ;; leaf -- all explored -- that fits
    (identical? 0 (count (.-unknown problem)))
    problem

    :let [; _ (println (str (- (js/performance.now) t0) " ms exploring\n" (str/join "\n" (map #(str/join %) (partition w (.-field problem))))))
          problem' (auto-open problem)]

    (not= (.-field problem') (.-field problem))
    (recur problem')

    :let [problem' (auto-finish problem)]

    problem'
    (recur problem')

    :let [candidates (vec
                       (for [i     constrained
                             :when (identical? "." (aget (.-field problem) i))]
                         [i (count-chars (.-field problem) (get neighbours i) ".")]))
          _          (.sort candidates (fn [[_ a] [_ b]] (- a b)))]

    :else
    (reduce
      (fn [_ [i _]]
        (when-some [solution (or
                               (solve-impl (with-ch (clone-problem problem) i "F"))
                               (solve-impl (with-ch (clone-problem problem) i "?")))]
          (reduced solution)))
      nil candidates)))

(defn solve [field-w field-h total-flags input]
  (set! w field-w)
  (set! h field-h)
  (set! flags total-flags)
  (set! idxs (vec (range (* w h))))
  (set! neighbours
    (into {}
      (for [x (range w)
            y (range h)]
        [(+ (* y w) x)
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
               (+ (* y' w) x'))))])))
  (let [field    (.split (str/replace input #"\s" "") "")
        flagged  (count (filterv #(identical? "F" (aget field %)) idxs))
        unknown  (->> idxs
                   (filter #(identical? "." (aget field %)))
                   (into #{}))
        problem  (Problem. field flagged unknown)]
    (set! known
      (->> idxs
        (filterv #(<= "0" (aget field %) "8"))))
    (set! constrained
      (->> known
        (mapcat #(get neighbours %))
        (filter #(identical? "." (aget field %)))
        (distinct)
        (vec)))
    (:field (solve-impl problem))))

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
                            [6 6 14 "[V]6x6-14-10388"
                             "1.....
                              1.....
                              .....3
                              2.....
                              ...55?
                              ..4.?."]
                            [6 6 14 "[V]6x6-14-10740"
                             "......
                              .6....
                              .5....
                              ......
                              .3....
                              1....0"]
                            [7 7 20 "[V]7x7-20-11447"
                             "?3...1.
                              .......
                              ..4.4..
                              .4..4..
                              .5.....
                              3......
                              ....0.."]
                            [8 8 26 "[V]8x8-26-10145"
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
                              ........
                              0.1...3."]]]
  (let [t0       (js/performance.now)
        solution (solve w h f problem)]
    (println (str id " / " (-> (js/performance.now) (- t0) (* 1000) (js/Math.round) (/ 1000)) " ms / " (count (filter #(identical? "F" %) solution)) " mines\n"
               (if solution
                 (str/join "\n" (map #(str/join %) (partition w solution)))
                 "no solution")))))
