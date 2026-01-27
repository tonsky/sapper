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
(def candidates)

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

(defn field-str [problem]
  (str "\n\n" (str/join "\n" (map #(str/join %) (partition w (.-field problem))))))

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
  [{:name "total flags"
    :check
    (fn total-flags-check [{:keys [flagged unknown]}]
      (and
        ;; did not placed too many flags
        (<= flagged flags)
        ;; have enough space to get to total
        (>= (+ flagged (count unknown)) flags)))}

   {:name "vanilla"
    :check
    (fn vanilla-check [{:keys [field]}]
      (every?
        (fn [i]
          (let [value   (parse-long (aget field i))
                nbs     (get neighbours i)
                fs      (count-chars field nbs "F")
                unknown (count-chars field nbs ".")]
            (and
              ;; did not placed too many flags
              (<= fs value)
              ;; have enough space to get to total
              (>= (+ fs unknown) value))))
        known))}])

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
      (reduce #(with-ch %1 %2 "F") (clone-problem problem) unknown))))

(defn best-candidate [{:keys [field] :as problem}]
  (let [ratings (js/Map.)]
    (doseq [i     known
            :let  [nbs    (get neighbours i)
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
    (not (every? #((:check %) problem) constraints))
    nil

    ;; leaf -- all explored -- that fits
    (identical? 0 (count (.-unknown problem)))
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

(defn solve [field-w field-h total-flags input]
  (set! w field-w)
  (set! h field-h)
  (set! flags total-flags)
  (set! idxs (vec (shuffle (range (* w h)))))
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
    (set! candidates
      (filterv #(identical? "." (aget field %)) idxs))
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
      (println (str
                 (-> (js/performance.now) (- t0) (* 1000) (js/Math.round) (/ 1000)) " ms"
                 " / "
                 (count (filter #(identical? "F" %) solution)) " mines"
                 (if solution
                   (field-str {:field solution})
                   "\n\nno solution"))))))

#_(test)
