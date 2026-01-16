(ns sapper.solver
  (:require
   [clojure.string :as str]))

(defn key [x y]
  (str x "," y))

(defn parse-key [k]
  (let [[x y] (str/split k ",")]
    [(js/Number x) (js/Number y)]))

(defn neighbours
  "Returns all valid neighbor coordinates for a cell"
  [x y field-w field-h]
  (let [result #js []]
    (doseq [dx [-1 0 1]
            dy [-1 0 1]
            :when (not (and (= dx 0) (= dy 0)))
            :let [nx (+ x dx)
                  ny (+ y dy)]
            :when (and (>= nx 0) (< nx field-w)
                       (>= ny 0) (< ny field-h))]
      (conj! result [nx ny]))
    result))

(defn cell-is-known-mine? [cell]
  (:flagged cell))

(defn cell-is-unknown? [cell]
  (and (not (:open cell))
       (not (:flagged cell))))

(defn get-numeric-label [cell]
  (when (and (:open cell) (:label cell))
    (let [label (:label cell)]
      (when (re-matches #"\d+" label)
        (parse-long label)))))

(defn validate-initial-state
  "Check that initial state is not self-contradictory.
   Returns true if valid, false if contradictory."
  [field field-w field-h mine-count]
  (let [;; Check total flagged count doesn't exceed mine-count
        total-flagged (count (filter (fn [[_ v]] (cell-is-known-mine? v)) field))]
    (when (<= total-flagged mine-count)
      ;; Check each numbered cell's constraints
      (let [valid (atom true)]
        (doseq [[k cell] field
                :while @valid
                :let [required (get-numeric-label cell)]
                :when required
                :let [[x y] (parse-key k)
                      nbs (neighbours x y field-w field-h)
                      flagged-count (count (filter #(cell-is-known-mine?
                                                      (get field (key (first %) (second %))))
                                                   nbs))
                      unknown-count (count (filter #(cell-is-unknown?
                                                      (get field (key (first %) (second %))))
                                                   nbs))
                      potential-mines (+ flagged-count unknown-count)]]
          #_(println "x" x "y" y "flagged-count" flagged-count "unknown-count" unknown-count "potential-mines" potential-mines "required" required)
          (cond
            ;; Too many flags already (label < flagged neighbors)
            (> flagged-count required)
            (reset! valid false)

            ;; Not enough potential mine locations
            (< potential-mines required)
            (reset! valid false)))
        @valid))))

;; ============================================================================
;; Optimized solver using mutable state
;; ============================================================================

(def ^:const UNKNOWN 0)
(def ^:const MINE 1)
(def ^:const SAFE 2)

(defn build-solver-state
  "Build initial solver state with indexed constraints.
   Uses arrays for fast access and mutation."
  [field field-w field-h constrained-unknowns-set]
  (let [;; Map cell keys to indices for array access
        cell-keys (vec constrained-unknowns-set)
        key->idx (into {} (map-indexed (fn [i k] [k i]) cell-keys))
        num-cells (count cell-keys)

        ;; Cell states array: 0=unknown, 1=mine, 2=safe
        cell-states (js/Int8Array. num-cells)

        ;; Build constraints
        constraints #js []
        ;; cell->constraints: for each cell index, list of constraint indices
        cell->constraints (let [arr []]
                            (dotimes [_i num-cells]
                              (conj! arr []))
                            arr)]

    ;; Build constraints from numbered cells
    (doseq [[k cell] field
            :let [required (get-numeric-label cell)]
            :when required
            :let [[x y] (parse-key k)
                  nbs (neighbours x y field-w field-h)
                  known-mines (count (filter #(cell-is-known-mine?
                                                (get field (key (first %) (second %))))
                                             nbs))
                  unknown-indices (into []
                                        (comp
                                          (map (fn [[nx ny]] (key nx ny)))
                                          (filter #(contains? key->idx %))
                                          (map key->idx))
                                        nbs)]
            :when (seq unknown-indices)]
      (let [constraint-idx (count constraints)
            ;; Store unknown indices as Int16Array for fast iteration
            unknowns-arr (js/Int16Array. (count unknown-indices))
            _ (dotimes [i (count unknown-indices)]
                (aset unknowns-arr i (nth unknown-indices i)))
            ;; Track which cells are still unknown in this constraint
            unknowns-set (js/Set.)]
        (doseq [idx unknown-indices]
          (.add unknowns-set idx)
          (conj! (aget cell->constraints idx) constraint-idx))
        (conj! constraints #js {:required required
                                :known-mines known-mines
                                :all-cells unknowns-arr
                                :unknowns unknowns-set})))

    #js {:cell-keys cell-keys
         :key->idx key->idx
         :cell-states cell-states
         :constraints constraints
         :cell->constraints cell->constraints
         :unknown-count num-cells
         :mine-count 0}))

(defn clone-state
  "Clone solver state for backtracking"
  [state]
  (let [old-states (aget state "cell-states")
        old-constraints (aget state "constraints")
        new-states (js/Int8Array. (.-length old-states))
        new-constraints #js []]
    ;; Copy cell states
    (.set new-states old-states)
    ;; Clone constraints (only the mutable parts)
    (dotimes [i (count old-constraints)]
      (let [c (aget old-constraints i)
            old-unknowns (aget c "unknowns")
            new-unknowns (js/Set. old-unknowns)]
        (conj! new-constraints #js {:required (aget c "required")
                                    :known-mines (aget c "known-mines")
                                    :all-cells (aget c "all-cells")
                                    :unknowns new-unknowns})))
    #js {:cell-keys (aget state "cell-keys")
         :key->idx (aget state "key->idx")
         :cell-states new-states
         :constraints new-constraints
         :cell->constraints (aget state "cell->constraints")
         :unknown-count (aget state "unknown-count")
         :mine-count (aget state "mine-count")}))

(defn assign-and-propagate!
  "Assign cells and propagate constraints. Returns false if invalid."
  [state initial-queue]
  (let [cell-states (aget state "cell-states")
        constraints (aget state "constraints")
        cell->constraints (aget state "cell->constraints")
        queue initial-queue]

    (loop []
      (if (zero? (count queue))
        true
        (let [item (.shift queue)
              cell-idx (aget item 0)
              is-mine (aget item 1)]

          ;; Skip if already assigned
          (if (not= UNKNOWN (aget cell-states cell-idx))
            (if (= (if is-mine MINE SAFE) (aget cell-states cell-idx))
              (recur) ;; Same assignment, continue
              false)  ;; Conflict!

            (do
              ;; Make assignment
              (aset cell-states cell-idx (if is-mine MINE SAFE))
              (aset state "unknown-count" (dec (aget state "unknown-count")))
              (when is-mine
                (aset state "mine-count" (inc (aget state "mine-count"))))

              ;; Update all constraints involving this cell
              (let [constraint-indices (aget cell->constraints cell-idx)
                    valid (loop [ci 0]
                            (if (>= ci (count constraint-indices))
                              true
                              (let [c-idx (aget constraint-indices ci)
                                    c (aget constraints c-idx)
                                    unknowns (aget c "unknowns")]

                                (when (.has unknowns cell-idx)
                                  (.delete unknowns cell-idx)
                                  (when is-mine
                                    (aset c "known-mines" (inc (aget c "known-mines")))))

                                (let [remaining-needed (- (aget c "required") (aget c "known-mines"))
                                      unknown-count (.-size unknowns)]
                                  (cond
                                    ;; Too many mines
                                    (neg? remaining-needed)
                                    false

                                    ;; Not enough cells for required mines
                                    (> remaining-needed unknown-count)
                                    false

                                    ;; All remaining must be mines
                                    (and (pos? unknown-count) (= remaining-needed unknown-count))
                                    (do
                                      (.forEach unknowns
                                                (fn [idx]
                                                  (conj! queue #js [idx true])))
                                      (recur (inc ci)))

                                    ;; All remaining must be safe
                                    (and (pos? unknown-count) (= remaining-needed 0))
                                    (do
                                      (.forEach unknowns
                                                (fn [idx]
                                                  (conj! queue #js [idx false])))
                                      (recur (inc ci)))

                                    :else
                                    (recur (inc ci)))))))]
                (if valid
                  (recur)
                  false)))))))))

(defn find-best-cell
  "Find best cell using MRV heuristic - cell in smallest constraint"
  [state]
  (let [cell-states (aget state "cell-states")
        constraints (aget state "constraints")
        num-constraints (count constraints)]
    (loop [ci 0
           best-cell -1
           best-size js/Infinity]
      (if (>= ci num-constraints)
        (if (= best-cell -1)
          ;; Fallback: find any unknown cell
          (let [n (.-length cell-states)]
            (loop [i 0]
              (if (>= i n)
                -1
                (if (= UNKNOWN (aget cell-states i))
                  i
                  (recur (inc i))))))
          best-cell)
        (let [c (aget constraints ci)
              unknowns (aget c "unknowns")
              size (.-size unknowns)]
          (if (and (pos? size) (< size best-size))
            (let [;; Get first element from Set
                  first-cell (do
                               (let [result (atom -1)]
                                 (.forEach unknowns
                                           (fn [idx]
                                             (when (= @result -1)
                                               (reset! result idx))))
                                 @result))]
              (recur (inc ci) first-cell size))
            (recur (inc ci) best-cell best-size)))))))

(defn solve-recursive
  "Recursive backtracking solver. Returns true if solution found."
  [state min-mines max-mines]
  (let [mine-count (aget state "mine-count")
        unknown-count (aget state "unknown-count")]
    (cond
      ;; Too many mines
      (> mine-count max-mines)
      false

      ;; Can't reach minimum
      (> min-mines (+ mine-count unknown-count))
      false

      ;; All assigned - check if valid
      (zero? unknown-count)
      (and (>= mine-count min-mines) (<= mine-count max-mines))

      ;; Branch
      :else
      (let [cell-idx (find-best-cell state)]
        (if (neg? cell-idx)
          false
          ;; Try mine first
          (or
            (when (< mine-count max-mines)
              (let [state' (clone-state state)
                    queue #js [#js [cell-idx true]]]
                (when (assign-and-propagate! state' queue)
                  (when (solve-recursive state' min-mines max-mines)
                    ;; Copy solution back
                    (.set (aget state "cell-states") (aget state' "cell-states"))
                    (aset state "mine-count" (aget state' "mine-count"))
                    (aset state "unknown-count" (aget state' "unknown-count"))
                    true))))

            ;; Try safe
            (let [state' (clone-state state)
                  queue #js [#js [cell-idx false]]]
              (when (assign-and-propagate! state' queue)
                (when (solve-recursive state' min-mines max-mines)
                  (.set (aget state "cell-states") (aget state' "cell-states"))
                  (aset state "mine-count" (aget state' "mine-count"))
                  (aset state "unknown-count" (aget state' "unknown-count"))
                  true)))))))))

(defn solve
  "Find a valid mine placement for a Minesweeper puzzle.

   Args:
     field-w: width of the field
     field-h: height of the field
     mine-count: total number of mines
     field: map from 'x,y' strings to cell maps
            - {:open true :label \"N\"} - open cell with N adjacent mines (creates constraint)
            - {:open true :label \"q\"} - open cell, not a mine, unknown neighbor count (no constraint)
            - {:flagged true} - must be a mine
            - {} or nil - unknown cell (potential mine location)

   Returns:
     A field map with {:mine true} for cells with mines, or nil if no solution."
  [field-w field-h mine-count field]
  #_(println field)
  ;; Early validation: check for contradictions in initial state
  (when (validate-initial-state field field-w field-h mine-count)
    (let [;; Collect known mines (flagged cells)
        known-mines (into #{}
                          (comp
                            (filter (fn [[k v]] (cell-is-known-mine? v)))
                            (map first))
                          field)
        known-mine-count (count known-mines)

        ;; Get all unknown cells in the field
        all-field-unknowns (into #{}
                                 (for [x (range field-w)
                                       y (range field-h)
                                       :let [k (key x y)
                                             cell (get field k)]
                                       :when (cell-is-unknown? cell)]
                                   k))

        ;; Collect constrained unknowns (cells adjacent to numbered cells)
        constrained-unknowns (into #{}
                                   (for [[k cell] field
                                         :let [required (get-numeric-label cell)]
                                         :when required
                                         :let [[x y] (parse-key k)
                                               nbs (neighbours x y field-w field-h)]
                                         [nx ny] nbs
                                         :let [nk (key nx ny)]
                                         :when (contains? all-field-unknowns nk)]
                                     nk))

        unconstrained-unknowns (remove constrained-unknowns all-field-unknowns)
        unconstrained-count (count (vec unconstrained-unknowns))]

    (when (<= known-mine-count mine-count)
      (let [mines-to-place (- mine-count known-mine-count)
            constrained-count (count constrained-unknowns)
            min-constrained-mines (max 0 (- mines-to-place unconstrained-count))
            max-constrained-mines (min mines-to-place constrained-count)]

        (when (<= min-constrained-mines max-constrained-mines)
          (if (empty? constrained-unknowns)
            ;; No constraints - just place mines in unconstrained cells
            (let [result #js {}
                  unconstrained-vec (vec unconstrained-unknowns)]
              (doseq [k known-mines]
                (aset result k {:mine true}))
              (dotimes [i mines-to-place]
                (aset result (nth unconstrained-vec i) {:mine true}))
              result)

            ;; Run CSP solver
            (let [state (build-solver-state field field-w field-h constrained-unknowns)]
              ;; Initial propagation with empty queue
              (when (assign-and-propagate! state #js [])
                (when (solve-recursive state min-constrained-mines max-constrained-mines)
                  ;; Build result
                  (let [result #js {}
                        cell-keys (aget state "cell-keys")
                        cell-states (aget state "cell-states")]
                    ;; Add known mines
                    (doseq [k known-mines]
                      (aset result k {:mine true}))

                    ;; Add CSP assigned mines
                    (dotimes [i (.-length cell-states)]
                      (when (= MINE (aget cell-states i))
                        (aset result (nth cell-keys i) {:mine true})))

                    ;; Add remaining mines to unconstrained cells
                    (let [csp-mines (aget state "mine-count")
                          remaining-mines (- mines-to-place csp-mines)
                          unconstrained-vec (vec unconstrained-unknowns)]
                      (when (<= remaining-mines unconstrained-count)
                        (dotimes [i remaining-mines]
                          (aset result (nth unconstrained-vec i) {:mine true}))
                        result)))))))))))))
