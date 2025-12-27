(ns sapper.level-select
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [ctx canvas-w canvas-h]])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

(def won {})
(def started {})
(def lost {})

(def tab
  "v5.txt")

(defn set-tab [t]
  (set! tab t)
  (core/request-render))

(def tabs
  {"v5.txt" "5×5"
   "v6.txt" "6×6"
   "v7.txt" "7×7"
   "v8.txt" "8×8"})

(defn on-load []
  (add-watch core/*screen ::level-select
    (fn [_ _ old new]
      (when (and (not= :level-select old) (= :level-select new))
        (doseq [{:keys [op id]} (core/get-history)]
          (case op
            :win   (assoc! won id)
            :start (assoc! started id)
            :lose  (assoc! lost id)
            nil))))))

(.addEventListener js/window "load" on-load)

(defn render []
  (let [[left top _ _] core/safe-area
        puzzles (get core/puzzles tab)
        img     (get core/images "level_select.png")
        *seed   (atom 0)]
    (doseq [[i puzzle] (core/indexed puzzles)
            :let [_            (swap! *seed #(-> % (* 1103515245) (+ 12345) (mod 2147483648)))
                  x            (mod i 18)
                  y            (quot i 18)
                  {:keys [id]} puzzle
                  sprite-left  (cond
                                 (contains? won id)     400
                                 (contains? lost id)    300
                                 (contains? started id) 200
                                 :else                  0)
                  sprite-top   (-> @*seed (/ 2147483648) (* 5) js/Math.floor (* 100))]]
      (when (< (+ (* y 18) x) (count puzzles))
        (.drawImage ctx img
          sprite-left sprite-top 100 100
          (+ left 20 (* x 30) -10)
          (+  top 40 (* y 30) -10) 50 50)))

    (doseq [[x [key name]] (->> tabs (sort-by first) core/indexed)]
      (set! (.-fillStyle ctx) (if (= tab key) "#fff" "#2e4d6f"))
      (.beginPath ctx)
      (.roundRect ctx (+ left 110 (* x 90)) (+ top 760) 70 50 4)
      (.fill ctx)

      (set! (.-font ctx) "16px sans-serif")
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (set! (.-fillStyle ctx) (if (= tab key) "#2e4d6f" "#fff"))
      (.fillText ctx name (+ left 110 35 (* x 90)) (+ top 760 25)))))

            (defn on-event [e]
              (let [{:keys [x y event]} e
                    [left top _ _] core/safe-area]
                (case event
                  "mouseup"
                  (cond
                    (core/inside? x y (+ left 20) (+ top 40) (* 30 18) (* 30 23))
                    (let [gx      (quot (- x left 20) 30)
                          gy      (quot (- y top 40) 30)
                          i       (+ (* gy 18) gx)
                          puzzles (get core/puzzles tab)]
                      (when (< i (count puzzles))
                        (reset! core/*puzzle (nth puzzles i))
                        (reset! core/*screen :game)))

                    (core/inside? x y (+ left 110) (+ top 760) 70 50 10)
                    (set-tab "v5.txt")

                    (core/inside? x y (+ left 110 (* 1 90)) (+ top 760) 70 50 10)
                    (set-tab "v6.txt")

                    (core/inside? x y (+ left 110 (* 2 90)) (+ top 760) 70 50 10)
                    (set-tab "v7.txt")

                    (core/inside? x y (+ left 110 (* 3 90)) (+ top 760) 70 50 10)
                    (set-tab "v8.txt")))))
