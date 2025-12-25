(ns sapper.level-select
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [ctx canvas-w canvas-h]])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

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

(defn render []
  (let [[left top _ _] core/safe-area
        puzzles (get core/puzzles tab)]
    (doseq [[i puzzle] (core/indexed puzzles)
            :let [x (mod i 18)
                  y (quot i 18)]]
      (when (< (+ (* y 18) x) (count puzzles))
        (set! (.-fillStyle ctx) "#2e4d6f")
        (.beginPath ctx)
        (.roundRect ctx (+ left 20 (* x 30) 1) (+ top 40 (* y 30) 1) 28 28 2)
        (.fill ctx)

        (set! (.-font ctx) "8px sans-serif")
        (set! (.-textAlign ctx) "center")
        (set! (.-textBaseline ctx) "middle")
        (set! (.-fillStyle ctx) "#113050")
        (.fillText ctx (:id puzzle) (+ left 20 (* x 30) 15) (+ top 40 (* y 30) 15))))

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
        (core/inside? x y (+ left 110) (+ top 760) 70 50 10)
        (set-tab "v5.txt")

        (core/inside? x y (+ left 110 (* 1 90)) (+ top 760) 70 50 10)
        (set-tab "v6.txt")

        (core/inside? x y (+ left 110 (* 2 90)) (+ top 760) 70 50 10)
        (set-tab "v7.txt")

        (core/inside? x y (+ left 110 (* 3 90)) (+ top 760) 70 50 10)
        (set-tab "v8.txt")))))
