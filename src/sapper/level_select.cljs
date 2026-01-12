(ns sapper.level-select
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [ctx canvas-w canvas-h]])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

(def statuses)
(def hover-idx nil)

(def tab)

(defn set-tab [t]
  (set! tab t)
  (set! js/window.location.hash (str "level-select/" tab))
  (core/request-render))

(defn on-enter []
  (set-tab (second @core/*screen))
  (set! statuses (core/puzzle-statuses)))

(defn on-render []
  (let [[left top _ _] core/safe-area
        puzzles (get core/puzzles-by-type tab)
        {:keys [won lost started]} statuses
        img     (get core/images "level_select.png")
        *seed   (atom (js/Number. (subs tab 3 4)))]
    (doseq [[i puzzle] (core/indexed puzzles)
            :let [_            (swap! *seed #(-> % (* 1103515245) (+ 12345) (mod 2147483648)))
                  x            (mod i 18)
                  y            (quot i 18)
                  hover?       (= hover-idx i)
                  {:keys [id]} puzzle
                  sprite-left  (cond
                                 (and hover? (contains? won id))     700
                                 (contains? won id)                  600
                                 (and hover? (contains? lost id))    500
                                 (contains? lost id)                 400
                                 (and hover? (contains? started id)) 300
                                 (contains? started id)              200
                                 hover?                              100
                                 :else                               0)
                  sprite-top   (-> @*seed (/ 2147483648) (* 5) js/Math.floor (* 100))]]
      (when (< (+ (* y 18) x) (count puzzles))
        (.drawImage ctx img
          sprite-left sprite-top 100 100
          (+ left 20 (* x 30) -10)
          (+  top 40 (* y 30) -10) 50 50)))

    (doseq [[x key] (->> core/puzzles-by-type keys sort core/indexed)]
      (set! (.-fillStyle ctx) (if (= tab key) "#fff" "#2e4d6f"))
      (.beginPath ctx)
      (.roundRect ctx (+ left 110 (* x 90)) (+ top 760) 70 50 4)
      (.fill ctx)

      (set! (.-font ctx) "16px sans-serif")
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (set! (.-fillStyle ctx) (if (= tab key) "#2e4d6f" "#fff"))
      (.fillText ctx key (+ left 110 35 (* x 90)) (+ top 760 25)))))

(defn mouse->idx [x y]
  (let [[left top _ _] core/safe-area]
    (when (core/inside? x y (+ left 20) (+ top 40) (* 30 18) (* 30 23))
      (let [gx      (quot (- x left 20) 30)
            gy      (quot (- y top 40) 30)
            i       (+ (* gy 18) gx)
            puzzles (get core/puzzles-by-type tab)]
        (when (< i (count puzzles))
          i)))))

(defn on-pointer-move [e]
  (let [{:keys [x y]} e
        hover-idx' (mouse->idx x y)]
    (when (not= hover-idx hover-idx')
      (set! hover-idx hover-idx')
      (core/request-render))))

(defn on-pointer-up [e]
  (let [{:keys [x y start-x start-y]} e
        [left top _ _] core/safe-area
        in? (fn [l t w h] (core/both-inside? start-x start-y x y (+ left l) (+ top t) w h))]
    (cond
      (in? 20 40 (* 30 18) (* 30 23))
      (when-some [idx (mouse->idx x y)]
        (let [id (-> core/puzzles-by-type (get tab) (nth idx) :id)]
          (reset! core/*screen [:game id])))

      (in? 110 760 70 50 10)
      (set-tab "[V]5x5")

      (in? (+ 110 (* 1 90)) 760 70 50 10)
      (set-tab "[V]6x6")

      (in? (+ 110 (* 2 90)) 760 70 50 10)
      (set-tab "[V]7x7")

      (in? (+ 110 (* 3 90)) 760 70 50 10)
      (set-tab "[V]8x8"))))

(assoc! core/screens :level-select
  {:on-enter        on-enter
   :on-render       on-render
   :on-pointer-move on-pointer-move
   :on-pointer-up   on-pointer-up
   :resources       #{"v5.txt" "v6.txt" "v7.txt" "v8.txt"
                      "level_select.png"}})
