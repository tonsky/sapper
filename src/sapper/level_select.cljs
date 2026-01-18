(ns sapper.level-select
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [ctx canvas-w canvas-h]])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

(def statuses)
(def hover-idx nil)
(def type)
(def buttons)

(defn on-enter []
  (let [[left top width] core/safe-area
        [_ t] @core/*screen]
    (set! type t)
    (set! js/window.location.hash (str "level-select/" t))
    (set! statuses (core/puzzle-statuses t))
    (set! buttons
      [{:l 25 :t 25 :w 50 :h 50 :icon "btn_back.png"   :on-click #(reset! core/*screen [:menu])}
       {:l 100 :t 25 :w 50 :h 50 :icon "btn_reload.png" :on-click core/reload}
       {:l (- width 75) :t 25 :w 50 :h 50 :icon "btn_random.png" :on-click #(core/load-random-puzzle type)}])
    (core/sync-history t
      (fn [_]
        (set! statuses (core/puzzle-statuses t))
        (core/render)))))

(defn on-render []
  (doseq [b buttons]
    (core/button-render b))

  (let [[left top _ _] core/safe-area
        puzzles (get core/puzzles-by-type type)
        {:keys [won lost started]} statuses
        img     (get core/images "level_select.png")
        rng     (core/make-rng (js/parseInt (subs type 3 4)))]
    (doseq [[i puzzle] (core/indexed puzzles)
            :let [_            (core/advance-rng rng)
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
                  sprite-top   (-> (core/random rng) (* 5) js/Math.floor (* 100))]]
      (when (< (+ (* y 18) x) (count puzzles))
        (.drawImage ctx img
          sprite-left sprite-top 100 100
          (+ left 20 (* x 30) -10)
          (+  top 100 (* y 30) -10) 50 50)))))

(defn mouse->idx [x y]
  (let [[left top _ _] core/safe-area]
    (when (core/inside? x y (+ left 20)(+ top 100) (* 30 18) (* 30 23))
      (let [gx      (quot (- x left 20) 30)
            gy      (quot (- y top 100) 30)
            i       (+ (* gy 18) gx)
            puzzles (get core/puzzles-by-type type)]
        (when (< i (count puzzles))
          i)))))

(defn on-pointer-move [e]
  (doseq [b buttons]
    (core/button-on-pointer-move b e))
  (let [{:keys [x y]} e
        hover-idx' (mouse->idx x y)]
    (when (not= hover-idx hover-idx')
      (set! hover-idx hover-idx')
      (core/request-render))))

(defn on-pointer-up [e]
  (doseq [b buttons]
    (core/button-on-pointer-up b e))
  (let [{:keys [x y start-x start-y]} e
        [left top _ _] core/safe-area]
    (when (core/both-inside? start-x start-y x y (+ left 20) (+ top 100) (* 30 18) (* 30 23))
      (when-some [idx (mouse->idx x y)]
        (let [id (-> core/puzzles-by-type (get type) (nth idx) :id)]
          (reset! core/*screen [:game id]))))))

(assoc! core/screens :level-select
  {:on-enter        on-enter
   :on-render       on-render
   :on-pointer-move on-pointer-move
   :on-pointer-up   on-pointer-up
   :resources       #{"[V]5x5-10.txt" "[V]6x6-14.txt" "[V]7x7-20.txt" "[V]8x8-26.txt"
                      "level_select.png"}})
