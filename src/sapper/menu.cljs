(ns sapper.menu
  (:require
   [sapper.core :as core :refer [ctx canvas-w canvas-h]]))

(def types)
(def buttons)

(defn on-enter []
  (set! types (->> core/puzzles-by-type keys sort))
  (let [[left top width height] core/safe-area]
    (set! buttons
      [{:l (- width 75) :t 25 :w 50 :h 50 :icon "btn_settings.png" :on-click #(core/navigate [:settings])}])

    (let [cols           4
          rows           (-> (dec (count types)) (quot cols) inc)
          btn-w          (-> width (- 50) (- (* 20 (dec cols))) (/ cols))
          t              (-> height (- (* rows (+ 50 20))) (quot 2))]
      (doseq [y (range rows)
              x (range cols)
              :let [i (+ (* y cols) x)]
              :when (< i (count types))
              :let [type (nth types i)]]
        (conj! buttons {:l (+ 25 (* x (+ btn-w 20))) :t (+ t (* y 70)) :w btn-w :h 50
                        :text type
                        :on-click (fn [_]
                                    (core/navigate [:level-select type]))})))))

(defn on-render []
  (doseq [b buttons]
    (core/button-render b)))

(defn on-pointer-move [e]
  (doseq [b buttons]
    (core/button-on-pointer-move b e)))

(defn on-pointer-up [e]
  (doseq [b buttons]
    (core/button-on-pointer-up b e)))

(assoc! core/screens :menu
  {:on-enter        on-enter
   :on-render       on-render
   :on-pointer-move on-pointer-move
   :on-pointer-up   on-pointer-up
   :resources       #{"btn_settings.png"}})
