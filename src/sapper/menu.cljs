(ns sapper.menu
  (:require
   [sapper.core :as core :refer [ctx canvas-w canvas-h]]))

(def types
  [[0 "Vanilla"   "[V]"]
   [1 "Quad"      "[Q]"]
   [2 "Connected" "[C]"]
   [3 "Triplets"  "[T]"]])

(def sizes
  [[0 "5×5" "5x5-10"]
   [1 "6×6" "6x6-14"]
   [2 "7×7" "7x7-20"]
   [3 "8×8" "8x8-26"]])

(def buttons-left 195)
(def buttons-top 200)
(def button-w 60)

(def buttons)

(defn on-enter []
  (set! buttons
    (vec
      (concat
        [{:l (- core/safe-w 60) :t 10 :w 50 :h 50 :icon "btn_settings.png" :on-click #(core/navigate [:settings])}]
        (for [[row type-label type] types
              [col size-label size] sizes]
          {:l        (+ buttons-left (* col (+ button-w 20)))
           :t        (+ buttons-top (* row (+ 50 20)))
           :w        button-w
           :h        50
           :text     size-label
           :on-click (fn [_]
                       (core/navigate [:level-select (str type size)]))})))))

(defn on-render []
  ;; Title
  (set! (.-font ctx) "bold 24px font")
  (set! (.-textAlign ctx) "center")
  (set! (.-textBaseline ctx) "middle")
  (set! (.-fillStyle ctx) "#FFF")
  (.fillText ctx "4 Minesweeper Variants" (quot core/safe-w 2) 35)

  ;; Buttons
  (doseq [b buttons]
    (core/button-render b))

  ;; Puzzle types
  (set! (.-font ctx) "16px font")
  (set! (.-textAlign ctx) "left")
  (set! (.-textBaseline ctx) "middle")
  (set! (.-fillStyle ctx) "#fff")
  (doseq [[row type-label _] types]
    (.fillText ctx type-label 85 (+ buttons-top (* row (+ 50 20)) 25))))

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
   :resources       (js/Set.
                      (concat
                        ["btn_settings.png"]
                        (for [[_ _ type] types
                              [_ _ size] sizes]
                          (str type size ".txt"))))})