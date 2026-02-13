(ns sapper.menu
  (:require
   [sapper.core :as core :refer [ctx canvas-w canvas-h]]))

(def progress)

(def types
  [[0 "Vanilla"     "[V]"]
   [1 "Quad"        "[Q]"]
   [2 "Connected"   "[C]"]
   [3 "No triplets" "[T]"]
   [4 "Triplets"    "[T']"]])

(def sizes
  [[0 "5×5" "5x5-10"]
   [1 "6×6" "6x6-14"]
   [2 "7×7" "7x7-20"]
   [3 "8×8" "8x8-26"]])

(def buttons-left 195)
(def buttons-top 200)
(def button-w 60)
(def button-h 40)
(def button-gap-w 26)
(def button-gap-h 20)

(def buttons)

(defn on-enter []
  (set! progress
    (core/parse-progress
      (or (js/localStorage.getItem core/progress-key) "")))
  (core/sync-progress #(set! progress %))
  (set! buttons
    (vec
      (concat
        [{:l (- core/safe-w 60) :t 10 :w 50 :h 50 :icon "btn_settings.png" :on-click #(core/navigate [:settings])}]
        (for [[row type-label type] types
              [col size-label size] sizes]
          {:l        (+ buttons-left (* col (+ button-w button-gap-w)))
           :t        (+ buttons-top (* row (+ button-h button-gap-h)))
           :w        button-w
           :h        button-h
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

  ;; Progress

  (doseq [[row type-label type] types
          [col size-label size] sizes
          :let [{:keys [solved total]} (get progress (str type size))]
          :when (pos? solved)
          :let [l  (+ buttons-left (* col (+ button-w button-gap-w)) button-w)
                t  (+ buttons-top (* row (+ button-h button-gap-h)) button-h)
                h  20
                w  (max h (+ 10 (* 8 (count (str solved)))))]]
    (set! (.-fillStyle ctx)
      (cond
        (>= solved 10) "#ffd400"
        (>= solved 3)  "#fff"
        :else          "#0D2E4E"))
    (.beginPath ctx)
    (.roundRect ctx (- l (quot w 2)) (- t (quot h 2)) w h (quot h 2))
    (.fill ctx)

    (set! (.-font ctx) "13px font")
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (set! (.-fillStyle ctx)
      (cond
        (>= solved 10) "#0D2E4E"
        (>= solved 3)  "#0D2E4E"
        :else          "#8697A7"))
    (.fillText ctx solved l (+ t 0.5)))

  ;; Puzzle types
  (set! (.-font ctx) "16px font")
  (set! (.-textAlign ctx) "left")
  (set! (.-textBaseline ctx) "middle")
  (set! (.-fillStyle ctx) "#fff")
  (doseq [[row type-label _] types]
    (.fillText ctx type-label 85 (+ buttons-top (* row (+ button-h button-gap-h)) (quot button-h 2)))))

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