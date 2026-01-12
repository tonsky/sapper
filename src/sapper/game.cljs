(ns sapper.game
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [canvas ctx notes notes-ctx canvas-w canvas-h canvas-scale dpi images]]
   [sapper.level-select :as level-select])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

(def puzzle nil)

(def modern true)
(def cell-size 70)
(def sprite-size 100)
(def margin (-> sprite-size (- cell-size) (/ 2)))
(def field {})
(def flags 0)
(def field-w 0)
(def field-h 0)
(def grid-x 0)
(def grid-y 0)
(def grid-w 0)
(def grid-h 0)
(def outline-x nil)
(def outline-y nil)
(def phase)

(def dragging-flag false)
(def drag-x)
(def drag-y)
(def drag-device)

(def tool nil)
(def tools [:undo :eraser :color1 :color2 :color3 :color4 :clear])
(def tool-colors
  {:color1 "#F6CB1D"
   :color2 "#3FC833"
   :color3 "#F44D44"
   :color4 "#25D0FF"})
(def tool-points [nil nil])
(def tool-size 60)

(declare open-cell flag-cell)

(defn neighbours [x y]
  (concat
    (when (and (> x 0) (> y 0))
      [[(dec x) (dec y)]])
    (when (> x 0)
      [[(dec x)      y ]])
    (when (and (> x 0) (< y (dec field-h)))
      [[(dec x) (inc y)]])
    (when (> y 0)
      [[     x  (dec y)]])
    (when (< y (dec field-h))
      [[     x  (inc y)]])
    (when (and (< x (dec field-w)) (> y 0))
      [[(inc x) (dec y)]])
    (when (< x (dec field-w))
      [[(inc x)      y ]])
    (when (and (< x (dec field-w)) (< y (dec field-h)))
      [[(inc x) (inc y)]])))

(defn key [x y]
  (str x "," y))

(defn parse-key [key]
  (let [[x y] (str/split key ",")]
    [(js/Number x) (js/Number y)]))

(defn get-cell [x y]
  (get field (key x y)))

(defn field-coords [x y]
  (when (core/inside? x y grid-x grid-y grid-w grid-h)
    [(quot (- x grid-x) cell-size)
     (quot (- y grid-y) cell-size)]))

(defn flag-area []
  (let [flags' (cond-> flags dragging-flag dec)]
    (when (pos? flags')
      (let [max-flags 26
            flag-gap  (+ 20 (-> max-flags (- flags') (/ max-flags) (* 5) (max 0)))
            total-w   (+ (* (dec flags') flag-gap) cell-size -5)]
        [(-> canvas-w (- total-w) (quot 2))
         (+ grid-y grid-h 30)
         total-w
         cell-size
         flags'
         flag-gap]))))

(defn processed [cell]
  (or
    (:flagged cell)
    (:open cell)))

(defn-log update-field []
  (doseq [[key cell] field
          :let [[x y] (parse-key key)
                {:keys [mine label solved]} cell
                nbs   (map
                        (fn [[x y]] (get-cell x y))
                        (neighbours x y))
                solved' (every? processed nbs)]]
    (when (and (not= "q" label) (not mine))
      (let [mines  (count (filter :mine nbs))
            flags  (count (filter :flagged nbs))
            label' (cond
                     modern          (str (- mines flags))
                     (> flags mines) (str "error_" mines)
                     :else           (str mines))]
        (assoc! cell :label label')))
    (assoc! cell :solved solved')
    (when (and
            (not solved)
            solved'
            (= outline-x x)
            (= outline-y y))
      (set! outline-x nil)
      (set! outline-y nil)))
  (set! flags (- (->> field vals (filter :mine) count)
                (->> field vals (filter :flagged) (count))))
  (when (->> field vals (every? processed))
    (core/append-history (:id puzzle) :win)
    (set! phase :victory))
  (core/request-render))

(defn-log on-enter []
  (let [[_ id]   @core/*screen
        _        (set! js/window.location.hash (str "game/" id))
        _        (set! puzzle (get core/puzzles-by-id id))
        code     (:code puzzle)
        [_ fw fh] (re-find #"(\d+)x(\d+)" id)
        len      (count code)
        *to-open (atom [])]
    (set! phase :new)
    (set! field-w (parse-long fw))
    (set! field-h (parse-long fh))
    (set! grid-w (* field-w cell-size))
    (set! grid-h (* field-h cell-size))
    (set! grid-x (-> canvas-w (- grid-w) (quot 2)))
    (set! grid-y (-> canvas-h (- 100) (- grid-h) (quot 2)))
    (set! field {})

    (dotimes [i (* field-w field-h)]
      (let [x  (mod i field-w)
            y  (quot i field-w)
            ch (nth code i)]
        (assoc! field (key x y)
          (case ch
            "f" {:mine true,  :open false}
            "F" {:mine true,  :open false}
            "o" {:mine false, :open false}
            "O" {:mine false, :open false}
            "q" {:mine false, :open false, :label "q"}
            "Q" {:mine false, :open true,  :label "q"}))
        (case ch
          "F"       (swap! *to-open conj #(flag-cell x y))
          ("O" "Q") (swap! *to-open conj #(open-cell x y))
          nil)))

    (doseq [[i f] (core/indexed (shuffle @*to-open))]
      (core/set-timeout (* i 50) f))

    (update-field)
    (set! tool nil)))

(defn on-render []
  (let [[hover-x hover-y] (when (and drag-x drag-y)
                            (field-coords drag-x drag-y))]
    ;; level name
    (let [id (:id puzzle)]
      (set! (.-font ctx) "10px sans-serif")
      (set! (.-textAlign ctx) "left")
      (set! (.-fillStyle ctx) "#284E6D")
      (.fillText ctx id 13 35))

    ;; buttons
    (.drawImage ctx (get images "btn_back.png") (- canvas-w 250) 0 sprite-size sprite-size)
    (.drawImage ctx (get images "btn_retry.png") (- canvas-w 175) 0 sprite-size sprite-size)
    (.drawImage ctx (get images "btn_reload.png") (- canvas-w 100) 0 sprite-size sprite-size)

    ;; cells
    (doseq [y (range field-w)
            x (range field-h)]
      (let [{:keys [mine flagged open label solved]} (get-cell x y)
            err  (and label (or (str/starts-with? label "-")
                              (str/starts-with? label "error_")))
            name (cond
                   (and (not tool) (not open) (not flagged) (= x hover-x) (= y hover-y)) "hover.png"
                   flagged                          "flagged.png"
                   (not open)                       "closed.png"
                   err                              (str label ".png")
                   (and open solved)                (str label "_solved.png")
                   (= "q" label)                    "q_solved.png"
                   :else                            (str label ".png"))
            img  (get images name)
            px   (-> (* x cell-size) (+ grid-x) (- margin))
            py   (-> (* y cell-size) (+ grid-y) (- margin))]
        (.drawImage ctx img px py sprite-size sprite-size)))

    ;; outline
    (when (and outline-x outline-y)
      (let [left    (max 0 (dec outline-x))
            top     (max 0 (dec outline-y))
            right   (min (dec field-w) (inc outline-x))
            bottom  (min (dec field-h) (inc outline-y))
            padding 4
            {:keys [label mine solved]}(get-cell outline-x outline-y)
            color   (cond
                      mine          "#E15757"
                      solved        "#73A2C9"
                      (= "q" label) "#73A2C9"
                      (= "0" label) "#006073"
                      (= "1" label) "#0A9496"
                      (= "2" label) "#95D2BD"
                      (= "3" label) "#E9D8A6"
                      (= "4" label) "#EE9C02"
                      (= "5" label) "#CA6702"
                      (= "6" label) "#BC3E02"
                      (= "7" label) "#AF2012"
                      (= "8" label) "#9B2226"
                      :else         #"73A2C9")]
        (set! (.-strokeStyle ctx) color)
        (set! (.-lineWidth ctx) 3)
        (.beginPath ctx)
        (.roundRect ctx
          (+ grid-x (* left cell-size) (- padding))
          (+ grid-y (* top cell-size) (- padding))
          (-> (inc right) (- left) (* cell-size) (+ (* 2 padding)))
          (-> (inc bottom) (- top) (* cell-size) (+ (* 2 padding)))
          10)
        (.stroke ctx)))

    ;; Flags
    (when-some [[l t w h flags' flag-gap] (flag-area)]
      (let [flag-img (get images "flag.png")]
        (set! (.-fillStyle ctx) "#082848")
        (.beginPath ctx)
        (.roundRect ctx l t w h 6)
        (.fill ctx)
        (dotimes [i flags']
          (.drawImage ctx flag-img
            (-> l (+ (* i flag-gap)) (- margin))
            (- t margin)
            sprite-size sprite-size))))

    ;; Dragged flag
    (when dragging-flag
      (let [flag-img (get images "flag.png")]
        (.drawImage ctx flag-img
          (-> drag-x (- margin) (- (quot cell-size 2)))
          (-> drag-y (- margin) (- (case drag-device
                                     :mouse-left (quot cell-size 2)
                                     :touch      cell-size)))
          sprite-size sprite-size)))

    ;; Tools
    (let [width       (* (count tools) tool-size)
          left        (quot (- canvas-w width) 2)
          tool-margin (-> sprite-size (- tool-size) (/ 2))]
      (doseq [[i t] (core/indexed tools)
              :let [x   (+ left (* i tool-size))
                    y   (+ grid-y grid-h 115)
                    img (get images (str "tool_" t (if (= t tool) "_selected" "") ".png"))]]
        (.drawImage ctx img (- x tool-margin) (- y tool-margin) sprite-size sprite-size)))

    ;; Eraser cursor
    (when (and (= :eraser tool) drag-x drag-y)
      (set! (.-strokeStyle ctx) "#FFFFFF20")
      (set! (.-lineWidth ctx) 1)
      (.beginPath ctx)
      (.arc ctx drag-x drag-y 20 0 (* 2 js/Math.PI))
      (.stroke ctx))

    ;; End game screen
    (when (#{:game-over :victory} phase)
      (set! (.-fillStyle ctx) "#072947F4")
      (.fillRect ctx 0 (+ grid-y (quot (- grid-h 90) 2)) canvas-w 90)
      (set! (.-font ctx) "40px sans-serif")
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (set! (.-fillStyle ctx) "#FFF")
      (.fillText ctx (case phase :game-over "Game Over :(" :victory "Victory :)")
        (+ grid-x (quot grid-w 2)) (+ grid-y (quot grid-h 2))))))

(defn open-cell [gx gy]
  (when (= :new phase)
    (set! phase :play)
    (core/append-history (:id puzzle) :start))
  (let [key                         (key gx gy)
        {:keys [mine open flagged]} (get-cell gx gy)]
    (when-not open
      (cond
        flagged (assoc! (get field key) :flagged false)
        mine    (do
                  (core/append-history (:id puzzle) :lose)
                  (set! phase :game-over))
        :else   (assoc! (get field key) :open true))
      (update-field))))

(defn flag-cell [gx gy]
  (when (= :new phase)
    (set! phase :play)
    (core/append-history (:id puzzle) :start))
  (let [key                    (key gx gy)
        {:keys [flagged open]} (get-cell gx gy)]
    (cond
      open
      :noop

      flagged
      (do
        (assoc! (get field key) :flagged false)
        (update-field))

      (<= flags 0)
      :noop

      :else
      (do
        (assoc! (get field key) :flagged true)
        (update-field)))))

(defn reload []
  #_(reset! core/*screen @core/*screen)
  (.reload (.-location js/window)))

(defn on-tool-click [tool']
  (case tool'
    :undo
    :TODO

    :clear
    (do
      (aset tool-points 0 nil)
      (aset tool-points 1 nil)
      (.clearRect notes-ctx 0 0 canvas-w canvas-h)
      (core/request-render))

    (:eraser :color1 :color2 :color3 :color4)
    (do
      (if (= tool tool')
        (set! tool nil)
        (set! tool tool'))
      (core/request-render))))

(defn on-resize []
  (set! grid-x (-> core/canvas-w (- grid-w) (quot 2)))
  (set! grid-y (-> core/canvas-h (- 100) (- grid-h) (quot 2)))
  (core/request-render))

(defn on-key-down [e]
  (let [key (.-key e)
        mod (or (.-altKey e) (.-ctrlKey e) (.-metaKey e) (.-shiftKey e))]
    (cond
      (or (.-altKey e) (.-ctrlKey e) (.-metaKey e) (.-shiftKey e))
      :noop

      (= "1" key)
      (on-tool-click :color1)

      (= "2" key)
      (on-tool-click :color2)

      (= "3" key)
      (on-tool-click :color3)

      (= "4" key)
      (on-tool-click :color4)

      (= "e" key)
      (on-tool-click :eraser)

      (= "c" key)
      (on-tool-click :clear)

      (= "r" key)
      (reload)

      (= "Escape" key)
      (do
        (set! tool nil)
        (core/request-render)))))

(defn on-pointer-down [{:keys [x y device]}]
  (set! drag-x x)
  (set! drag-y y)
  (set! drag-device device)
  (cond+
    tool
    (do
      (set! (.-lineWidth notes-ctx) (case tool :eraser 40 6))
      (set! (.-strokeStyle notes-ctx) (get tool-colors tool "#000"))
      (set! (.-lineCap notes-ctx) "round")
      (set! (.-lineJoin notes-ctx) "round")
      (set! (.-globalCompositeOperation notes-ctx) (case tool :eraser "destination-out" "source-over"))
      (aset tool-points 0 nil)
      (aset tool-points 1 [x y])
      (core/request-render))

    :let [[l t w h] (flag-area)]

    (and
      (#{:new :play} phase)
      (#{:mouse-left :touch} device)
      (core/inside? x y l t w h margin))
    (do
      (set! dragging-flag true)
      (set! tool nil)
      (core/request-render))))

(defn on-pointer-move [{:keys [x y device]}]
  (when (and (#{:mouse-left :touch} device) tool)
    (let [[x0 y0] (aget tool-points 0)
          [x1 y1] (aget tool-points 1)]
      (when (and x1 y1 (>= (js/Math.hypot (- x x1) (- y y1)) 5))
        (when (and (nil? x0) (nil? y0) x1 y1)
          (.beginPath notes-ctx)
          (.moveTo notes-ctx x1 y1))
        (when (and x0 y0 x1 y1)
          (.quadraticCurveTo notes-ctx x1 y1 (/ (+ x1 x) 2) (/ (+ y1 y) 2))
          #_(.lineTo notes-ctx x y)
          (.stroke notes-ctx))
        (aset tool-points 0 [x1 y1])
        (aset tool-points 1 [x y]))))
  (when (or
          (= :eraser tool)
          (and (#{:mouse-left :touch} device)
            (or tool dragging-flag)))
    (set! drag-x x)
    (set! drag-y y)
    (core/request-render)))

(defn on-pointer-up [{:keys [x y start-x start-y device]}]
  (set! drag-x nil)
  (set! drag-y nil)
  (set! drag-device nil)
  (let [[gx gy]   (field-coords x y)
        toolbox-w (* (count tools) tool-size)
        toolbox-x (quot (- canvas-w toolbox-w) 2)
        toolbox-y (+ grid-y grid-h 115)
        in?       #(core/both-inside? x y start-x start-y %1 %2 %3 %4 %5)]
    (cond
      ;; drop flag
      dragging-flag
      (do
        (when-some [[gx gy] (field-coords x y)]
          (let [{:keys [open]} (get-cell gx gy)]
            (flag-cell gx gy)))
        (set! dragging-flag false)
        (core/request-render))

      ;; toolbox
      (in? toolbox-x (+ grid-y grid-h 115) toolbox-w tool-size)
      (let [i (quot (- x toolbox-x) tool-size)
            t (nth tools i)]
        (on-tool-click (nth tools i)))

      ;; back button
      (in? (- canvas-w 225) 25 50 50)
      (reset! core/*screen [:level-select (:type puzzle)])

      ;; retry button
      (in? (- canvas-w 150) 25 50 50)
      (reload)

      ;; random button
      (in? (- canvas-w 75) 25 50 50)
      :TODO

      tool
      (do
        (aset tool-points 0 nil)
        (aset tool-points 1 nil))

      ;; end game
      (#{:game-over :victory} phase)
      :noop

      ;; end outside field
      (not (and gx gy))
      (do
        (set! outline-x nil)
        (set! outline-y nil)
        (core/request-render))

      ;; second click on outlined cell
      (and (= gx outline-x) (= gy outline-y))
      (do
        (set! outline-x nil)
        (set! outline-y nil)
        (core/request-render))

      ;; click on open cell
      (:open (get-cell gx gy))
      (do
        (set! outline-x gx)
        (set! outline-y gy)
        (core/request-render))

      ;; click on closed cell
      :else
      (let [key                 (key gx gy)
            {:keys [mine open]} (get-cell gx gy)]
        #_(println "on-grid-click" gx gy action mine open)
        (case device
          (:mouse-left :touch) (open-cell gx gy)
          :mouse-right         (flag-cell gx gy))))))

(assoc! core/screens :game
  {:on-enter        on-enter
   :on-resize       on-resize
   :on-render       on-render
   :on-key-down     on-key-down
   :on-pointer-up   on-pointer-up
   :on-pointer-move on-pointer-move
   :on-pointer-down on-pointer-down
   :resources       #{"0.png" "1.png" "2.png" "3.png" "4.png" "5.png" "6.png" "7.png" "8.png"
                      "0_solved.png" "1_solved.png" "2_solved.png" "3_solved.png" "4_solved.png" "5_solved.png" "6_solved.png" "7_solved.png" "8_solved.png"
                      "-1.png" "-2.png" "-3.png" "-4.png" "-5.png" "-6.png" "-7.png" "-8.png"
                      "error_0.png" "error_1.png" "error_2.png" "error_3.png" "error_4.png" "error_5.png" "error_6.png" "error_7.png"
                      "q.png" "q_solved.png" "closed.png" "hover.png" "flagged.png" "flag.png"
                      "tool_undo.png" "tool_eraser.png" "tool_color1.png" "tool_color2.png" "tool_color3.png" "tool_color4.png" "tool_clear.png"
                      "tool_eraser_selected.png" "tool_color1_selected.png" "tool_color2_selected.png" "tool_color3_selected.png" "tool_color4_selected.png"}})
