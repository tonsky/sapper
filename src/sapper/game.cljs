(ns sapper.game
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [canvas ctx notes-ctx canvas-w canvas-h canvas-scale dpi images safe-area sprite-size]]
   [sapper.level-select :as level-select])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

(def puzzle nil)

(def modern true)
(def cell-size 70)
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

(def buttons)

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
(def tool-size 60)
(def notes [])
(def anim-start 0)

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

(defn undo []
  (when (seq notes)
    (.pop notes)
    (core/request-render)))

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
  (let [[_ id]           @core/*screen
        _                (set! js/window.location.hash (str "game/" id))
        _                (set! puzzle (get core/puzzles-by-id id))
        [left top width] core/safe-area
        code             (:code puzzle)
        [_ fw fh]        (re-find #"(\d+)x(\d+)" id)
        len              (count code)]

    (set! buttons
      [{:l 25 :t 25 :w 50 :h 50 :icon "btn_back.png"   :on-click #(reset! core/*screen [:level-select (:type puzzle)])}
       {:l 100 :t 25 :w 50 :h 50 :icon "btn_reload.png" :on-click core/reload}])

    (set! phase :init)
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
            "F" {:mine true,  :open true}
            "o" {:mine false, :open false}
            "O" {:mine false, :open true}
            "q" {:mine false, :open false, :label "q"}
            "Q" {:mine false, :open true,  :label "q"}))))

    (update-field)
    (set! outline-x nil)
    (set! outline-y nil)
    (set! phase :new)
    (set! dragging-flag false)
    (set! drag-x nil)
    (set! drag-y nil)
    (set! drag-device nil)
    (set! tool nil)
    (set! notes [])
    (set! anim-start (js/Date.now))
    (js/requestAnimationFrame #(core/render))))

(defn on-render []
  (let [anim-length       300
        anim-progress     (core/clamp (/ (- (js/Date.now) anim-start) anim-length) 0 1)
        [hover-x hover-y] (when (and drag-x drag-y)
                            (field-coords drag-x drag-y))
        id                (:id puzzle)]
    ;; level name
    (set! (.-font ctx) "10px sans-serif")
    (set! (.-textAlign ctx) "left")
    (set! (.-fillStyle ctx) "#477397")
    (.fillText ctx id 13 47)

    ;; buttons
    (doseq [b buttons]
      (core/button-render b))

    ;; cells
    (doseq [y (range field-w)
            x (range field-h)]
      (let [{:keys [mine flagged open label solved]} (get-cell x y)
            err            (and label (or (str/starts-with? label "-")
                                        (str/starts-with? label "error_")))
            name           (cond
                             (and (not tool) (not open) (not flagged) (= x hover-x) (= y hover-y)) "hover.png"
                             flagged                          "flagged.png"
                             (not open)                       "closed.png"
                             err                              (str label ".png")
                             (and open solved)                (str label "_solved.png")
                             (= "q" label)                    "q_solved.png"
                             :else                            (str label ".png"))
            img            (get images name)
            anim-stagger   (-> (+ (* y field-w) x) (/ field-w field-h))
            anim-progress' (-> anim-progress (- (* anim-stagger 0.75)) (* 4) (core/clamp 0 1))
            anim-travel    (/ (core/dist [0 0] [x y]) (core/dist [0 0] [field-w field-h]))
            anim-offset-x  (* (- 1 anim-progress') (* anim-travel 30))
            anim-offset-y  (* (- 1 anim-progress') (* anim-travel 50))
            px             (-> (* x cell-size) (+ grid-x) (- margin) (+ anim-offset-x))
            py             (-> (* y cell-size) (+ grid-y) (- margin) (+ anim-offset-y))]
        (set! (.-globalAlpha ctx) anim-progress')
        (.drawImage ctx img px py sprite-size sprite-size)))
    (set! (.-globalAlpha ctx) 1)

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

    ;; Draw notes
    (.clearRect notes-ctx 0 0 canvas-w canvas-h)
    (let [[sa-x sa-y] safe-area]
      (doseq [stroke notes
              :let [t         (:tool stroke)
                    points    (:points stroke)
                    nth-point #(let [[x y] (aget points %)]
                                 [(+ x sa-x) (+ y sa-y)])]]
        (when (seq points)
          (set! (.-lineWidth notes-ctx) (case t :eraser 40 6))
          (set! (.-strokeStyle notes-ctx) (get tool-colors t "#000"))
          (set! (.-lineCap notes-ctx) "round")
          (set! (.-lineJoin notes-ctx) "round")
          (set! (.-globalCompositeOperation notes-ctx) (case t :eraser "destination-out" "source-over"))
          (.beginPath notes-ctx)
          (let [[x y] (nth-point 0)]
            (.moveTo notes-ctx x y))
          (dotimes [i (dec (count points))]
            (let [[x1 y1] (nth-point i)
                  [x2 y2] (nth-point (inc i))]
              (.quadraticCurveTo notes-ctx x1 y1 (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))
          (let [[x y] (nth-point (dec (count points)))]
            (.lineTo notes-ctx x y))

          (.stroke notes-ctx)
          (set! (.-globalCompositeOperation notes-ctx) "source-over"))))

    ;; Eraser cursor
    (when (and drag-x drag-y
            (or
              (= :eraser tool)
              (and tool (= :mouse-right drag-device))))
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
      (.fillText ctx (case phase :game-over "Game Over :(" :victory "Victory!")
        (+ grid-x (quot grid-w 2)) (+ grid-y (quot grid-h 2))))

    ;; Animation
    (when (< anim-progress 1)
      (js/requestAnimationFrame #(core/render)))))

(defn open-cell [gx gy]
  (when (= :new phase)
    (set! phase :play)
    (core/append-history (:id puzzle) :start))
  (let [key                         (key gx gy)
        {:keys [mine open flagged]} (get-cell gx gy)]
    (cond
      open    :noop
      flagged (do
                (assoc! (get field key) :flagged false)
                (update-field))
      mine    (do
                (core/append-history (:id puzzle) :lose)
                (set! phase :game-over)
                (core/request-render))
      :else   (do
                (assoc! (get field key) :open true)
                (update-field)))))

(defn flag-cell [gx gy]
  (when (= :new phase)
    (set! phase :play)
    (core/append-history (:id puzzle) :start))
  (let [key                    (key gx gy)
        {:keys [flagged open]} (get-cell gx gy)]
    (cond
      open         :noop
      flagged      (do
                     (assoc! (get field key) :flagged false)
                     (update-field))
      (<= flags 0) :noop
      :else        (do
                     (assoc! (get field key) :flagged true)
                     (update-field)))))

(defn on-tool-click [tool']
  (case tool'
    :undo
    (undo)

    :clear
    (do
      (set! notes [])
      (set! tool nil)
      (set! drag-x nil)
      (set! drag-y nil)
      (set! drag-device nil)
      (core/request-render))

    (:eraser :color1 :color2 :color3 :color4)
    (do
      (if (= tool tool')
        (do
          (set! tool nil)
          (set! drag-x nil)
          (set! drag-y nil)
          (set! drag-device nil))
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
      (and (or (.-ctrlKey e) (.-metaKey e)) (= "z" key))
      (undo)

      (or (.-altKey e) (.-ctrlKey e) (.-metaKey e) (.-shiftKey e))
      :noop

      (#{"1" "d"} key)
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
      (core/reload)

      (= "Escape" key)
      (do
        (cond
          tool
          (do
            (set! tool nil)
            (core/request-render))

          (and outline-x outline-y)
          (do
            (set! outline-x nil)
            (set! outline-y nil)
            (core/request-render)))))))

(defn on-pointer-down [{:keys [x y device]}]
  (set! drag-x x)
  (set! drag-y y)
  (set! drag-device device)
  (cond+
    tool
    (let [tool' (if (= :mouse-right device) :eraser tool)]
      (conj! notes {:tool tool' :points []})
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

(defn on-pointer-move [{:keys [x y device] :as e}]
  (doseq [b buttons]
    (core/button-on-pointer-move b e))

  (when (and (#{:mouse-left :mouse-right :touch} device) tool (seq notes))
    (let [[sa-x sa-y] safe-area
          last-stroke (aget notes (dec (count notes)))
          points      (:points last-stroke)
          rel-x       (- x sa-x)
          rel-y       (- y sa-y)
          num-points  (count points)]
      (if (and
            (>= num-points 2)
            (< (core/dist (last points) (core/penultimate points)) 5))
        (aset points (dec num-points) [rel-x rel-y])
        (conj! points [rel-x rel-y]))))
  (when (or
          (= :eraser tool)
          (and (#{:mouse-left :mouse-right :touch} device) tool)
          (and (#{:mouse-left :touch} device) dragging-flag))
    (set! drag-x x)
    (set! drag-y y)
    (core/request-render)))

(defn on-pointer-up [{:keys [x y start-x start-y device] :as e}]
  (doseq [b buttons]
    (core/button-on-pointer-up b e))

  (set! drag-x nil)
  (set! drag-y nil)
  (set! drag-device nil)

  (when (and tool (seq notes))
    (let [points (:points (last notes))]
      (when
        (or
          (= 0 (count points))
          (= 1 (count points))
          (and
            (= 2 (count points))
            (< (core/dist (nth points 0) (nth points 1)) 5)))
        (.pop notes))))

  (let [[gx gy]   (field-coords x y)
        toolbox-w (* (count tools) tool-size)
        toolbox-x (quot (- canvas-w toolbox-w) 2)
        toolbox-y (+ grid-y grid-h 115)
        in?       #(core/both-inside? x y start-x start-y %1 %2 %3 %4 %5)]
    (cond+
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

      tool
      :noop

      ;; end game
      (#{:game-over :victory} phase)
      :noop

      ;; end outside field
      (not (and gx gy))
      (do
        (set! outline-x nil)
        (set! outline-y nil)
        (core/request-render))

      ;; auto-open neighbors if label matches unopened count
      :let [cell (when (and gx gy) (get-cell gx gy))]

      (and
        (:open cell)
        (re-matches #"\d+" (or (:label cell) ""))
        (let [label-num (parse-long (:label cell))
              nbs       (->> (neighbours gx gy)
                          (remove (fn [[x y]] (processed (get-cell x y)))))]
          (cond
            (= (count nbs) label-num)
            (do
              (doseq [[i [nx ny]] (core/indexed nbs)]
                (core/set-timeout (* i 100) #(flag-cell nx ny)))
              true)

            (= 0 label-num)
            (do
              (doseq [[i [nx ny]] (core/indexed nbs)]
                (core/set-timeout (* i 100) #(open-cell nx ny)))
              true))))
      :noop

      ;; second click on outlined cell
      (and (= gx outline-x) (= gy outline-y))
      (do
        (set! outline-x nil)
        (set! outline-y nil)
        (core/request-render))

      ;; click on open cell
      (:open cell)
      (do
        (set! outline-x gx)
        (set! outline-y gy)
        (core/request-render))

      ;; click on closed cell
      :else
      (case device
        (:mouse-left :touch) (open-cell gx gy)
        :mouse-right         (flag-cell gx gy)))))

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
