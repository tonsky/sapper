(ns sapper.game
  (:require
   [clojure.string :as str]
   [sapper.puzzles :as puzzles])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

(def canvas nil)
(def ctx nil)
(def notes nil)
(def notes-ctx nil)
(def canvas-w 0)
(def canvas-h 0)
(def puzzle)
(def modern true)
(def dpi (or (.-devicePixelRatio js/window) 1))
(def canvas-scale 1)
(def cell-size 70)
(def sprite-size 100)
(def margin (-> sprite-size (- cell-size) (/ 2)))
(def field {})
(def flags 0)
(def flag-gap 20)
(def field-w 0)
(def field-h 0)
(def grid-x 0)
(def grid-y 0)
(def grid-w 0)
(def grid-h 0)
(def images {})
(def outline-x nil)
(def outline-y nil)
(def screen :loading)
(def render-requested false)

(def dragging-flag false)
(def drag-type nil)
(def drag-x 0)
(def drag-y 0)
(def tool nil)
(def tools [:eraser :color1 :color2 :color3 :color4])
(def tool-colors
  {:eraser "#000"
   :color1 "#F6CB1D"
   :color2 "#3FC833"
   :color3 "#F44D44"
   :color4 "#25D0FF"})
(def tool-points [nil nil])

(declare render maybe-render open-cell flag-cell)

(defn set-timeout [dt f]
  (js/setTimeout #(do (f) (maybe-render)) dt))

(defn add-event-listener [el event f opts]
  (.addEventListener el event
    #(do (f %) (maybe-render))
    opts))

(defn request-render []
  (set! render-requested true))

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

(defn inside? [x y l t w h margin]
  (let [margin (or margin 0)]
    (and
      (<= (- l margin) x)
      (< x (+ l w margin))
      (<= (- t margin) y)
      (< y (+ t h margin)))))

(defn rel-coords [e]
  (let [rect (.getBoundingClientRect canvas)
        x    (-> (.-clientX e) (- (.-left rect)) (* dpi) (/ canvas-scale) js/Math.round)
        y    (-> (.-clientY e) (- (.-top rect)) (* dpi) (/ canvas-scale) js/Math.round)]
    [x y]))

(defn field-coords [x y]
  (when (inside? x y grid-x grid-y grid-w grid-h)
    [(quot (- x grid-x) cell-size)
     (quot (- y grid-y) cell-size)]))

(defn flag-area []
  (let [flags' (cond-> flags dragging-flag dec)]
    (when (pos? flags')
      (let [total-w  (+ (* (dec flags') flag-gap) cell-size)]
        [(-> canvas-w (- total-w) (quot 2))
         (+ grid-y grid-h 30)
         total-w
         cell-size
         flags']))))

(defn indexed [seq]
  (map vector (range) seq))

(defn-log update-field []
  (doseq [[key cell] field
          :let [[x y] (parse-key key)
                {:keys [mine label]} cell
                nbs   (map
                        (fn [[x y]] (get-cell x y))
                        (neighbours x y))]]
    (when (and (not= "q" label) (not mine))
      (assoc! cell :label (cond->> nbs
                            true   (filter :mine)
                            modern (remove :open)
                            true   (count)
                            true   (str))))
    (assoc! cell :solved (every? :open nbs))
    (assoc! cell :reachable (some #(and (:open %) (not (:mine %)) (not= "q" (:label %))) nbs)))
  (set! flags (->> field vals (filter :mine) (remove :open) (count)))
  (when (= 0 flags)
    (set! screen :victory)))

(defn-log load-game []
  (let [code     (re-find #"[foqFOQ]+" puzzle)
        len      (count code)
        *to-open (atom [])]
    (set! field-w (js/Math.sqrt len))
    (set! field-h (js/Math.sqrt len))
    (set! grid-w (* field-w cell-size))
    (set! grid-h (* field-h cell-size))
    (set! grid-x (-> canvas-w (- grid-w) (quot 2)))
    (set! grid-y (-> canvas-h (- 100) (- grid-h) (quot 2)))
    (set! field {})

    (dotimes [i len]
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

    (doseq [[i f] (indexed (shuffle @*to-open))]
      (set-timeout (* i 50) f))

    (update-field)
    (set! screen :game)

    ;; tools
    (.clearRect (.getContext notes "2d") 0 0 canvas-w canvas-h)
    (set! tool nil)

    (request-render)))

(defn render-text [text]
  (set! (.-font ctx) "24px sans-serif")
  (set! (.-fillStyle ctx) "#FFF")
  (set! (.-textAlign ctx) "center")
  (set! (.-textBaseline ctx) "middle")
  (.fillText ctx text (/ canvas-w 2) (/ canvas-h 2)))

(defn-log preload-images []
  (let [names    (concat
                   (for [i (range 9)]
                     (str i ".png"))
                   (for [i (range 9)]
                     (str i "_solved.png"))
                   ["q.png" "q_solved.png"
                    "closed.png" "unreachable.png" "hover.png"
                    "flagged.png" "flagged_classic.png" "flag.png"
                    "btn_retry.png" "btn_reload.png"
                    "tool_eraser.png" "tool_color1.png" "tool_color2.png" "tool_color3.png" "tool_color4.png"
                    "tool_eraser_selected.png" "tool_color1_selected.png" "tool_color2_selected.png" "tool_color3_selected.png" "tool_color4_selected.png"])
        *to-load (atom (count names))]
    (doseq [name names
            :let [img (js/Image.)]]
      (set! (.-onload img)
        (fn []
          (assoc! images name img)
          (when (= 0 (swap! *to-load dec))
            (load-game)
            (maybe-render))))
      (set! (.-src img) (str "i/" name)))))

(defn render-game []
  (let [[hover-x hover-y] (when (and drag-x drag-y)
                            (field-coords drag-x drag-y))]
    ;; cells
    (doseq [y (range field-w)
            x (range field-h)]
      (let [{:keys [mine open label solved reachable]} (get-cell x y)
            name (cond
                   (and (not tool) (not open) (= x hover-x) (= y hover-y)) "hover.png"
                   (and mine open)                  (if modern "flagged.png" "flagged_classic.png")
                   (and (not open) (not reachable)) "unreachable.png"
                   (not open)                       "closed.png"
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
                      (= "0" label) "#FFFFFF"
                      (= "1" label) "#FF63C1"
                      (= "2" label) "#3EA8FF"
                      (= "3" label) "#E15757"
                      (= "4" label) "#FFEC41"
                      (= "5" label) "#33FB37"
                      (= "6" label) "#6365FF"
                      (= "7" label) "#63FFF7"
                      (= "8" label) "#FFFFFF"
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
    (when-some [[l t w h flags'] (flag-area)]
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
          (-> drag-y (- margin) (- (case drag-type
                                     :mouse (quot cell-size 2)
                                     :touch cell-size)))
          sprite-size sprite-size)))

    ;; Tools
    (let [width (* (count tools) cell-size)
          left  (quot (- canvas-w width) 2)]
      (doseq [[i t] (indexed tools)
              :let [x   (+ left (* i cell-size))
                    y   (- canvas-h sprite-size 35)
                    img (get images (str "tool_" t (if (= t tool) "_selected" "") ".png"))]]
        (.drawImage ctx img (- x margin) (- y margin) sprite-size sprite-size)))

    ;; level name
    (let [name (re-find #"^[^ ]+" puzzle)]
      (set! (.-font ctx) "12px sans-serif")
      (set! (.-textAlign ctx) "left")
      (set! (.-fillStyle ctx) "#284E6D")
      (.fillText ctx name 10 35))))

(defn open-cell [gx gy]
  (let [key                 (key gx gy)
        {:keys [mine open]} (get-cell gx gy)]
    (cond
      open  :noop
      mine  (set! screen :game-over)
      :else (do
              (assoc! (get field key) :open true)
              (update-field)))
    (request-render)))

(defn flag-cell [gx gy]
  (let [key                 (key gx gy)
        {:keys [mine open]} (get-cell gx gy)]
    (cond
      open  :noop
      mine  (do
              (assoc! (get field key) :open true)
              (update-field))
      :else (set! screen :game-over))
    (request-render)))

(defn-log on-click [x y action]
  (cond+
    (inside? x y grid-x grid-y grid-w grid-h)
    (let [[gx gy]             (field-coords x y)
          key                 (key gx gy)
          {:keys [mine open]} (get-cell gx gy)]
      #_(println "on-grid-click" gx gy action mine open)
      (case action
        :primary   (open-cell gx gy)
        :secondary (flag-cell gx gy)))

    :let [toolbox-w (* (count tools) cell-size)
          toolbox-x (quot (- canvas-w toolbox-w) 2)]
    (inside? x y toolbox-x (- canvas-h sprite-size 35) toolbox-w cell-size)
    (let [i (quot (- x toolbox-x) cell-size)
          t (nth tools i)]
      (if (= tool t)
        (set! tool nil)
        (set! tool t)))

    (not= :primary action)
    :noop

    ;; retry button
    (inside? x y (- canvas-w 150) 25 50 50)
    (load-game puzzle)

    ;; reload button
    (inside? x y (- canvas-w 75) 25 50 50)
    (.reload (.-location js/window))

    :else
    (do
      (set! outline-x nil)
      (set! outline-y nil))))

(defn on-resize []
  (let [w      (.-innerWidth js/window)
        h      (.-innerHeight js/window)
        dw     (* w dpi)
        dh     (* h dpi)
        scales [4 3 2 1.75 1.5 1.25 1 0.75 0.6666667 0.5 0.3333333 0.25]
        sx     (some #(when (<= (* % 560) dw) %) scales)
        sy     (some #(when (<= (* % 900) dh) %) scales)
        scale  (min sx sy)]
    (set! canvas-w (-> dw (/ scale) (/ 2) js/Math.floor (* 2)))
    (set! canvas-h (-> dh (/ scale) (/ 2) js/Math.floor (* 2)))
    (set! canvas-scale scale)

    (set! (.-width canvas) dw)
    (set! (.-height canvas) dh)
    (.resetTransform ctx)
    (.scale ctx canvas-scale canvas-scale)

    (set! (.-width notes) dw)
    (set! (.-height notes) dh)
    (.resetTransform notes-ctx)
    (.scale notes-ctx canvas-scale canvas-scale)

    (set! grid-x (-> canvas-w (- grid-w) (quot 2)))
    (set! grid-y (-> canvas-h (- 100) (- grid-h) (quot 2)))
    (request-render)))

(defn-log render []
  (.clearRect ctx 0 0 canvas-w canvas-h)

  ;; render screen
  (case screen
    :loading   (render-text "Loading resources...")
    :game      (render-game)
    :game-over (render-text "Game Over")
    :victory   (render-text "Congratulations! You won!"))

  ;; buttons
  (when-some [img (get images "btn_retry.png")]
    (.drawImage ctx img  (- canvas-w 175) 0 sprite-size sprite-size))
  (when-some [img (get images "btn_reload.png")]
    (.drawImage ctx img  (- canvas-w 100) 0 sprite-size sprite-size))

  ;; viewport size
  (set! (.-font ctx) "12px sans-serif")
  (set! (.-fillStyle ctx) "#284E6D")
  (.fillText ctx (str canvas-w "×" canvas-h "@" canvas-scale) 10 20))

(defn maybe-render []
  (when render-requested
    (set! render-requested false)
    (render)))

(defn on-load [_e]
  ;; Prevent all scrolling and rubber band effect
  (.addEventListener js/document "touchmove"
    (fn [e]
      (.preventDefault e))
    #js {:passive false})

  ;; Prevent double-tap zoom
  (let [*last-touch-end (atom 0)]
    (.addEventListener js/document "touchend"
      (fn [e]
        (let [now (js/Date.now)]
          (when (<= (- now @*last-touch-end) 300)
            (.preventDefault e))
          (reset! *last-touch-end now)))))

  ;; Prevent pinch zoom
  (.addEventListener js/document "gesturestart"
    (fn [e]
      (.preventDefault e)))

  ;; Resize listener
  (add-event-listener js/window "resize" on-resize)

  ;; Setup canvas
  (set! canvas     (.querySelector js/document "#canvas"))
  (set! ctx (.getContext canvas "2d"))
  (set! notes      (.querySelector js/document "#notes"))
  (set! notes-ctx  (.getContext notes "2d"))
  (on-resize)

  ;; Touch/click/drag listeners
  (let [on-start   (fn [x y type]
                     (set! drag-type type)
                     (set! drag-x x)
                     (set! drag-y y)
                     (when tool
                         (set! (.-lineWidth notes-ctx) (case tool :eraser 30 6))
                         (set! (.-strokeStyle notes-ctx) (get tool-colors tool))
                         (set! (.-lineCap notes-ctx) "round")
                         (set! (.-lineJoin notes-ctx) "round")
                         (set! (.-globalCompositeOperation notes-ctx) (case tool :eraser "destination-out" "source-over"))
                         (aset tool-points 0 nil)
                         (aset tool-points 1 [drag-x drag-y]))

                     (let [[l t w h] (flag-area)]
                       (when (inside? x y l t w h margin)
                         (set! dragging-flag true)
                         (set! tool nil)))
                     (request-render))

        on-move    (fn [x y]
                     (when (and drag-type tool drag-x drag-y)
                       (let [[x0 y0] (aget tool-points 0)
                             [x1 y1] (aget tool-points 1)
                             [x  y ] [x y]]
                         (when (and x1 y1 (>= (js/Math.hypot (- x x1) (- y y1)) 15))
                           (when (and (nil? x0) (nil? y0) x1 y1)
                             (.beginPath notes-ctx)
                             (.moveTo notes-ctx x1 y1))
                           (when (and x0 y0 x1 y1)
                             (.quadraticCurveTo notes-ctx x1 y1 (/ (+ x1 x) 2) (/ (+ y1 y) 2))
                             #_(.lineTo notes-ctx x y)
                             (.stroke notes-ctx))
                           (aset tool-points 0 [x1 y1])
                           (aset tool-points 1 [x y]))))
                     (set! drag-x x)
                     (set! drag-y y)
                     (request-render))

        on-end     (fn [x y action]
                     (set! drag-type nil)
                     (set! drag-x nil)
                     (set! drag-y nil)
                     (let [[gx gy] (field-coords x y)]
                       (cond
                         ;; drop flag
                         dragging-flag
                         (do
                           (when-some [[gx gy] (field-coords x y)]
                             (let [{:keys [open]} (get-cell gx gy)]
                               (when-not open
                                 (flag-cell gx gy))))
                           (set! dragging-flag false))

                         ;; end outside field
                         (not (and gx gy))
                         (on-click x y action)

                         ;; second click on outlined cell
                         (and (= gx outline-x) (= gy outline-y))
                         (do
                           (set! outline-x nil)
                           (set! outline-y nil))

                         ;; click on open cell
                         (:open (get-cell gx gy))
                         (do
                           (set! outline-x gx)
                           (set! outline-y gy))

                         ;; click on closed cell
                         :else
                         (on-click x y action)))
                     (request-render))]

    ;; Touch events
    (add-event-listener canvas "touchstart"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords (aget (.-touches e) 0))]
          (on-start x y :touch))))

    (add-event-listener canvas "touchmove"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords (aget (.-touches e) 0))]
          (on-move x y))))

    (add-event-listener canvas "touchend"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords (aget (.-changedTouches e) 0))]
          (on-end x y :primary))))

    ;; Mouse events
    (add-event-listener canvas "mousedown"
      (fn [e]
        (when (= 0 (.-button e))
          (let [[x y] (rel-coords e)]
            (on-start x y :mouse)))))

    (add-event-listener canvas "mousemove"
      (fn [e]
        (when (= 0 (.-button e))
          (let [[x y] (rel-coords e)]
            (on-move x y)))))

    (add-event-listener canvas "mouseup"
      (fn [e]
        (when (= 0 (.-button e))
          (let [[x y] (rel-coords e)]
            (on-end x y :primary)))))

    (add-event-listener canvas "contextmenu"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords e)]
          (on-click x y :secondary)))))

  ;; Render
  (set! puzzle
    (->> puzzles/puzzles
      (filter #(re-find #"8x8" %))
      (rand-nth))
    #_"OffqqoofffqoooqfoOfOffOfo"
    #_"ffoqfffOfooqQfoOoqOOqOqffffOfoqoffqO"
    #_"qffqfOfffoqoffOOfOoqOfooofqffffofOqOfofoOfqfqqooo"
    #_"fOfffOffffofqffqffoqoqoofoooOofooofofOOoqfofqfoqq"
    #_"OffqooOqqffqfooqOqOffofoqofqoOOffffOffqfqooofoOoOofqffoqfffffOfq"
    #_"OFFQOOOQQFFQFOOQOQOFFOFOQofqoOOffffOffqfqooofoOoOofqffoqfffffOfq"
    #_"OffooOfOqffOqfoqqOOqfOfOqqofOqfoffqoooofQqofofoffqfooqfqfffffoff"
    #_"OffOfofqofqqfQfOOqoOoqOfofOfoffffffqoqoqOfOfffqooqOOfqfOfOfQfoqf")
  (preload-images))

(add-event-listener js/window "load" on-load)
