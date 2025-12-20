(ns sapper.game
  (:require
   [clojure.string :as str]
   [sapper.puzzles :as puzzles])
  (:require-macros
   [sapper.macros :refer [defn-log]]))

(def canvas nil)
(def canvas-w 0)
(def canvas-h 0)
(def dpi (or (.-devicePixelRatio js/window) 1))
(def canvas-scale 1)
(def cell-size 70)
(def sprite-size 100)
(def margin (-> cell-size (- sprite-size) (/ 2)))
(def field {})
(def flags 0)
(def field-w 0)
(def field-h 0)
(def grid-x 0)
(def grid-y 0)
(def grid-w 0)
(def grid-h 0)
(def images {})
(def screen :loading)
(def render-requested false)

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

(defn inside? [x y l t w h]
  (and
    (<= l x)
    (< x (+ l w))
    (<= t y)
    (< y (+ t h))))

(defn index [seq]
  (map vector (range) seq))

(defn-log update-field []
  (doseq [[key cell] field
          :let [[x y] (parse-key key)
                {:keys [mine label]} cell
                nbs   (map
                        (fn [[x y]] (get-cell x y))
                        (neighbours x y))]]
    (when (and (not= "q" label) (not mine))
      (assoc! cell :label (->> nbs (filter :mine) (remove :open) (count) str)))
    (assoc! cell :solved (every? :open nbs))
    (assoc! cell :reachable (some #(and (:open %) (not (:mine %)) (not= "q" (:label %))) nbs)))
  (set! flags (->> field vals (filter :mine) (remove :open) (count))))

(defn-log load-game [s]
  (set! field-w (js/Math.sqrt (count s)))
  (set! field-h (js/Math.sqrt (count s)))
  (set! grid-w (* field-w cell-size))
  (set! grid-h (* field-h cell-size))
  (set! grid-x (-> canvas-w (- grid-w) (quot 2)))
  (set! grid-y (-> canvas-h (- grid-h) (quot 2)))
  (let [*to-open (atom [])]
    (set! field {})
    (dotimes [i (count s)]
      (let [x  (mod i field-w)
                  y  (quot i field-w)
                  ch (nth s i)]
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
    (doseq [[i f] (index (shuffle @*to-open))]
      (set-timeout (* i 33.3333333) f)))

  (update-field)

  (request-render))

(defn render-text [ctx text]
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
                    "closed.png" "closed_unreachable.png"
                    "flagged.png" "flag.png"
                    "btn_reload.png"])
        *to-load (atom (count names))]
    (doseq [name names
            :let [img (js/Image.)]]
      (set! (.-onload img)
        (fn []
          (assoc! images name img)
          (when (= 0 (swap! *to-load dec))
            (set! screen :game)
            (request-render)
            (maybe-render))))
      (set! (.-src img) (str "i/" name)))))

(defn render-game [ctx]
  ;; Render cells
  (doseq [y (range field-w)
          x (range field-h)]
    (let [{:keys [mine open label solved reachable]} (get-cell x y)
          name (cond
                 (and mine open)                  "flagged.png"
                 (and (not open) (not reachable)) "closed_unreachable.png"
                 (not open)                       "closed.png"
                 (and open solved)                (str label "_solved.png")
                 (= "q" label)                    "q_solved.png"
                 :else                            (str label ".png"))
          img  (get images name)
          px   (-> (* x cell-size) (+ grid-x margin))
          py   (-> (* y cell-size) (+ grid-y margin))]
      (.drawImage ctx img px py sprite-size sprite-size)))

  ;; Flag counter
  (when (pos? flags)
    (let [flag-img  (get images "flag.png")
          offset    20
          total-w   (+ (* (dec flags) offset) cell-size)
          rect-x    (-> canvas-w (- total-w) (quot 2))
          rect-y    (+ grid-y grid-h 30)]
      (set! (.-fillStyle ctx) "#082848")
      (.beginPath ctx)
      (.roundRect ctx rect-x rect-y total-w cell-size 6)
      (.fill ctx)
      (dotimes [i flags]
        (.drawImage ctx flag-img (+ rect-x (* i offset) margin) (+ rect-y margin) sprite-size sprite-size))))

  #_(doseq [[mode {:keys [top]}] mode-buttons
            :let [pressed? (= mode @*mode)
                  name     (if pressed?
                             (str "btn_" mode "_pressed.png")
                             (str "btn_" mode ".png"))
                  img      (get images name)]]
      (.drawImage ctx img 0 (- top margin) sprite-size sprite-size)))

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
  (cond
    (inside? x y grid-x grid-y grid-w grid-h)
    (let [gx (quot (- x grid-x) cell-size)
          gy (quot (- y grid-y) cell-size)
          key                 (key gx gy)
          {:keys [mine open]} (get-cell gx gy)]
      (println "on-grid-click" gx gy action mine open)
      (case action
        :primary   (open-cell gx gy)
        :secondary (flag-cell gx gy)))

    (not= :primary action)
    :noop

    ;; reload button
    (inside? x y (- canvas-w 75) 25 50 50)
    (.reload (.-location js/window))

    #_#_(<= x cell-size)
    (doseq [[mode {:keys [top]}] mode-buttons
            :when (inside? x y 0 top cell-size cell-size)]
      (reset! *mode mode))))

(defn on-resize []
  (let [w      (.-innerWidth js/window)
        h      (.-innerHeight js/window)
        dw     (* w dpi)
        dh     (* h dpi)
        scales [4 3 2 1.75 1.5 1.25 1 0.75 0.6666667 0.5 0.3333333 0.25]
        sx     (some #(when (<= (* % 700) dw) %) scales)
        sy     (some #(when (<= (* % 900) dh) %) scales)
        scale  (min sx sy)]
    (set! canvas-w (-> dw (/ scale) (/ 2) js/Math.floor (* 2)))
    (set! canvas-h (-> dh (/ scale) (/ 2) js/Math.floor (* 2)))
    (set! (.-width canvas) dw)
    (set! (.-height canvas) dh)
    (set! canvas-scale scale)
    (set! grid-x (-> canvas-w (- grid-w) (quot 2)))
    (set! grid-y (-> canvas-h (- grid-h) (quot 2)))
    (request-render)))

(defn-log render []
  (let [ctx (.getContext canvas "2d")]
    (.save ctx)
    (.scale ctx canvas-scale canvas-scale)
    (.clearRect ctx 0 0 canvas-w canvas-h)

    ;; render screen
    (case screen
      :loading   (render-text ctx "Loading resources...")
      :game      (render-game ctx)
      :game-over (render-text ctx "Game Over"))

    ;; buttons
    (when-some [img (get images "btn_reload.png")]
      (.drawImage ctx img  (- canvas-w 100) 0 sprite-size sprite-size))

    ;; viewport size
    (set! (.-font ctx) "12px sans-serif")
    (set! (.-fillStyle ctx) "#284E6D")
    (.fillText ctx (str canvas-w "×" canvas-h "@" canvas-scale) 10 20)

    (.restore ctx)))

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
  (set! canvas (.querySelector js/document "canvas"))
  (on-resize)

  ;; Touch/click listeners
  (let [rel-coords (fn [e]
                     (let [rect (.getBoundingClientRect canvas)
                           x    (-> (.-clientX e) (- (.-left rect)) (* dpi) (/ canvas-scale) js/Math.round)
                           y    (-> (.-clientY e) (- (.-top rect)) (* dpi) (/ canvas-scale) js/Math.round)]
                       [x y]))]
    (add-event-listener canvas "touchend"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords (aget (.-changedTouches e) 0))]
          (on-click x y :primary))))
    (add-event-listener canvas "click"
      (fn [e]
        (let [[x y] (rel-coords e)]
          (on-click x y :primary))))
    (add-event-listener canvas "contextmenu"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords e)]
          (on-click x y :secondary)))))

  ;; Render
  (preload-images)
  (load-game
    #_(rand-nth puzzles/eights)
    #_"OffqqoofffqoooqfoOfOffOfo"
    #_"ffoqfffOfooqQfoOoqOOqOqffffOfoqoffqO"
    #_"qffqfOfffoqoffOOfOoqOfooofqffffofOqOfofoOfqfqqooo"
    #_"fOfffOffffofqffqffoqoqoofoooOofooofofOOoqfofqfoqq"
    "OffqooOqqffqfooqOqOffofoqofqoOOffffOffqfqooofoOoOofqffoqfffffOfq"
    #_"OFFQOOOQQFFQFOOQOQOFFOFOQofqoOOffffOffqfqooofoOoOofqffoqfffffOfq"
    #_"OffooOfOqffOqfoqqOOqfOfOqqofOqfoffqoooofQqofofoffqfooqfqfffffoff"
    #_"OffOfofqofqqfQfOOqoOoqOfofOfoffffffqoqoqOfOfffqooqOOfqfOfOfQfoqf")

  (maybe-render))

  (.addEventListener js/window "load" on-load)
