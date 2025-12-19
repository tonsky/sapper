(ns game
  (:require
   [clojure.string :as str]))

(def canvas nil)
(def canvas-w 1000)
(def canvas-h 700)
(def cell-size 70)
(def sprite-size 100)
(def margin (-> sprite-size (- cell-size) (/ 2)))
(def pixel-ratio (or (.-devicePixelRatio js/window) 1))
(def *field (atom nil))
(def field-w 0)
(def field-h 0)
(def grid-x 0)
(def grid-y 0)
(def grid-w 0)
(def grid-h 0)

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

(defn cell [field x y]
  (get field (key x y)))

(defn parse-key [key]
  (let [[x y] (str/split key ",")]
    [(js/Number x) (js/Number y)]))

(defn inside? [x y l t w h]
  (and
    (<= l x)
    (< x (+ l w))
    (<= t y)
    (< y (+ t h))))

(defn update-reachability [field]
  (reduce-kv
    (fn [field key value]
      (let [[x y] (parse-key key)
            {:keys [mine open label]} value
            nbs   (->> (neighbours x y)
                    (map (fn [[x y]] (cell field x y))))]
        (update field key assoc
          :solved    (every? :open nbs)
          :reachable (some #(and (:open %) (not (:mine %)) (not= "q" (:label %))) nbs))))
    field
    field))

(defn load-game [s]
  (set! field-w (js/Math.sqrt (count s)))
  (set! field-h (js/Math.sqrt (count s)))
  (set! grid-w (* field-w cell-size))
  (set! grid-h (* field-h cell-size))
  (set! grid-x (-> canvas-w (- grid-w) (quot 2)))
  (set! grid-y (-> canvas-h (- grid-h) (quot 2)))
  (reset! *field
    (into {}
      (for [i (range (count s))
            :let [x  (mod i field-w)
                  y  (quot i field-w)
                  ch (nth s i)]]
        [(key x y)
         (case ch
           "f" {:mine true,  :open false}
           "o" {:mine false, :open false}
           "O" {:mine false, :open true}
           "q" {:mine false, :open false, :label "q"}
           "Q" {:mine false, :open true,  :label "q"})])))
  (swap! *field
    (fn [field]
      (reduce-kv
        (fn [field key value]
          (let [[x y] (parse-key key)
                {:keys [mine open label]} value
                nbs   (->> (neighbours x y)
                        (map (fn [[x y]] (cell field x y))))
                cnt   (->> nbs (filter :mine) (count))]
            (update field key assoc
              :label (or label (str cnt)))))
        field
        field)))
  (swap! *field update-reachability))

(def *images
  (atom {}))

(def *mode
  (atom :open))

(def mode-buttons
  {:flag {:top 60}
   :open {:top 145}})

(defn render [f & args]
  (let [ctx (.getContext canvas "2d")]
    (.save ctx)
    (.scale ctx pixel-ratio pixel-ratio)
    (.clearRect ctx 0 0 canvas-w canvas-h)
    (apply f ctx args)

    ;; viewport size
    (set! (.-font ctx) "12px sans-serif")
    (set! (.-fillStyle ctx) "#284E6D")
    (.fillText ctx (str (.-innerWidth js/window) "×" (.-innerHeight js/window)) 0 10)

    (.restore ctx)))

(defn render-loading [ctx]
  (set! (.-font ctx) "24px sans-serif")
  (set! (.-fillStyle ctx) "#FFF")
  (set! (.-textAlign ctx) "center")
  (set! (.-textBaseline ctx) "middle")
  (.fillText ctx "Loading resources..." (/ canvas-w 2) (/ canvas-h 2)))

(defn preload-images []
  (let [names    (concat
                   (for [i (range 9)]
                     (str i ".png"))
                   (for [i (range 9)]
                     (str i "_solved.png"))
                   ["q.png" "q_solved.png"
                    "closed.png" "closed_unreachable.png"
                    "flag.png"
                    "btn_flag.png" "btn_flag_pressed.png"
                    "btn_open.png" "btn_open_pressed.png"])
        *to-load (atom (count names))]
    (doseq [name names
            :let [img (js/Image.)]]
      (set! (.-onload img)
        (fn []
          (swap! *images assoc name img)
          (when (= 0 (swap! *to-load dec))
            (render render-game))))
      (set! (.-src img) (str "i/" name)))))

(defn render-game [ctx]
  ;; Render cells
  (doseq [y (range field-w)
          x (range field-h)]
    (let [{:keys [mine open label solved reachable]} (cell @*field x y)
          name (cond
                 (and mine open)                  "flag.png"
                 (and (not open) (not reachable)) "closed_unreachable.png"
                 (not open)                       "closed.png"
                 (and open solved)                (str label "_solved.png")
                 :else                            (str label ".png"))
          img  (get @*images name)
          px   (-> (* x cell-size) (+ grid-x) (- margin))
          py   (-> (* y cell-size) (+ grid-y) (- margin))]
      (.drawImage ctx img px py sprite-size sprite-size)))

  ;; Buttons
  (doseq [[mode {:keys [top]}] mode-buttons
          :let [pressed? (= mode @*mode)
                name     (if pressed?
                           (str "btn_" mode "_pressed.png")
                           (str "btn_" mode ".png"))
                img      (get @*images name)]]
    (.drawImage ctx img 0 (- top margin) sprite-size sprite-size)))

(defn on-grid-click [gx gy]
  (let [mode                @*mode
        key                 (key gx gy)
        {:keys [mine open]} (cell @*field gx gy)]
    (cond
      open
      :noop

      (and (= :flag mode) mine)
      (swap! *field #(-> %
                       (update key assoc :open true)
                       update-reachability))

      (and (= :open mode) (not mine))
      (swap! *field #(-> %
                       (update key assoc :open true)
                       update-reachability))

      :else
      (println "clicked" mode gx gy mine open))))


(defn on-click [x y]
  (cond
    (inside? x y grid-x grid-y grid-w grid-h)
    (let [gx (quot (- x grid-x) cell-size)
          gy (quot (- y grid-y) cell-size)]
      (on-grid-click gx gy))

    (<= x cell-size)
    (doseq [[mode {:keys [top]}] mode-buttons
            :when (inside? x y 0 top cell-size cell-size)]
      (reset! *mode mode))))

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

  ;; Setup canvas
  (set! canvas (.querySelector js/document "canvas"))
  (set! (.-width canvas) (* canvas-w pixel-ratio))
  (set! (.-height canvas) (* canvas-h pixel-ratio))

  ;; Click listener
  (.addEventListener canvas "click"
    (fn [e]
      (let [rect  (.getBoundingClientRect canvas)
            x     (- (.-clientX e) (.-left rect))
            y     (- (.-clientY e) (.-top rect))]
        (on-click x y))))

  ;; Render
  (render render-loading)
  (preload-images)
  (load-game
    "OffqooOqqffqfooqOqOffofoqofqoOOffffOffqfqooofoOoOofqffoqfffffOfq"
    #_"OffooOfOqffOqfoqqOOqfOfOqqofOqfoffqoooofQqofofoffqfooqfqfffffoff"
    #_"OffOfofqofqqfQfOOqoOoqOfofOfoffffffqoqoqOfOfffqooqOOfqfOfOfQfoqf")

  (add-watch *field :render
    (fn [_ _ _ _]
      (render render-game)))

  (add-watch *mode :render
    (fn [_ _ _ _]
      (render render-game))))

(.addEventListener js/window "load" on-load)
