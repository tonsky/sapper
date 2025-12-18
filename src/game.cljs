(ns game
  (:require
   [clojure.string :as str]))

(def canvas
  nil)

(def canvas-w
  1000)

(def canvas-h
  700)

(def cell-size
  70)

(def sprite-size
  100)

(def pixel-ratio
  (or (.-devicePixelRatio js/window) 1))

(defn neighbours [x y w h]
  (concat
    (when (and (> x 0) (> y 0))
      [[(dec x) (dec y)]])
    (when (> x 0)
      [[(dec x)      y ]])
    (when (and (> x 0) (< y (dec h)))
      [[(dec x) (inc y)]])
    (when (> y 0)
      [[     x  (dec y)]])
    (when (< y (dec h))
      [[     x  (inc y)]])
    (when (and (< x (dec w)) (> y 0))
      [[(inc x) (dec y)]])
    (when (< x (dec w))
      [[(inc x)      y ]])
    (when (and (< x (dec w)) (< y (dec h)))
      [[(inc x) (inc y)]])))

(defn parse-puzzle [s]
  (let [size (js/Math.sqrt (count s))
        game {}
        game (into {}
               (for [i (range (count s))
                     :let [x  (mod i size)
                           y  (quot i size)
                           ch (nth s i)]]
                 [(str x "," y)
                  (case ch
                    "f" {:mine true,  :open false}
                    "o" {:mine false, :open false}
                    "O" {:mine false, :open true}
                    "q" {:mine false, :open false, :label "q"}
                    "Q" {:mine false, :open true,  :label "q"})]))
        game (reduce-kv
               (fn [game key value]
                 (let [[x y] (map js/Number (str/split key ","))
                       {:keys [mine open label]} value
                       nbs    (->> (neighbours x y size size)
                                (map (fn [[x y]] (get game (str x "," y)))))
                       cnt    (->> nbs (filter :mine) (count))]
                   (update game key assoc
                     :label     (or label (str cnt))
                     :solved    (every? :open nbs)
                     :reachable (some #(and (:open %) (not (:mine %)) (not= "q" (:label %))) nbs))))
               game
               game)]
    game))

(def *game
  (atom
    (parse-puzzle
      #_"OffqooOqqffqfooqOqOffofoqofqoOOffffOffqfqooofoOoOofqffoqfffffOfq"
      "OffooOfOqffOqfoqqOOqfOfOqqofOqfoffqoooofQqofofoffqfooqfqfffffoff"
      #_"OffOfofqofqqfQfOOqoOoqOfofOfoffffffqoqoqOfOfffqooqOOfqfOfOfQfoqf")))

(def *images
  (atom {}))

(defn render [f & args]
  (let [ctx (.getContext canvas "2d")]
    (.save ctx)
    (.scale ctx pixel-ratio pixel-ratio)
    (.clearRect ctx 0 0 canvas-w canvas-h)
    (apply f ctx args)

    ;; viewport size
    (set! (.-font ctx) "12px sans-serif")
    (set! (.-fillStyle ctx) "#FFF")
    (.fillText ctx (str (.-innerWidth js/window) "×" (.-innerHeight js/window)) 0 10)

    (.restore ctx)))

(defn render-loading [ctx]
  (set! (.-font ctx) "24px sans-serif")
  (set! (.-fillStyle ctx) "#000")
  (set! (.-textAlign ctx) "center")
  (set! (.-textBaseline ctx) "middle")
  (.fillText ctx "Loading resources..." (/ canvas-w 2) (/ canvas-h 2)))

(defn preload-images [on-complete]
  (let [names    (concat
                   (for [i (range 9)]
                     (str i ".png"))
                   (for [i (range 9)]
                     (str i "_solved.png"))
                   ["q.png" "q_solved.png" "closed.png" "closed_unreachable.png" "flag.png"])
        *to-load (atom (count names))]
    (doseq [name names
            :let [img (js/Image.)]]
      (set! (.-onload img)
        (fn []
          (swap! *images assoc name img)
          (when (= 0 (swap! *to-load dec))
            (on-complete))))
      (set! (.-src img) (str "i/" name)))))

(defn render-game [ctx game]
  (let [game-w (js/Math.sqrt (count game))
        game-h game-w
        grid-x (/ (- canvas-w (* cell-size game-w)) 2)
        grid-y (/ (- canvas-h (* cell-size game-h)) 2)]
    ;; Render cells
    (doseq [y (range game-w)
            x (range game-h)]
      (let [key (str x "," y)
            cell (get game key)
            {:keys [mine open label solved reachable]} cell
            img-name (cond
                       (and mine open)                  "flag.png"
                       (and (not open) (not reachable)) "closed_unreachable.png"
                       (not open)                       "closed.png"
                       (and open solved)                (str label "_solved.png")
                       :else                            (str label ".png"))
            img (get @*images img-name)
            margin (- (/ (- sprite-size cell-size) 2))
            px (+ (* x cell-size) grid-x margin)
            py (+ (* y cell-size) grid-y margin)]
        (.drawImage ctx img px py sprite-size sprite-size)))))

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

  ;; Draw loading screen
  (render render-loading)

  ;; Preload all images, then render the game
  (preload-images #(render render-game @*game)))

(.addEventListener js/window "load" on-load)
