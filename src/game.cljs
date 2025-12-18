(ns game
  (:require
   [clojure.string :as str]))

(def canvas
  nil)

(def canvas-w
  1000)

(def canvas-h
  700)

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
                   (assoc game key (assoc value
                     :label     (or label (str cnt))
                     :solved    (every? :open nbs)
                     :reachable (some #(and (:open %) (not (:mine %))) nbs)))))
               game
               game)]
    game))

(def *game
  (atom
    (parse-puzzle
      #_"OffqooOqqffqfooqOqOffofoqofqoOOffffOffqfqooofoOoOofqffoqfffffOfq"
      "OffooOfOqffOqfoqqOOqfOfOqqofOqfoffqoooofQqofofoffqfooqfqfffffoff")))

(def *images
  (atom {}))

(defn load-image [name]
  (or
    (get @*images name)
    (let [img (js/Image.)]
      (set! (.-src img) (str "i/" name))
      (swap! *images assoc name img)
      img)))

(defn render [game]
  (let [ctx (.getContext canvas "2d")
        game-w 8
        game-h 8
        grid-x (/ (- canvas-w (* 70 game-w)) 2)
        grid-y (/ (- canvas-h (* 70 game-h)) 2)]
    (.save ctx)
    (.scale ctx pixel-ratio pixel-ratio)

    (.clearRect ctx 0 0 canvas-w canvas-h)

    ;; Render cells
    (doseq [y (range game-w)
            x (range game-h)]
        (let [key (str x "," y)
              cell (get game key)
              {:keys [mine open label solved reachable]} cell
              _ (println x y mine open label solved)
              img-name (cond
                         (and mine open)                  "flag.png"
                         (and (not open) (not reachable)) "closed_unreachable.png"
                         (not open)                       "closed.png"
                         (and open solved)                (str label "_solved.png")
                         :else                            (str label ".png"))
              img (load-image img-name)
              px (+ grid-x (* x 70))
              py (+ grid-y (* y 70))]
          (.drawImage ctx img (- px 15) (- py 15) 100 100)))

    ;; viewport size
    (set! (.-font ctx) "12px sans-serif")
    (set! (.-fillStyle ctx) "#FFF")
    (.fillText ctx (str (.-innerWidth js/window) "×" (.-innerHeight js/window)) 0 10)

    (.restore ctx)))

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

  (render @*game)
  (js/setTimeout #(render @*game) 100))

(.addEventListener js/window "load" on-load)
