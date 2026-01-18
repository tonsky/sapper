(ns sapper.game
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [canvas ctx notes-ctx canvas-w canvas-h canvas-scale dpi images safe-area sprite-size]]
   [sapper.level-select :as level-select]
   [sapper.solver :as solver])
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
(def exploded-x nil)
(def exploded-y nil)

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
(def anim-length 300)

(def auto-open-queue [])
(def auto-open-timer nil)
(def auto-open-dt 32.333333)

(declare open-cell flag-cell maybe-auto-open)

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

(defn mine-count [x y]
  (->> (neighbours x y)
    (filter (fn [[x y]] (:mine (get-cell x y))))
    (count)))

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

(defn flag-area [flags']
  (let [flags' (or flags' (cond-> flags dragging-flag dec))]
    (when (pos? flags')
      (let [max-flags  26
            flag-gap   (+ 15 (-> max-flags (- flags') (/ max-flags) (* 10) (max 0)))
            total-w    (+ (* (dec flags') flag-gap) cell-size -5)
            number-w   (cond
                         (>= flags' 10) 50
                         (>= flags' 1)  25
                         :else          0)
            combined-w (+ total-w 15 number-w)
            left       (-> canvas-w (- combined-w) (quot 2))
            top        (+ grid-y grid-h 30)]
        [left
         top
         combined-w
         cell-size
         flags'
         flag-gap
         (+ left total-w)]))))

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
       {:l 100 :t 25 :w 50 :h 50 :icon "btn_reload.png" :on-click core/reload}
       {:l (- width 75) :t 25 :w 50 :h 50 :icon "btn_random.png" :on-click #(core/load-random-puzzle (:type puzzle))}])

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
    (set! exploded-x nil)
    (set! exploded-y nil)
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
  (let [anim-progress     (core/clamp (/ (- (js/Date.now) anim-start) anim-length) 0 1)
        [hover-x hover-y] (when (and drag-x drag-y)
                            (field-coords drag-x drag-y))
        id                (:id puzzle)
        rng               (core/make-rng (js/parseInt (str/join (re-seq #"\d" id))))]
    ;; level name
    (set! (.-font ctx) "10px font")
    (set! (.-textAlign ctx) "left")
    (set! (.-fillStyle ctx) "#477397")
    (.fillText ctx id 13 47)

    ;; buttons
    (doseq [b buttons]
      (core/button-render b))

    ;; cells
    (doseq [y (range field-w)
            x (range field-h)]
      (let [_              (core/advance-rng rng)
            {:keys [mine flagged open label solved] :as cell} (get-cell x y)
            err            (and label (or (str/starts-with? label "-")
                                        (str/starts-with? label "error_")))
            name           (cond
                             (and (= :game-over phase) (= x exploded-x) (= y exploded-y)) nil
                             (and (= :game-over phase) mine (not flagged)) (str "mine_" (-> (core/random rng) (* 5) js/Math.floor) ".png")
                             (and (= :game-over phase) (not mine) flagged) "flagged_wrong.png"
                             (and (not tool) (not open) (not flagged) (= x hover-x) (= y hover-y)) "hover.png"
                             flagged                          "flagged.png"
                             (not open)                       "closed.png"
                             err                              (str label ".png")
                             (and open solved)                (str label "_solved.png")
                             :else                            (str label ".png"))
            img            (get images name)
            anim-stagger   (-> (+ (* y field-w) x) (/ field-w field-h))
            anim-progress' (-> anim-progress (- (* anim-stagger 0.75)) (* 4) (core/clamp 0 1))
            anim-travel    (/ (core/dist [0 0] [x y]) (core/dist [0 0] [field-w field-h]))
            anim-offset-x  (* (- 1 anim-progress') (* anim-travel 30))
            anim-offset-y  (* (- 1 anim-progress') (* anim-travel 50))
            px             (-> (* x cell-size) (+ grid-x) (- margin) (+ anim-offset-x))
            py             (-> (* y cell-size) (+ grid-y) (- margin) (+ anim-offset-y))]
        (when name
          (set! (.-globalAlpha ctx) anim-progress')
          (.drawImage ctx img px py sprite-size sprite-size))))
    (set! (.-globalAlpha ctx) 1)

    ;; explosion
    (when (and (= :game-over phase) exploded-x exploded-y)
      (let [img (get images "explosion.png")
            px  (-> (* exploded-x cell-size) (+ grid-x) (- margin))
            py  (-> (* exploded-y cell-size) (+ grid-y) (- margin))]
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
    (let [[fl ft fw fh _ fg fnl] (flag-area flags)
          over?                  (and dragging-flag drag-x drag-y (core/inside? drag-x drag-y fl ft fw fh))
          area                   (if over? [fl ft fw fh flags fg fnl] (flag-area))]
      (when-some [[l t w h visible-flags flag-gap number-left] area]
        (let [flag-img  (get images "flag.png")
              hover-idx (when over?
                          (-> drag-x (- l) (- 20) (quot flag-gap) (core/clamp 0 (dec visible-flags))))]
          (set! (.-fillStyle ctx) "#082848")
          (.beginPath ctx)
          (.roundRect ctx l t w h 6)
          (.fill ctx)
          (dotimes [idx visible-flags]
            (when (not= hover-idx idx)
              (.drawImage ctx flag-img
                (-> l (+ (* idx flag-gap)) (- margin))
                (- t margin)
                sprite-size sprite-size)))
          ;; Flag count
          (set! (.-font ctx) "bold 42px font")
          (set! (.-textAlign ctx) "left")
          (set! (.-textBaseline ctx) "middle")
          (set! (.-fillStyle ctx) "#FA8787")
          (.fillText ctx (str visible-flags) number-left (+ t (quot h 2))))))

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
      (doseq [[dx dy color] [[1 2 "#00000080"] [0 0 nil]]
              stroke notes
              :let [t         (:tool stroke)
                    points    (:points stroke)
                    nth-point #(let [[x y] (aget points %)]
                                 [(+ x sa-x dx) (+ y sa-y dy)])]]
        (when (seq points)
          (set! (.-lineWidth notes-ctx) (case t :eraser 40 6))
          (set! (.-strokeStyle notes-ctx) (or color (get tool-colors t) "#000"))
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
      (set! (.-fillStyle ctx) "#07294798")
      (.fillRect ctx 0 (+ grid-y (quot (- grid-h 90) 2)) canvas-w 90)
      (set! (.-font ctx) "bold 40px font")
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (set! (.-fillStyle ctx) "#FFF")
      (.fillText ctx (case phase :game-over "Game Over :(" :victory "Victory!")
        (+ grid-x (quot grid-w 2)) (+ grid-y (quot grid-h 2))))

    ;; Animation
    (when (< anim-progress 1)
      (js/requestAnimationFrame #(core/render)))))

(defn field-comparator [[key value]]
  (let [[x y] (parse-key key)]
    (str y "," x)))

(defn open-cell [gx gy]
  (when (= :new phase)
    (set! phase :play)
    (core/append-history (:id puzzle) :start))
  (let [key                         (key gx gy)
        {:keys [mine open flagged]} (get-cell gx gy)]
    (cond+
      open    :noop
      flagged (do
                (assoc! (get field key) :flagged false)
                (update-field))
      #_mine    #_(do
                    (core/append-history (:id puzzle) :lose)
                    (set! phase :game-over)
                    (core/request-render))
      :else   (do
                #_(println (- (js/Date.now) t0) "open" gx gy)

                (let [problem-with-flags {}
                      _ (doseq [[key' {:keys [open flagged label]}] field]
                          (assoc! problem-with-flags key'
                            {:open    open
                             :flagged (if (= key key')
                                        true
                                        flagged)
                             :label   (cond
                                        (not open)    nil
                                        (= "q" label) "q"
                                        flagged       nil
                                        :else         (str (apply mine-count (parse-key key'))))}))
                      problem-without-flags {}
                      _ (doseq [[key' {:keys [open flagged label]}] field]
                          (assoc! problem-without-flags key'
                            {:open    open
                             :flagged (= key key')
                             :label   (cond
                                        (not open)    nil
                                        (= "q" label) "q"
                                        flagged       nil
                                        :else         (str (apply mine-count (parse-key key'))))}))
                      cnt (count (filter :mine (vals field)))]

                  (if-some [counterexample (or
                                             (solver/solve field-w field-h cnt problem-with-flags)
                                             (solver/solve field-w field-h cnt problem-without-flags))]
                    (do
                      #_(println "yes" counterexample)
                      (doseq [[key cell] field]
                        (assoc! cell :mine (get-in counterexample [key :mine] false)))
                      #_(update-field)
                      (core/append-history (:id puzzle) :lose)
                      (set! exploded-x gx)
                      (set! exploded-y gy)
                      (set! phase :game-over)
                      (core/request-render))
                    (do
                      #_(println "no" field)
                      (assoc! (get field key) :open true)
                      (update-field)
                      (maybe-auto-open gx gy))))))))

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
                     #_(println (- (js/Date.now) t0) "flag" gx gy)
                     (assoc! (get field key) :flagged true)
                     (update-field)))))

(defn can-auto-open [gx gy]
  (let [cell (get-cell gx gy)]
    (when (and
            (:open cell)
            (re-matches #"\d+" (or (:label cell) "")))
      (let [label-num (parse-long (:label cell))
            nbs       (->> (neighbours gx gy)
                        (remove (fn [[x y]] (processed (get-cell x y))))
                        vec)]
        (cond
          (empty? nbs)              nil
          (= (count nbs) label-num) [:flag nbs]
          (= 0 label-num)           [:open nbs])))))

(defn auto-open []
  (loop [queue auto-open-queue]
    (if-some [[x y] (first queue)]
      (if-some [[op nbs] (can-auto-open x y)]
        (let [[[nx ny] & _] nbs
              old-set       (set (map key auto-open-queue))
              all-new-nbs   (remove #(contains? old-set (key %)) (neighbours nx ny))]
          (case op
            :open (open-cell nx ny)
            :flag (flag-cell nx ny))
          (set! auto-open-queue (concat auto-open-queue all-new-nbs))
          (set! auto-open-timer (core/set-timeout auto-open-dt auto-open)))
        (recur (next queue)))
      (do
        (set! auto-open-queue [])
        (set! auto-open-timer nil)))))

(defn request-auto-open [cells]
  (when-not auto-open-timer
    (set! auto-open-timer (core/set-timeout auto-open-dt auto-open))))

(defn maybe-auto-open [gx gy]
  (when (can-auto-open gx gy)
    (set! auto-open-queue (conj auto-open-queue [gx gy]))
    (request-auto-open)
    true))

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
      (= "z" key)
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

    :let [[gx gy] (field-coords x y)]

    ;; drag flag from cell
    (and
      (#{:new :play} phase)
      (#{:mouse-left :touch} device)
      gx gy
      (:flagged (get-cell gx gy)))
    (do
      (set! dragging-flag true)
      (set! tool nil)
      (assoc! (get field (key gx gy)) :flagged false)
      (update-field))

    ;; drag flag from flag area
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
          points      (:points (last notes))
          rel-x       (- x sa-x)
          rel-y       (- y sa-y)
          num-points  (count points)]
      (if false #_(and
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
      (maybe-auto-open gx gy)
      :noop

      ;; second click on outlined cell
      (and (= gx outline-x) (= gy outline-y))
      (do
        (set! outline-x nil)
        (set! outline-y nil)
        (core/request-render))

      ;; click on open cell
      :let [cell (when (and gx gy) (get-cell gx gy))]

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
                      "q.png" "q_solved.png" "closed.png" "hover.png" "flagged.png" "flag.png" "flagged_wrong.png"
                      "mine_0.png" "mine_1.png" "mine_2.png" "mine_3.png" "mine_4.png" "explosion.png"
                      "tool_undo.png" "tool_eraser.png" "tool_color1.png" "tool_color2.png" "tool_color3.png" "tool_color4.png" "tool_clear.png"
                      "tool_eraser_selected.png" "tool_color1_selected.png" "tool_color2_selected.png" "tool_color3_selected.png" "tool_color4_selected.png"}})
