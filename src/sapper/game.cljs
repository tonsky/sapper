(ns sapper.game
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [ctx notes-ctx overlay-ctx images safe-w safe-h sprite-size]]
   [sapper.level-select :as level-select]
   [sapper.solver :as solver])
  (:require-macros
   [sapper.macros :refer [cond+]]))

(defclass Field
  (field -map (js/Map.))
  (field width)
  (field height)
  (constructor [this w h]
    (set! width w)
    (set! height h))

  Object
  (get       [_ key def]   (or (.get -map (js/JSON.stringify key)) def))
  (set       [_ key value] (.set -map (js/JSON.stringify key) value))
  (keys      [_]           (map js/JSON.parse (.keys -map)))
  (vals      [_]           (.values -map))
  (contains? [_ key]       (.has -map (js/JSON.stringify key)))
  (seq       [_]           (map (fn [[k v]] [(js/JSON.parse k) v]) (.entries -map))))

(def puzzle nil)

(def cell-size 70)
(def margin (-> sprite-size (- cell-size) (/ 2)))
(def field)
(def flags 0)
(def grid-x 0)
(def grid-y 0)
(def grid-w 0)
(def grid-h 0)
(def outline-pos nil)
(def phase)
(def exploded-pos nil)
(def buttons)
(def dragging-flag false)
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
(def auto-open-dt 50)

(defn neighbours [[x y]]
  (let [max-x (dec (.-width field))
        max-y (dec (.-height field))]
    (concat
      (when (and (> x 0) (> y 0))
        [[(dec x) (dec y)]])
      (when (> x 0)
        [[(dec x)      y ]])
      (when (and (> x 0) (< y max-y))
        [[(dec x) (inc y)]])
      (when (> y 0)
        [[     x  (dec y)]])
      (when (< y max-y)
        [[     x  (inc y)]])
      (when (and (< x max-x) (> y 0))
        [[(inc x) (dec y)]])
      (when (< x max-x)
        [[(inc x)      y ]])
      (when (and (< x max-x) (< y max-y))
        [[(inc x) (inc y)]]))))

(defn get-cell [pos]
  (when pos
    (.get field pos)))

(defn coord->pos [[x y]]
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
            left       (-> safe-w (- combined-w) (quot 2))
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

(defn can-auto-finish? []
  (when (#{:new :play} phase)
    (let [unprocessed (->> field .vals (remove processed) count)]
      (when (pos? unprocessed)
        (cond
          (= flags 0)           :open-all
          (= flags unprocessed) :flag-all)))))

(defn undo []
  (when (seq notes)
    (.pop notes)
    (core/request-render)))

(defn update-field []
  (doseq [[pos cell] (.seq field)
          :let [nbs (mapv get-cell (neighbours pos))]]
    (assoc! cell
      :flags  (count (filter :flagged nbs))
      :solved (every? processed nbs)))

  (when (and outline-pos (:solved (get-cell outline-pos)))
    (set! outline-pos nil))

  (set! flags (-
                (->> field .vals (filter :mine) count)
                (->> field .vals (filter :flagged) count)))

  (assoc! (:finish buttons) :disabled (not (can-auto-finish?)))

  (when (->> field .vals (every? processed))
    (core/append-history (:id puzzle) :win)
    (set! phase :victory))

  (core/request-render))

(defn restart []
  (on-enter @core/*screen)
  (core/request-render))

(defn on-enter [[_ id]]
  (let [_         (set! puzzle (get core/puzzles-by-id id))
        code      (:code puzzle)
        [_ fw fh] (re-find #"(\d+)x(\d+)" id)]

    (set! buttons
      {:back     {:l  10            :t 10 :w 50 :h 50 :icon "btn_back.png"     :on-click #(core/navigate [:level-select (:type puzzle)])}
       :random   {:l (- safe-w 120) :t 10 :w 50 :h 50 :icon "btn_random.png"   :on-click #(core/load-random-puzzle (:type puzzle))}
       :settings {:l (- safe-w  60) :t 10 :w 50 :h 50 :icon "btn_settings.png" :on-click #(core/navigate [:settings])}
       :restart  {:l (- safe-w 120) :t 70 :w 50 :h 50 :icon "btn_restart.png"  :on-click restart}
       :finish   {:l (- safe-w  60) :t 70 :w 50 :h 50 :icon "btn_finish.png"   :on-click auto-finish :disabled true}})

    (set! phase :init)
    (set! field (Field. (parse-long fw) (parse-long fh)))
    (set! grid-w (* (.-width field) cell-size))
    (set! grid-h (* (.-height field) cell-size))
    (set! grid-x (-> safe-w (- grid-w) (quot 2)))
    (set! grid-y (-> safe-h (- 100) (- grid-h) (quot 2)))

    (doseq [[i ch] (core/indexed code)]
      (let [x (mod i (.-width field))
            y (quot i (.-width field))]
        (.set field [x y]
          (case ch
            "f" {:mine true,  :open false}
            "F" {:mine true,  :open false, :flagged true}
            "o" {:mine false, :open false}
            "O" {:mine false, :open true}
            "q" {:mine false, :open false, :secret true}
            "Q" {:mine false, :open true,  :secret true}))))

    (doseq [[pos cell] (.seq field)]
      (let [nbs (mapv get-cell (neighbours pos))]
        (assoc! cell :mines (count (filter :mine nbs)))))

    (update-field)
    (set! outline-pos nil)
    (set! exploded-pos nil)
    (set! phase :new)
    (set! dragging-flag false)
    (set! tool nil)
    (set! notes [])
    (set! anim-start (js/Date.now))))

(defn on-render []
  (let [anim-progress (core/clamp (/ (- (js/Date.now) anim-start) anim-length) 0 1)
        hover-pos     (when dragging-flag
                        (coord->pos [core/pointer-x core/pointer-y]))
        id            (:id puzzle)
        rng           (core/make-rng (js/parseInt (str/join (re-seq #"\d" id))))
        cell-coord    (fn [[x y]]
                        [(+ (* x cell-size) (+ grid-x) (- margin))
                         (+ (* y cell-size) (+ grid-y) (- margin))])
        modern?       (:modern @core/*settings)]
    ;; Title
    (set! (.-font ctx) "bold 24px font")
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (set! (.-fillStyle ctx) "#FFF")
    (.fillText ctx id (quot safe-w 2) 35)

    ;; buttons
    (doseq [b (vals buttons)]
      (core/button-render b))

    ;; cells
    (doseq [y (range (.-height field))
            x (range (.-width field))]
      (let [pos            [x y]
            cell           (get-cell pos)
            {:keys [mine mines flagged flags open secret solved]} cell
            random         (core/advance-rng rng)
            name           (cond
                             (= pos exploded-pos)
                             nil ;; render after everything else

                             (and (= :game-over phase) mine (not flagged))
                             (str "mine_" (-> random (* 5) js/Math.floor) ".png")

                             (and (= :game-over phase) (not mine) flagged)
                             "flagged_wrong.png"

                             (and dragging-flag (not open) (not flagged) (= pos hover-pos))
                             "hover.png"

                             flagged
                             "flagged.png"

                             (not open)
                             "closed.png"

                             (and secret solved)
                             "secret_solved.png"

                             secret
                             "secret.png"

                             (and modern? (> flags mines))
                             (str (- mines flags) ".png")

                             (> flags mines)
                             (str "error_" mines ".png")

                             (and modern? solved)
                             "0_solved.png"

                             solved
                             (str mines "_solved.png")

                             modern?
                             (str (- mines flags) ".png")

                             :else
                             (str mines ".png"))
            img            (get images name)
            anim-stagger   (-> (+ (* y (.-width field)) x) (/ (.-width field) (.-height field)))
            anim-progress' (-> anim-progress (- (* anim-stagger 0.75)) (* 4) (core/clamp 0 1))
            anim-travel    (/ (core/dist [0 0] pos) (core/dist [0 0] [(.-width field) (.-height field)]))
            anim-offset-x  (* (- 1 anim-progress') (* anim-travel 30))
            anim-offset-y  (* (- 1 anim-progress') (* anim-travel 50))
            [px py]        (cell-coord pos)]
        (when name
          (set! (.-globalAlpha ctx) anim-progress')
          (.drawImage ctx img (+ px anim-offset-x) (+ py anim-offset-y) sprite-size sprite-size))))
    (set! (.-globalAlpha ctx) 1)

    ;; explosion
    (when exploded-pos
      (let [[px py] (cell-coord exploded-pos)
            img     (get images "explosion.png")]
        (.drawImage ctx img px py sprite-size sprite-size)))

    ;; outline
    (when outline-pos
      (let [[x y]   outline-pos
            left    (max 0 (dec x))
            top     (max 0 (dec y))
            right   (min (dec (.-width field)) (inc x))
            bottom  (min (dec (.-height field)) (inc y))
            padding 4
            {:keys [flags mines solved secret]} (get-cell outline-pos)
            cnt     (if (:modern @core/*settings)
                      (- mines flags)
                      mines)
            color   (cond
                      secret          "#73A2C9"
                      (> flags mines) "#9B2226"
                      solved          "#73A2C9"
                      (= 0 cnt)       "#0A9496"
                      (= 1 cnt)       "#0A9496"
                      (= 2 cnt)       "#95D2BD"
                      (= 3 cnt)       "#E9D8A6"
                      (= 4 cnt)       "#EE9C02"
                      (= 5 cnt)       "#CA6702"
                      (= 6 cnt)       "#BC3E02"
                      (= 7 cnt)       "#AF2012"
                      (= 8 cnt)       "#9B2226"
                      :else           "#F0F")]
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
          over?                  (and dragging-flag (core/inside? core/pointer-x core/pointer-y fl ft fw fh))
          area                   (if over? [fl ft fw fh flags fg fnl] (flag-area))]
      (when-some [[l t w h visible-flags flag-gap number-left] area]
        (let [flag-img  (get images "flag.png")
              hover-idx (when over?
                          (-> core/pointer-x (- l) (- 20) (quot flag-gap) (core/clamp 0 (dec visible-flags))))]
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
          (set! (.-fillStyle ctx) "#DA4F55")
          (.fillText ctx (str visible-flags) number-left (+ t (quot h 2))))))

    ;; Tools
    (let [width       (* (count tools) tool-size)
          left        (quot (- safe-w width) 2)
          tool-margin (-> sprite-size (- tool-size) (/ 2))]
      (doseq [[i t] (core/indexed tools)
              :let [x   (+ left (* i tool-size))
                    y   (- safe-h 25 tool-size)
                    img (get images (str "tool_" t (if (= t tool) "_selected" "") ".png"))]]
        (.drawImage ctx img (- x tool-margin) (- y tool-margin) sprite-size sprite-size)))

    ;; Draw notes
    (doseq [stroke notes
            [dx dy mode] [[2 2 :shadow] [0 0 :stroke]]
            :let [t         (:tool stroke)
                  points    (:points stroke)
                  nth-point #(let [[x y] (aget points %)]
                               [(+ x dx) (+ y dy)])]]
      (when (seq points)
        (set! (.-lineWidth notes-ctx) (case t :eraser 40 6))
        (set! (.-strokeStyle notes-ctx)
          (cond
            (= :eraser t)    "#000"
            (= :shadow mode) "#00000080"
            :else            (get tool-colors t)))
        (set! (.-lineCap notes-ctx) "round")
        (set! (.-lineJoin notes-ctx) "round")
        (when (= :eraser t)
          (set! (.-globalCompositeOperation notes-ctx) "destination-out"))
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
        (when (= :eraser t)
          (set! (.-globalCompositeOperation notes-ctx) "source-over"))))

    ;; Eraser cursor
    (when (or
            (= :eraser tool)
            (and tool (= :mouse-right core/pointer-device)))
      (set! (.-strokeStyle overlay-ctx) "#FFFFFF20")
      (set! (.-lineWidth overlay-ctx) 1)
      (.beginPath overlay-ctx)
      (.arc overlay-ctx core/pointer-x core/pointer-y 20 0 (* 2 js/Math.PI))
      (.stroke overlay-ctx))

    ;; Dragged flag
    (when dragging-flag
      (let [flag-img (get images "flag.png")]
        (.drawImage overlay-ctx flag-img
          (-> core/pointer-x (- margin) (- (quot cell-size 2)))
          (-> core/pointer-y (- margin) (- (case core/pointer-device
                                             :mouse-left (quot cell-size 2)
                                             :touch      cell-size)))
          sprite-size sprite-size)))

    ;; End game screen
    (when (#{:game-over :victory} phase)
      (set! (.-fillStyle overlay-ctx) "#07294798")
      (.fillRect overlay-ctx core/canvas-x (+ grid-y (quot (- grid-h 90) 2)) core/canvas-w 90)
      (set! (.-font overlay-ctx) "bold 40px font")
      (set! (.-textAlign overlay-ctx) "center")
      (set! (.-textBaseline overlay-ctx) "middle")
      (set! (.-fillStyle overlay-ctx) "#FFF")
      (.fillText overlay-ctx (case phase :game-over "Game Over :(" :victory "Victory!")
        (+ grid-x (quot grid-w 2)) (+ grid-y (quot grid-h 2))))

    ;; Animation
    (when (< anim-progress 1)
      (core/request-render))))

(defn open-cell [pos]
  (when (= :new phase)
    (set! phase :play)
    (core/append-history (:id puzzle) :start))
  (let [cell (get-cell pos)
        {:keys [mine open flagged]} cell
        expert? (:expert @core/*settings)]

    (cond+
      (#{:game-over :victory} phase)
      :noop

      open
      :noop

      flagged
      (do
        (assoc! cell :flagged false)
        (update-field))

      (and (not expert?) mine)
      (do
        (set! exploded-pos pos)
        (core/append-history (:id puzzle) :lose)
        (set! phase :game-over)
        (core/request-render))

      (not expert?)
      (do
        (assoc! cell :open true)
        (update-field)
        (when (:auto-open-recursive @core/*settings)
          (maybe-auto-open pos)))

      :let [problem-with-flags    (str/join
                                    (for [y (range (.-height field))
                                          x (range (.-width field))
                                          :let [{:keys [open flagged secret mines]} (.get field [x y])]]
                                      (cond
                                        flagged       "F"
                                        (= [x y] pos) "F"
                                        (not open)    "."
                                        secret        "?"
                                        :else         (str mines))))
            problem-without-flags (str/join
                                    (for [y (range (.-height field))
                                          x (range (.-width field))
                                          :let [{:keys [open flagged secret mines]} (.get field [x y])]]
                                      (cond
                                        (= [x y] pos) "F"
                                        (not open)    "."
                                        secret        "?"
                                        :else         (str mines))))
            total-flags           (count (filter :mine (.vals field)))
            counterexample        (or
                                    (solver/solve (.-width field) (.-height field) total-flags problem-with-flags)
                                    (solver/solve (.-width field) (.-height field) total-flags problem-without-flags))]

      counterexample
      (do
        #_(println "yes" counterexample)
        (doseq [[[x y] cell] (.seq field)]
          (assoc! cell :mine (= "F" (aget counterexample (+ (* y (.-width field)) x)))))
        (set! exploded-pos pos)
        #_(update-field)
        (core/append-history (:id puzzle) :lose)
        (set! phase :game-over)
        (core/request-render))

      :else
      (do
        #_(println "no" field)
        (assoc! (.get field pos) :open true)
        (update-field)
        (when (:auto-open-recursive @core/*settings)
          (maybe-auto-open pos))))))

(defn flag-cell [pos]
  (when (= :new phase)
    (set! phase :play)
    (core/append-history (:id puzzle) :start))
  (let [cell (get-cell pos)
        {:keys [flagged open]} cell]
    (cond+
      (#{:game-over :victory} phase)
      :noop

      open
      :noop

      flagged
      (do
        (assoc! cell :flagged false)
        (update-field))

      (<= flags 0)
      :noop

      (not (:check-flags @core/*settings))
      (do
        (assoc! cell :flagged true)
        (update-field))

      :let [problem (str/join
                      (for [y (range (.-height field))
                            x (range (.-width field))
                            :let [{:keys [open flagged secret mines]} (.get field [x y])]]
                        (cond
                          (= [x y] pos) "?"
                          flagged       "F"
                          (not open)    "."
                          secret        "?"
                          :else         (str mines))))
            total-flags    (count (filter :mine (.vals field)))
            counterexample (solver/solve (.-width field) (.-height field) total-flags problem)]

      counterexample
      (do
        (assoc! cell :flagged true)
        (doseq [[[x y] cell] (.seq field)]
          (assoc! cell :mine (= "F" (aget counterexample (+ (* y (.-width field)) x)))))
        (core/append-history (:id puzzle) :lose)
        (set! phase :game-over)
        (core/request-render))

      :else
      (do
        (assoc! cell :flagged true)
        (update-field)))))

(defn auto-finish []
  (when-some [action (can-auto-finish?)]
    (let [unprocessed (->> (.keys field)
                        (remove #(processed (get-cell %)))
                        (shuffle))]
      (doseq [[i pos] (core/indexed unprocessed)]
        (js/setTimeout
          (case action
            :open-all #(open-cell pos)
            :flag-all #(flag-cell pos))
          (* auto-open-dt i))))))

(defn can-auto-open [pos]
  (let [{:keys [open secret mines flags]} (get-cell pos)]
    (when (and open (not secret))
      (let [nbs         (neighbours pos)
            unprocessed (->> nbs
                          (remove #(processed (get-cell %)))
                          vec)]
        (cond
          (= 0 (count unprocessed))               nil
          (= (+ flags (count unprocessed)) mines) [:flag unprocessed]
          (= flags mines)                         [:open unprocessed])))))

(defn auto-open []
  (loop [queue auto-open-queue]
    (if-some [pos (first queue)]
      (if-some [[op nbs] (can-auto-open pos)]
        (let [npos          (rand-nth nbs)
              old-set       (set (map js/JSON.stringify auto-open-queue))
              all-new-nbs   (remove #(contains? old-set (js/JSON.stringify %)) (neighbours npos))]
          (case op
            :open (open-cell npos)
            :flag (flag-cell npos))
          (when (:auto-open-recursive @core/*settings)
            (set! auto-open-queue (concat auto-open-queue all-new-nbs)))
          (set! auto-open-timer (js/setTimeout auto-open auto-open-dt)))
        (recur (next queue)))
      (do
        (set! auto-open-queue [])
        (set! auto-open-timer nil)))))

(defn request-auto-open [cells]
  (when-not auto-open-timer
    (set! auto-open-timer (js/setTimeout auto-open auto-open-dt))))

(defn maybe-auto-open [pos]
  (when (and (:auto-open-click @core/*settings) (can-auto-open pos))
    (set! auto-open-queue (conj auto-open-queue pos))
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
      (core/request-render))

    (:eraser :color1 :color2 :color3 :color4)
    (do
      (if (= tool tool')
        (set! tool nil)
        (set! tool tool'))
      (core/request-render))))

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
      (restart)

      (= "Escape" key)
      (do
        (cond
          tool
          (do
            (set! tool nil)
            (core/request-render))

          outline-pos
          (do
            (set! outline-pos nil)
            (core/request-render)))))))

(defn on-pointer-down [{:keys [x y device]}]
  (cond+
    tool
    (let [tool' (if (= :mouse-right device) :eraser tool)]
      (conj! notes {:tool tool' :points []})
      (core/request-render))

    :let [pos  (coord->pos [x y])
          cell (get-cell pos)]

    ;; drag flag from cell
    (and
      (#{:new :play} phase)
      (#{:mouse-left :touch} device)
      pos
      (:flagged cell))
    (do
      (set! dragging-flag true)
      (set! tool nil)
      (assoc! cell :flagged false)
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
  (doseq [b (vals buttons)]
    (core/button-on-pointer-move b e))

  (when (and (#{:mouse-left :mouse-right :touch} device) tool (seq notes))
    (let [points     (:points (last notes))
          num-points (count points)]
      (if (and
            (>= num-points 2)
            (< (core/dist (last points) (core/penultimate points)) 5))
        (aset points (dec num-points) [x y])
        (conj! points [x y]))))
  (when (or
          (= :eraser tool)
          (and (#{:mouse-left :mouse-right :touch} device) tool)
          (and (#{:mouse-left :touch} device) dragging-flag))
    (core/request-render)))

(defn on-pointer-up [{:keys [x y start-x start-y device] :as e}]
  (doseq [b (vals buttons)]
    (core/button-on-pointer-up b e))

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

  (let [pos       (coord->pos [x y])
        cell      (get-cell pos)
        toolbox-w (* (count tools) tool-size)
        toolbox-x (quot (- safe-w toolbox-w) 2)
        toolbox-y (- safe-h 25 tool-size)
        in?       #(core/both-inside? x y start-x start-y %1 %2 %3 %4 %5)]
    (cond+
      ;; drop flag
      dragging-flag
      (do
        (when (and cell (not (:open cell)) (not (:flagged cell)))
          (flag-cell pos))
        (set! dragging-flag false)
        (core/request-render))

      ;; toolbox
      (in? toolbox-x toolbox-y toolbox-w tool-size)
      (let [i (quot (- x toolbox-x) tool-size)
            t (nth tools i)]
        (on-tool-click (nth tools i)))

      tool
      :noop

      ;; end game
      (#{:game-over :victory} phase)
      :noop

      ;; end outside field
      (not pos)
      (do
        (set! outline-pos nil)
        (core/request-render))

      ;; auto-open neighbors if label matches unopened count
      (maybe-auto-open pos)
      :noop

      ;; second click on outlined cell
      (= outline-pos pos)
      (do
        (set! outline-pos nil)
        (core/request-render))

      ;; click on open cell
      (:open cell)
      (do
        (set! outline-pos pos)
        (core/request-render))

      ;; click on closed cell
      (#{:mouse-left :touch} device)
      (open-cell pos)

      (#{:mouse-right} device)
      (flag-cell pos))))

(assoc! core/screens :game
  {:on-enter        on-enter
   :on-reenter      update-field
   :on-render       on-render
   :on-key-down     on-key-down
   :on-pointer-up   on-pointer-up
   :on-pointer-move on-pointer-move
   :on-pointer-down on-pointer-down
   :resources       #{"btn_restart.png" "btn_finish.png"
                      "0.png" "1.png" "2.png" "3.png" "4.png" "5.png" "6.png" "7.png" "8.png"
                      "0_solved.png" "1_solved.png" "2_solved.png" "3_solved.png" "4_solved.png" "5_solved.png" "6_solved.png" "7_solved.png" "8_solved.png"
                      "-1.png" "-2.png" "-3.png" "-4.png" "-5.png" "-6.png" "-7.png" "-8.png"
                      "error_0.png" "error_1.png" "error_2.png" "error_3.png" "error_4.png" "error_5.png" "error_6.png" "error_7.png"
                      "secret.png" "secret_solved.png" "closed.png" "hover.png" "flagged.png" "flag.png" "flagged_wrong.png"
                      "mine_0.png" "mine_1.png" "mine_2.png" "mine_3.png" "mine_4.png" "explosion.png"
                      "tool_undo.png" "tool_eraser.png" "tool_color1.png" "tool_color2.png" "tool_color3.png" "tool_color4.png" "tool_clear.png"
                      "tool_eraser_selected.png" "tool_color1_selected.png" "tool_color2_selected.png" "tool_color3_selected.png" "tool_color4_selected.png"}})
