(ns sapper.core
  (:require
   [clojure.string :as str])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

(def canvas nil)
(def ctx nil)
(def notes-canvas nil)
(def notes-ctx nil)
(def canvas-w 0)
(def canvas-h 0)
(def canvas-scale 1)
(def dpi (or (.-devicePixelRatio js/window) 1))
(def sprite-size 100)
(def safe-area nil)
(def *screen (atom [:loading]))
(def screens {})
(def images {})
(def puzzles-by-id {})
(def puzzles-by-type {})
(def *sync-id (atom nil))
(def pointer-pos [0 0])

;; RENDERING

(declare maybe-render)

(def *render-requested
  (atom false))

(defn request-render []
  (reset! *render-requested true))

(defn set-timeout [dt f]
  (js/setTimeout #(do (f) (maybe-render)) dt))

(defn add-event-listener [el event f opts]
  (.addEventListener el event
    #(do (f %) (maybe-render))
    opts))

(defn call-screen-fn-impl [screen-key name & args]
  (let [screen (get screens (first screen-key))]
    (assert (some? screen) (str "unknown screen '" screen-key "'"))
    (when-some [f (get screen name)]
      (apply f args))))

(defn call-screen-fn [name & args]
  (apply call-screen-fn-impl @*screen name args))

(defn render [screen]
  (reset! *render-requested false)
  (.clearRect ctx 0 0 canvas-w canvas-h)
  (.clearRect notes-ctx 0 0 canvas-w canvas-h)

  ;; safe area
  (let [[l t w h] safe-area]
    (set! (.-fillStyle ctx) "#103050")
    (.beginPath ctx)
    (.roundRect ctx l t w h 4)
    (.fill ctx))

  ;; viewport size
  (set! (.-font ctx) (str "10px " font-family))
  (set! (.-fillStyle ctx) "#477397")
  (set! (.-textAlign ctx) "left")
  (.fillText ctx (str canvas-w "×" canvas-h "@" canvas-scale) 13 23)
  (.fillText ctx @*sync-id 13 35)

  (call-screen-fn-impl (or screen @*screen) :on-render))

(defn maybe-render []
  (when @*render-requested
    (render)))

;; UTILS

(defn indexed [seq]
  (map vector (range) seq))

(defn clamp [x min max]
  (js/Math.max (js/Math.min x max) min))

(defn dist [[x1 y1] [x2 y2]]
  (js/Math.hypot (- x1 x2) (- y1 y2)))

(defn penultimate [xs]
  (nth xs (- (count xs) 2)))

(defn inside? [x y l t w h margin]
  (let [margin (or margin 0)]
    (and
      (<= (- l margin) x)
      (< x (+ l w margin))
      (<= (- t margin) y)
      (< y (+ t h margin)))))

(defn both-inside? [x1 y1 x2 y2 l t w h margin]
  (and
    (inside? x1 y1 l t w h margin)
    (inside? x2 y2 l t w h margin)))

(defn make-rng [seed]
  (atom seed))

(defn advance-rng [rng]
  (swap! rng #(-> % (* 1103515245) (+ 12345) (mod 2147483648))))

(defn random [rng]
  (/ @rng 2147483648))

(defn parse-puzzle [puzzle]
  (let [[_ id type code] (re-find #"(([^ -]+-[^ -]+)-[^ -]+) +([foqFOQ]+)" puzzle)]
    {:id   id
     :type type
     :code code}))

(defn reload []
  #_(reset! *screen @*screen)
  (.reload (.-location js/window)))

(defn load-random-puzzle [type]
  (let [puzzles (mapv :id (get puzzles-by-type type))
        {:keys [won lost started]} (puzzle-statuses)
        union   (-> won (.union lost) (.union started))
        fresh   (into [] (remove #(.has union %) puzzles))]
    (if-not (empty? fresh)
      (reset! *screen [:game (rand-nth fresh)])
      (let [just-started (-> started (.difference won) (.difference lost))]
        (if-not (empty? just-started)
          (reset! *screen [:game (rand-nth just-started)])
          (let [just-lost (-> lost (.difference won) (.difference started))]
            (if-not (empty? just-lost)
              (reset! *screen [:game (rand-nth just-lost)])
              (reset! *screen [:game (rand-nth puzzles)]))))))))

;; Resources

(def font-family
  "'CoFo Sans Semi-Mono'")

(defn-log load-resources [cb]
  (let [resources (into
                    #{"btn_back.png" "btn_reload.png" "btn_random.png"
                      "CoFoSansSemi-Mono-Regular.woff2" "CoFoSansSemi-Mono-Bold.woff2"}
                    (mapcat :resources (vals screens)))
        *pending (atom (count resources))]
    (add-watch *pending ::cb
      (fn [_ _ _ v]
        (when (= 0 v)
          (cb))))
    (doseq [name resources]
      (condp re-matches name
        #".*\.png"
        (let [img (js/Image.)]
          (set! (.-onload img) #(do
                                  (assoc! images name img)
                                  (swap! *pending dec)))
          (set! (.-src img) (str "i/" name)))

        #".*\.txt"
        (-> (js/fetch (str "puzzles/" name))
          (.then #(.text %))
          (.then (fn [text]
                   (let [arr (->> (str/split text #"\n")
                               (map parse-puzzle)
                               vec)]
                     (assoc! puzzles-by-type (:type (first arr)) arr)
                     (doseq [puzzle arr]
                       (assoc! puzzles-by-id (:id puzzle) puzzle))
                     (swap! *pending dec))))
          (.catch (fn [err]
                    (println "Error loading" name err)
                    (swap! *pending dec))))

        #".*\.woff2"
        (let [weight (condp re-find name
                       #"Regular" "400"
                       #"Bold"    "700")
              family (re-find #"(?<=^').*(?='$)" font-family)
              font (js/FontFace. family (str "url(fonts/" name ")") {:weight (str weight)})]
          (-> (.load font)
            (.then (fn [loaded-font]
                     (.add js/document.fonts loaded-font)
                     (swap! *pending dec)))
            (.catch (fn [err]
                      (println "Error loading font" file err)
                      (swap! *pending dec)))))))))

;; BUTTONS

(defn button-render [{:keys [l t w h text icon hover disabled]}]
  (let [[left top _ _] safe-area]
  (when disabled
    (set! (.-globalAlpha ctx) 0.5))

  (set! (.-fillStyle ctx) (if (and hover (not disabled)) "#466689" "#2e4d6f"))
  (.beginPath ctx)
  (.roundRect ctx (+ left l) (+ top t) w h 4)
  (.fill ctx)
  (cond
    text
    (do
      (set! (.-font ctx) (str "16px " font-family))
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (set! (.-fillStyle ctx) "#fff")
      (.fillText ctx text (+ left l (quot w 2)) (+ top t (quot h 2))))

    icon
    (.drawImage ctx (get images icon) (+ left l (quot (- w sprite-size) 2)) (+ top t (quot (- h sprite-size) 2)) sprite-size sprite-size))

  (when disabled
    (set! (.-globalAlpha ctx) 1))))

(defn button-on-pointer-move [button e]
  (let [[left top _ _] safe-area
        {:keys [l t w h text hover]} button
        {:keys [x y]} e
        over? (inside? x y (+ left l) (+ top t) w h)]
    (cond
      (and (not hover) over?)
      (do
        (assoc! button :hover true)
        (request-render))

      (and hover (not over?))
      (do
        (assoc! button :hover false)
        (request-render)))))

(defn button-on-pointer-up [button e]
  (let [[left top _ _] safe-area
        {:keys [l t w h text hover on-click]} button
        {:keys [x y start-x start-y]} e]
    (when (and (both-inside? x y start-x start-y (+ left l) (+ top t) w h) on-click)
      (on-click e))))

;; STORAGE

(defn gen-sync-id []
  (let [chars "abcdefghijklmnopqrstuvwxyz0123456789"]
    (apply str (repeatedly 13 #(get chars (rand-int (count chars)))))))

(def t0
  (.getTime (Date. "2025-01-01")))

(defn date->short-date [t]
  (-> t (- t0) (/ 1000) js/Math.floor))

(defn short-date->date [t]
  (-> t (* 1000) (+ t0) js/Date.))

(defn history-key [type]
  (str "sapper/history-" type))

(defn valid-history-line? [line]
  (and line (re-matches #"\d+ \w+ \w" line)))

(defn get-history [type]
  (vec
    (for [line (str/split (or (js/localStorage.getItem (history-key type)) "") #"\n")
          :when (valid-history-line? line)
          :let [[t short-id op] (str/split line #"\s")]]
      {:id   (str type "-" short-id)
       :op   (case op "s" :start "l" :lose "w" :win)
       :date (short-date->date t)})))

(defn puzzle-statuses [type]
  (let [won     #{}
        started #{}
        lost    #{}]
    (doseq [{:keys [op id]} (get-history type)]
      (case op
        :win   (.add won id)
        :start (.add started id)
        :lose  (.add lost id)
        nil))
    {:won won :started started :lost lost}))

(defn split-puzzle-id [id]
  (next (re-matches #"([^ -]+-[^ -]+)-([^ -]+)" id)))

(defn sync-history [type cb]
  (let [key (history-key type)
        url (str "https://sapper.tonsky.me/sync/" @*sync-id "/history-" type ".txt")]
    (-> (js/fetch url)
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 "")))
      (.catch (fn [_] ""))
      (.then
        (fn [server-history]
          (let [merged-lines   (->> (str/split server-history #"\n")
                                 (filter valid-history-line?)
                                 (into #{}))
                local-history  (or (js/localStorage.getItem key) "")
                _              (doseq [line  (str/split local-history #"\n")
                                       :when (valid-history-line? line)]
                                 (conj! merged-lines line))
                sorted-lines   (->> merged-lines
                                 vec
                                 (sort-by
                                   (fn [line]
                                     (parse-long (or (first (re-find #"^\d+" line)) "0")))))
                merged-history (if (seq sorted-lines)
                                 (str (str/join "\n" sorted-lines) "\n")
                                 "")]

            (when (not= merged-history server-history)
              (js/fetch url
                #js {:method  "PUT"
                     :body    merged-history
                     :headers #js {"Content-Type" "text/plain"}}))

            (when (not= merged-history local-history)
              (js/localStorage.setItem key merged-history)
              (when cb
                (cb merged-history)))))))))

(defn append-history [id op]
  (let [[type short-id] (split-puzzle-id id)
        v               (or (js/localStorage.getItem (history-key type)) "")
        t'              (date->short-date (js/Date.now))
        op'             (subs op 0 1)
        v'              (str v t' " " short-id " " op' "\n")]
    (js/localStorage.setItem (history-key type) v')
    (sync-history type)))

(defn upgrade-storage-v1 []
  (let [history (-> (or (js/localStorage.getItem "history") "")
                  (str/split #"\n")
                  (->> (remove str/blank?)
                    (mapv js/JSON.parse)))
        history' (for [{:keys [id op date]} history]
                   (str "[V]5x5-10-" id " " (subs op 0 1) " " (-> date (- t0) (/ 1000) js/Math.floor)))]
    #_(println (str/join "\n" history'))
    (js/localStorage.setItem "sapper/h" (str (str/join "\n" history') "\n"))
    (js/localStorage.setItem "sapper/v" "2")
    (js/localStorage.removeItem "history")))

(defn upgrade-storage-v2 []
  (let [result {}]
    (doseq [line (str/split (or (js/localStorage.getItem "sapper/h") "") #"\n")
            :when (not (str/blank? line))
            :let [[_ type id op t] (re-find #"([^ -]+-[^ -]+)-([^ -]+) ([a-z]) (\d+)" line)]]
      (assoc! result type (str (get result type "") t " " id " " op "\n")))
    (doseq [[type value] result]
      (js/localStorage.setItem (str "sapper/history-" type) value))
    (js/localStorage.removeItem "sapper/h")
    (js/localStorage.setItem "sapper/v" "3")))

(defn maybe-upgrade-storage []
  (let [v (-> (js/localStorage.getItem "sapper/v") (or "1") parse-long)]
    (when (<= v 1)
      (upgrade-storage-v1))
    (when (<= v 2)
      (upgrade-storage-v2))))

;; EVENTS

(defn rel-coords [e]
  (let [rect (.getBoundingClientRect canvas)
        x    (-> (.-clientX e) (- (.-left rect)) (* dpi) (/ canvas-scale) js/Math.round)
        y    (-> (.-clientY e) (- (.-top rect)) (* dpi) (/ canvas-scale) js/Math.round)]
    [x y]))

(defn on-resize []
  (let [w      (.-innerWidth js/window)
        h      (.-innerHeight js/window)
        dw     (* w dpi)
        dh     (* h dpi)
        scales [4 3 2.5 2 1.75 1.5 1.25 1 0.75 0.6666667 0.5 0.3333333 0.25]
        sx     (some #(when (<= (* % 580) dw) %) scales)
        sy     (some #(when (<= (* % 850) dh) %) scales)
        scale  (min sx sy)]
    (set! canvas-w (-> dw (/ scale) (/ 2) js/Math.floor (* 2)))
    (set! canvas-h (-> dh (/ scale) (/ 2) js/Math.floor (* 2)))
    (set! canvas-scale scale)
    (set! safe-area [(quot (- canvas-w 580) 2) (quot (- canvas-h 850) 2) 580 850])

    (set! (.-width canvas) dw)
    (set! (.-height canvas) dh)
    (.resetTransform ctx)
    (.scale ctx canvas-scale canvas-scale)

    (set! (.-width notes-canvas) dw)
    (set! (.-height notes-canvas) dh)
    (.resetTransform notes-ctx)
    (.scale notes-ctx canvas-scale canvas-scale)
    (request-render)))

(defn on-load []
  (println "core/on-load")

  (reset! *sync-id
    (or (js/localStorage.getItem "sapper/id")
        (let [id (gen-sync-id)]
          (js/localStorage.setItem "sapper/id" id)
          id)))

  (set! canvas       (.querySelector js/document "#canvas"))
  (set! ctx          (.getContext canvas "2d"))
  (set! notes-canvas (.querySelector js/document "#notes"))
  (set! notes-ctx    (.getContext notes-canvas "2d"))
  (on-resize)
  (maybe-upgrade-storage)

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

  ;; Prevent context menu
  (add-event-listener canvas "contextmenu"
    (fn [e]
      (.preventDefault e)))

  ;; event handlers
  (add-watch *screen ::on-enter
    (fn [_ _ old new]
      (call-screen-fn-impl old :on-exit)
      (call-screen-fn-impl new :on-enter)
      (render new)))

  (add-event-listener js/window "resize"
    (fn [e]
      (on-resize e)
      (call-screen-fn :on-resize e)))

  (add-event-listener js/document "visibilitychange"
    (fn [_]
      (when-not (.-hidden js/document)
        (request-render))))

  (add-event-listener js/window "keydown" #(call-screen-fn :on-key-down %))

  (let [*start  (atom nil)
        *device (atom nil)]
    (add-event-listener canvas "touchstart"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords (aget (.-touches e) 0))]
          (set! pointer-pos [x y])
          (reset! *start {:start-x x, :start-y y})
          (reset! *device :touch)
          (call-screen-fn :on-pointer-down {:x x :y y :device @*device}))))

    (add-event-listener canvas "touchmove"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords (aget (.-touches e) 0))]
          (set! pointer-pos [x y])
          (call-screen-fn :on-pointer-move (merge @*start {:x x :y y :device @*device})))))

    (add-event-listener canvas "touchend"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords (aget (.-changedTouches e) 0))]
          (call-screen-fn :on-pointer-up (merge @*start {:x x :y y :device @*device}))
          (reset! *device nil))))

    (add-event-listener canvas "mousedown"
      (fn [e]
        (when-some [device (case (.-button e)
                             0 :mouse-left
                             2 :mouse-right
                             nil)]
          (let [[x y] (rel-coords e)]
            (set! pointer-pos [x y])
            (reset! *start {:start-x x, :start-y y})
            (reset! *device device)
            (call-screen-fn :on-pointer-down {:x x :y y :device @*device})))))

    (add-event-listener canvas "mousemove"
      (fn [e]
        (when-some [device (or @*device :mouse-hover)]
          (let [[x y] (rel-coords e)]
            (set! pointer-pos [x y])
            (call-screen-fn :on-pointer-move (merge @*start {:x x :y y :device device}))))))

    (add-event-listener canvas "mouseup"
      (fn [e]
        (when-some [device @*device]
          (let [[x y] (rel-coords e)]
            (call-screen-fn :on-pointer-up (merge @*start {:x x :y y :device device}))
            (reset! *device nil)))))

    (let [navigate (fn [_e]
                     (let [hash   js/window.location.hash
                           hash   (if (str/blank? hash) nil (subs hash 1))
                           screen (str/split (or hash "menu") #"/")]
                       (reset! *screen screen)))]
      (add-event-listener js/window "hashchange" navigate)
      (load-resources navigate))))

(add-event-listener js/window "load" on-load)
