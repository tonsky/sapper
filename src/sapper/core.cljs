(ns sapper.core
  (:require
   [clojure.string :as str]
   [sapper.wake-lock :as wake-lock])
  (:require-macros
   [sapper.macros :refer [cond+]]))

(def canvas nil)
(def ctx nil)
(def notes-canvas nil)
(def notes-ctx nil)
(def overlay-canvas nil)
(def overlay-ctx nil)
(def canvas-x 0)
(def canvas-y 0)
(def canvas-w 0)
(def canvas-h 0)
(def canvas-scale 1)
(def dpi (or (.-devicePixelRatio js/window) 1))
(def sprite-size 100)
(def safe-w 580)
(def safe-h 960)
(def *screen (atom [:loading]))
(def screens {})
(def images {})
(def puzzles-by-id {})
(def puzzles-by-type {})
(def *sync-id (atom nil))
(def pointer-x)
(def pointer-y)
(def pointer-device)
(def *last-puzzle-id (atom nil))
(def *settings (atom nil))
(def default-settings
  {:expert              true
   :modern              true
   :auto-open-click     false
   :auto-open-recursive false
   :keep-awake          false
   :check-flags         false})

;; RENDERING

(def *render-requested
  (atom false))

(defn request-render []
  (when (compare-and-set! *render-requested false true)
    (js/window.requestAnimationFrame #(render))))

(defn call-screen-fn-impl [screen-key name & args]
  (let [screen (get screens (first screen-key))]
    (assert (some? screen) (str "unknown screen '" screen-key "'"))
    (when-some [f (get screen name)]
      (apply f args))))

(defn call-screen-fn [name & args]
  (apply call-screen-fn-impl @*screen name args))

(defn render [screen]
  (reset! *render-requested false)
  (.clearRect ctx canvas-x canvas-y canvas-w canvas-h)
  (.clearRect notes-ctx canvas-x canvas-y canvas-w canvas-h)
  (.clearRect overlay-ctx canvas-x canvas-y canvas-w canvas-h)

  ;; safe area
  (set! (.-strokeStyle ctx) "#1F3E5F")
  (set! (.-lineWidth ctx) 1)
  (.beginPath ctx)
  (.moveTo ctx 0 10) (.lineTo ctx 0 0) (.lineTo ctx 10 0)
  (.moveTo ctx (- safe-w 10) 0) (.lineTo ctx safe-w 0) (.lineTo ctx safe-w 10)
  (.moveTo ctx safe-w (- safe-h 10)) (.lineTo ctx safe-w safe-h) (.lineTo ctx (- safe-w 10) safe-h)
  (.moveTo ctx 10 safe-h) (.lineTo ctx 0 safe-h) (.lineTo ctx 0 (- safe-h 10))
  (.stroke ctx)

  (call-screen-fn-impl (or screen @*screen) :on-render))

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
  (/ (swap! rng #(-> % (* 1103515245) (+ 12345) (mod 2147483648))) 2147483648))

(defn parse-puzzle [puzzle]
  (let [[_ id type code] (re-find #"(([^ -]+-[^ -]+)-[^ -]+) +([foqFOQ]+)" puzzle)]
    {:id   id
     :type type
     :code code}))

(defn load-random-puzzle [type]
  (let [puzzles (mapv :id (get puzzles-by-type type))
        {:keys [won lost started]} (puzzle-statuses)
        union   (-> won (.union lost) (.union started))
        fresh   (into [] (remove #(.has union %) puzzles))]
    (if-not (empty? fresh)
      (navigate [:game (rand-nth fresh)])
      (let [just-started (-> started (.difference won) (.difference lost))]
        (if-not (empty? just-started)
          (navigate [:game (rand-nth just-started)])
          (let [just-lost (-> lost (.difference won) (.difference started))]
            (if-not (empty? just-lost)
              (navigate [:game (rand-nth just-lost)])
              (navigate [:game (rand-nth puzzles)]))))))))

;; Resources

(defn load-resources [cb]
  (let [t0        (js/Date.now)
        resources (into
                    #{"btn_back.png" "btn_random.png" "btn_settings.png"
                      "toggle.png"
                      "CoFoSansSemi-Mono-Regular.woff2" "CoFoSansSemi-Mono-Bold.woff2"}
                    (mapcat :resources (vals screens)))
        *pending (atom (count resources))]
    (add-watch *pending ::cb
      (fn [_ _ _ v]
        (when (= 0 v)
          (println "Loaded in" (- (js/Date.now) t0) "ms")
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
              font (js/FontFace. "font" (str "url(fonts/" name ")") {:weight (str weight)})]
          (-> (.load font)
            (.then (fn [loaded-font]
                     (.add js/document.fonts loaded-font)
                     (swap! *pending dec)))
            (.catch (fn [err]
                      (println "Error loading font" file err)
                      (swap! *pending dec)))))))))

;; BUTTONS

(defn button-render [{:keys [l t w h text icon hover disabled]}]
  (when disabled
    (set! (.-globalAlpha ctx) 0.5))

  (set! (.-fillStyle ctx)
    (cond
      disabled "#2e4d6f"
      hover    "#466689"
      :else    "#2e4d6f"))
  (.beginPath ctx)
  (.roundRect ctx l t w h 4)
  (.fill ctx)
  (cond
    text
    (do
      (set! (.-font ctx) "16px font")
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (set! (.-fillStyle ctx) (if disabled "#082848" "#fff"))
      (.fillText ctx text (+ l (quot w 2)) (+ t (quot h 2))))

    icon
    (.drawImage ctx (get images icon) (+ l (quot (- w sprite-size) 2)) (+ t (quot (- h sprite-size) 2)) sprite-size sprite-size))

  (when disabled
    (set! (.-globalAlpha ctx) 1)))

(defn button-on-pointer-move [button e]
  (let [{:keys [l t w h text hover]} button
        {:keys [start-x start-y x y device]} e
        over? (and
                (inside? x y l t w h)
                (or
                  (= :mouse-hover device)
                  (inside? start-x start-y l t w h)))]
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
  (let [{:keys [l t w h on-click disabled]} button
        {:keys [x y start-x start-y]} e]
    (when (and (not disabled) (both-inside? x y start-x start-y l t w h) on-click)
      (on-click e))))

;; TOGGLES

(defn toggle-render [toggle]
  (let [{:keys [l t get-value text disabled]} toggle
        value (get-value)]
    (.drawImage ctx (get images "toggle.png")
      0 (cond
          (and value       (not disabled)) 0
          (and (not value) (not disabled)) 200
          (and value            disabled)  400
          (and (not value)      disabled)  600)
      200 200
      (+ l -25) (+ t -25) sprite-size sprite-size)

    (set! (.-font ctx) "16px font")
    (set! (.-textAlign ctx) "left")
    (set! (.-textBaseline ctx) "middle")
    (set! (.-fillStyle ctx) "#fff")
    (.fillText ctx text (+ l 50 15) (+ t 25))

    (set! (.-w toggle) (+ 50 15 (:width (.measureText ctx text))))))

(defn toggle-on-pointer-up [toggle e]
  (let [{:keys [l t w get-value set-value disabled]} toggle
        {:keys [x y start-x start-y]} e]
    (when (not disabled)
      (when (both-inside? x y start-x start-y l t w 50)
        (set-value (not (get-value)))
        (request-render)))))

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

(defn parse-history-line [type line]
  (when (valid-history-line? line)
    (let [[t short-id op] (str/split line #"\s")]
      {:id   (str type "-" short-id)
       :op   (case op "s" :start "l" :lose "w" :win)
       :date (short-date->date t)})))

(defn get-history [type history]
  (-> (or history (js/localStorage.getItem (history-key type)) "")
    (str/split #"\n")
    (->>
      (keep #(parse-history-line type %))
      (vec))))

(defn puzzle-statuses [type history]
  (let [won     #{}
        started #{}
        lost    #{}]
    (doseq [{:keys [op id]} (or history (get-history type))]
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
              (js/localStorage.setItem key merged-history))
            (when cb
              (cb sorted-lines))))))))

(defn append-history [id op]
  (reset! *last-puzzle-id id)
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

;; NAVIGATION

(defn screen->hash [screen]
  (str "#" (str/join "/" screen)))

(defn hash->screen [hash]
  (as-> hash %
    (or % "")
    (str/replace % #"^#" "")
    (if (str/blank? %) nil %)
    (or % "menu")
    (str/split % #"/")))

(def *background-screen
  (atom nil))

(defn on-screen-change [old new]
  #_(println "on-screen-change" old new)
  ;; navigating into settings doesn't destroy old screen
  (if (= [:settings] new)
    (reset! *background-screen old)
    (call-screen-fn-impl old :on-exit old))

  (cond
    ;; navigating back from settings to the same screen doesn't re-initialize it
    (and
      (= [:settings] old)
      (= @*background-screen new))
    (do
      (call-screen-fn-impl new :on-reenter new)
      (reset! *background-screen nil))

    ;; navigating back from settings to another screen exits old and enters new
    (and
      (= [:settings] old)
      @*background-screen)
    (do
      (call-screen-fn-impl @*background-screen :on-exit old)
      (reset! *background-screen nil)
      (call-screen-fn-impl new :on-enter new))

    :else
    (call-screen-fn-impl new :on-enter new))

  (request-render))

(defn on-navigate [_e]
  (let [screen  (hash->screen js/window.location.hash)
        [old _] (reset-vals! *screen screen)]
    #_(println "on-navigate" old screen)
    (on-screen-change old screen)))

(defn navigate [screen]
  (let [[old _] (reset-vals! *screen screen)]
    #_(println "navigate" old screen)
    (js/history.pushState nil "" (screen->hash screen))
    (on-screen-change old screen)))

;; EVENTS

(defn rel-coords [e]
  (let [rect (.getBoundingClientRect canvas)
        x    (-> (.-clientX e) (- (.-left rect)) (* dpi) (/ canvas-scale) (+ canvas-x) js/Math.round)
        y    (-> (.-clientY e) (- (.-top rect)) (* dpi) (/ canvas-scale) (+ canvas-y) js/Math.round)]
    [x y]))

(defn on-resize []
  (let [w      (.-innerWidth js/window)
        h      (.-innerHeight js/window)
        dw     (* w dpi)
        dh     (* h dpi)
        scales [4 3 2.5 2 1.75 1.5 1.25 1 0.75 0.6666667 0.5 0.3333333 0.25]
        sx     (some #(when (<= (* % safe-w) dw) %) scales)
        sy     (some #(when (<= (* % safe-h) dh) %) scales)]
    (set! canvas-scale (min sx sy))
    (set! canvas-w (-> dw (/ canvas-scale) (/ 2) js/Math.floor (* 2)))
    (set! canvas-h (-> dh (/ canvas-scale) (/ 2) js/Math.floor (* 2)))
    (set! canvas-x (quot (- safe-w canvas-w) 2))
    (set! canvas-y (quot (- safe-h canvas-h) 2))

    (doseq [canvas [canvas notes-canvas overlay-canvas]
            :let [ctx (.getContext canvas "2d")]]
      (set! (.-width canvas) dw)
      (set! (.-height canvas) dh)
      (.resetTransform ctx)
      (.scale ctx canvas-scale canvas-scale)
      (.translate ctx (- canvas-x) (- canvas-y)))

    (request-render)))

(defn on-load []
  (println "Loading...")

  (reset! *sync-id
    (or (js/localStorage.getItem "sapper/id")
      (let [id (gen-sync-id)]
        (js/localStorage.setItem "sapper/id" id)
        id)))

  (reset! *settings
    (merge
      default-settings
      (some-> (js/localStorage.getItem "sapper/settings") js/JSON.parse)))

  (add-watch *settings ::store
    (fn [_ _ _ new]
      (js/localStorage.setItem "sapper/settings" (js/JSON.stringify new))))

  (add-watch *settings ::change
    (fn [_ _ old new]
      (when (not= (boolean (:keep-awake old)) (boolean (:keep-awake new)))
        (if (:keep-awake new)
          (wake-lock/request)
          (wake-lock/release)))))

  (set! canvas         (.querySelector js/document "#canvas"))
  (set! ctx            (.getContext canvas "2d"))
  (set! notes-canvas   (.querySelector js/document "#notes"))
  (set! notes-ctx      (.getContext notes-canvas "2d"))
  (set! overlay-canvas (.querySelector js/document "#overlay"))
  (set! overlay-ctx    (.getContext overlay-canvas "2d"))
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
  (.addEventListener overlay-canvas "contextmenu"
    (fn [e]
      (.preventDefault e)))

  ;; event handlers
  (.addEventListener js/window "resize"
    (fn [e]
      (on-resize e)
      (call-screen-fn :on-resize e)))

  (.addEventListener js/document "visibilitychange"
    (fn [_]
      (when-not (.-hidden js/document)
        (request-render)
        (wake-lock/maybe-restore (:keep-awake @*settings)))))

  (.addEventListener js/window "keydown" #(call-screen-fn :on-key-down %))

  (let [*start (atom nil)]
    (.addEventListener overlay-canvas "touchstart"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords (aget (.-touches e) 0))]
          (set! pointer-x x)
          (set! pointer-y y)
          (set! pointer-device :touch)
          (reset! *start {:start-x x, :start-y y})
          (call-screen-fn :on-pointer-down {:x x :y y :device pointer-device}))))

    (.addEventListener overlay-canvas "touchmove"
      (fn [e]
        (.preventDefault e)
        (let [[x y] (rel-coords (aget (.-touches e) 0))]
          (set! pointer-x x)
          (set! pointer-y y)
          (call-screen-fn :on-pointer-move (merge @*start {:x x :y y :device pointer-device})))))

    (.addEventListener overlay-canvas "touchend"
      (fn [e]
        (.preventDefault e)
        (wake-lock/maybe-restore (:keep-awake @*settings))
        (let [[x y] (rel-coords (aget (.-changedTouches e) 0))]
          (call-screen-fn :on-pointer-up (merge @*start {:x x :y y :device pointer-device}))
          (set! pointer-device nil))))

    (.addEventListener overlay-canvas "mousedown"
      (fn [e]
        (when-some [device (case (.-button e)
                             0 :mouse-left
                             2 :mouse-right
                             nil)]
          (let [[x y] (rel-coords e)]
            (set! pointer-x x)
            (set! pointer-y y)
            (set! pointer-device device)
            (reset! *start {:start-x x, :start-y y})
            (call-screen-fn :on-pointer-down {:x x :y y :device pointer-device})))))

    (.addEventListener overlay-canvas "mousemove"
      (fn [e]
        (when-some [device (or pointer-device :mouse-hover)]
          (let [[x y] (rel-coords e)]
            (set! pointer-x x)
            (set! pointer-y y)
            (call-screen-fn :on-pointer-move (merge @*start {:x x :y y :device device}))))))

    (.addEventListener overlay-canvas "mouseup"
      (fn [e]
        (wake-lock/maybe-restore (:keep-awake @*settings))
        (when-some [device pointer-device]
          (let [[x y] (rel-coords e)]
            (call-screen-fn :on-pointer-up (merge @*start {:x x :y y :device device}))
            (set! pointer-device nil)))))

    (call-screen-fn-impl [:loading] :on-enter [:loading])

    (.addEventListener js/window "popstate" on-navigate)

    (load-resources on-navigate)))

(.addEventListener js/window "load" on-load)
