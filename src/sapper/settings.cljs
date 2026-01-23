(ns sapper.settings
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [ctx canvas-w canvas-h]]))

(def buttons)
(def toggles)

;; Sync ID

(def sync-message)

(defn show-message [m]
  (set! sync-message m)
  (core/request-render)
  (js/setTimeout
    #(when (= m sync-message)
       (set! sync-message nil)
       (core/request-render))
    1000))

(defn on-sync-id-copy [_e]
  (.. js/navigator -clipboard (writeText @core/*sync-id))
  (show-message "Copied!"))

(defn on-sync-id-paste [_e]
  (let [text (js/prompt "Paste Sync ID:")]
    (when (some? text)
      (if (re-matches #"[a-z0-9]{13}" text)
        (do
          (js/localStorage.setItem "sapper/id" text)
          (reset! core/*sync-id text)
          (show-message "Pasted!"))
        (show-message "Invalid Sync ID")))))

(defn on-key-down [e]
  (when (= "Escape" (.-key e))
    (close)))

;; Screen logic

(defn reload []
  (js/history.back)
  (js/setTimeout #(.reload js/window.location) 100))

(defn close []
  (cond
    (nil? @core/*background-screen)
    (core/navigate [:menu])

    (= [:loading] @core/*background-screen)
    (core/navigate [:menu])

    :else
    (js/history.back)))

(defn on-enter [_]
  (let [[_ _ width _] core/safe-area]
    (set! buttons
      {:close  {:l (- width 60) :t  10 :w  50 :h 50 :icon "btn_close.png" :on-click close}
       :copy   {:l 200          :t 555 :w  80 :h 50 :text "Copy"          :on-click on-sync-id-copy}
       :paste  {:l 290          :t 555 :w  80 :h 50 :text "Paste"         :on-click on-sync-id-paste}
       :reload {:l 200          :t 725 :w 120 :h 50 :text "Reload app"    :on-click reload}})

    (set! toggles
      (into {}
        (for [[i [key text]] (core/indexed
                               (partition 2
                                 [:keep-awake "Keep device awake"
                                  :expert     "Expert mode"
                                  :modern     "Flags reduce counter"
                                  :auto-open  "Recursive auto-open"]))]
          [key {:l         200
                :t         (+ 250 (* i 50))
                :get-value #(get @core/*settings key)
                :set-value #(swap! core/*settings assoc key %)
                :text      text}])))))

(defn on-render []
  (let [[sa-left sa-top sa-width sa-height] core/safe-area]
    (doseq [[_ b] buttons]
      (core/button-render b))
    (doseq [[_ t] toggles]
      (core/toggle-render t))

    ;; Title
    (set! (.-font ctx) "bold 24px font")
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (set! (.-fillStyle ctx) "#FFF")
    (.fillText ctx "Preferences" (+ sa-left (quot sa-width 2)) (+ sa-top 35))

    ;; Sync ID
    (set! (.-font ctx) "16px font")
    (set! (.-textAlign ctx) "left")
    (set! (.-textBaseline ctx) "middle")
    (set! (.-fillStyle ctx) "#fff")
    (.fillText ctx "Sync ID" (+ sa-left 110) (+ sa-top 500 20))

    (set! (.-lineWidth ctx) 1)
    (set! (.-strokeStyle ctx) "#2e4d6f")
    (.beginPath ctx)
    (.roundRect ctx (+ sa-left 200) (+ sa-top 495) 230 50 4)
    (.stroke ctx)

    (.save ctx)
    (.beginPath ctx)
    (.rect ctx (+ sa-left 200) (+ sa-top 495) 230 50)
    (.clip ctx)
    (set! (.-fillStyle ctx) (if (and sync-message (str/starts-with? sync-message "Invalid: ")) "#ff0000" "#fff"))
    (.fillText ctx (or sync-message @core/*sync-id) (+ sa-left 200 15) (+ sa-top 495 25))
    (.restore ctx)

    ;; Viewport size
    (.fillText ctx "Viewport" (+ sa-left 110) (+ sa-top 665))
    (.fillText ctx (str canvas-w "×" canvas-h "@" core/canvas-scale) (+ sa-left 200) (+ sa-top 665))))

(defn on-pointer-move [e]
  (doseq [[_ b] buttons]
    (core/button-on-pointer-move b e)))

(defn on-pointer-up [e]
  (doseq [[_ b] buttons]
    (core/button-on-pointer-up b e))
  (doseq [[_ t] toggles]
    (core/toggle-on-pointer-up t e)))

(assoc! core/screens :settings
  {:on-enter        on-enter
   :on-render       on-render
   :on-key-down     on-key-down
   :on-pointer-move on-pointer-move
   :on-pointer-up   on-pointer-up
   :resources       #{"btn_close.png"}})
