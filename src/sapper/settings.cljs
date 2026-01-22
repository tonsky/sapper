(ns sapper.settings
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [ctx canvas-w canvas-h]]))

(def buttons)
(def toggles)
(def sync-message)

(defn show-message [m]
  (set! sync-message m)
  (core/render)
  (core/set-timeout 1000
    (fn []
      (when (= m sync-message)
        (set! sync-message nil)
        (core/request-render)))))

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

(defn on-enter []
  (set! js/window.location.hash "settings")
  (let [[_ _ width _] core/safe-area]
    (set! buttons
      {:close  {:l (- width 75) :t  25 :w 50 :h 50 :icon "btn_close.png"  :on-click #(reset! core/*screen
                                                                                       (case @core/*previous-screen
                                                                                         [:loading] [:menu]
                                                                                         nil        [:menu]
                                                                                         @core/*previous-screen))}
       ; :reload {:l 100          :t  25 :w 50 :h 50 :icon "btn_reload.png" :on-click core/reload}
       :copy   {:l 375          :t 760 :w 80 :h 50 :text "Copy"           :on-click on-sync-id-copy}
       :paste  {:l 475          :t 760 :w 80 :h 50 :text "Paste"          :on-click on-sync-id-paste}})

    (set! toggles
      (into {}
        (for [[i [key text]] (core/indexed
                               (partition 2
                                 [:expert "Expert mode"
                                  :modern "Flags reduce counter"
                                  :auto-open "Recursive auto-open"
                                  :keep-awake "Keep device awake"]))]
          [key {:l         200
                :t         (+ 250 (* i 50))
                :get-value #(get @core/*settings key)
                :set-value #(do (swap! core/*settings assoc key %) (core/request-render))
                :text      text}])))))

(defn on-render []
  (let [[left top _ _] core/safe-area]
    (doseq [[_ b] buttons]
      (core/button-render b))
    (doseq [[_ t] toggles]
      (core/toggle-render t))

    ;; Sync ID
    (set! (.-font ctx) (str "16px " core/font-family))
    (set! (.-textAlign ctx) "left")
    (set! (.-textBaseline ctx) "middle")
    (set! (.-fillStyle ctx) "#fff")
    (.fillText ctx "Sync ID" (+ left 45) (+ top 760 25))

    (set! (.-lineWidth ctx) 1)
    (set! (.-strokeStyle ctx) "#2e4d6f")
    (.beginPath ctx)
    (.roundRect ctx (+ left 125) (+ top 760) 230 50 4)
    (.stroke ctx)

    (.save ctx)
    (.beginPath ctx)
    (.rect ctx (+ left 125) (+ top 760) 230 50)
    (.clip ctx)
    (set! (.-fillStyle ctx) (if (and sync-message (str/starts-with? sync-message "Invalid: ")) "#ff0000" "#fff"))
    (.fillText ctx (or sync-message @core/*sync-id) (+ left 125 15) (+ top 760 25))
    (.restore ctx)))

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
   :on-pointer-move on-pointer-move
   :on-pointer-up   on-pointer-up
   :resources       #{"btn_close.png"}})
