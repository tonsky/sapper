(ns sapper.core
  (:require
   [clojure.string :as str])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

(def canvas nil)
(def ctx nil)
(def notes nil)
(def notes-ctx nil)
(def canvas-w 0)
(def canvas-h 0)
(def canvas-scale 1)
(def dpi (or (.-devicePixelRatio js/window) 1))
(def safe-area nil)
(def *puzzle (atom nil))
(def *screen (atom :loading))
(def images {})
(def puzzles {})
(def t0 (.getTime (Date. "2025-01-01")))

(def *render-requested
  (atom false))

(defn request-render []
  (reset! *render-requested true))

(defn indexed [seq]
  (map vector (range) seq))

(defn inside? [x y l t w h margin]
  (let [margin (or margin 0)]
    (and
      (<= (- l margin) x)
      (< x (+ l w margin))
      (<= (- t margin) y)
      (< y (+ t h margin)))))

(defn parse-puzzle [puzzle]
  (let [[_ id code] (re-find #"([^ ]+) +([foqFOQ]+)" puzzle)]
    {:id id
     :code code}))

(defn-log load-resources [cb]
  (let [*pending (atom 0)
        _        (add-watch *pending ::cb
                   (fn [_ _ _ v]
                     (when (= 0 v)
                       (cb))))
        is    ["level_select.png"
               "btn_back.png" "btn_retry.png" "btn_reload.png"
               "0.png" "1.png" "2.png" "3.png" "4.png" "5.png" "6.png" "7.png" "8.png"
               "0_solved.png" "1_solved.png" "2_solved.png" "3_solved.png" "4_solved.png" "5_solved.png" "6_solved.png" "7_solved.png" "8_solved.png"
               "-1.png" "-2.png" "-3.png" "-4.png" "-5.png" "-6.png" "-7.png" "-8.png"
               "error_0.png" "error_1.png" "error_2.png" "error_3.png" "error_4.png" "error_5.png" "error_6.png" "error_7.png"
               "q.png" "q_solved.png" "closed.png" "hover.png" "flagged.png" "flag.png"
               "tool_eraser.png" "tool_color1.png" "tool_color2.png" "tool_color3.png" "tool_color4.png"
               "tool_eraser_selected.png" "tool_color1_selected.png" "tool_color2_selected.png" "tool_color3_selected.png" "tool_color4_selected.png"]
        _     (swap! *pending + (count is))
        _     (doseq [name is
                      :let [img (js/Image.)]]
                (set! (.-onload img) #(do
                                        (assoc! images name img)
                                        (swap! *pending dec)))
                (set! (.-src img) (str "i/" name)))
        ps    ["v5.txt" "v6.txt" "v7.txt" "v8.txt"]
        _     (swap! *pending + (count ps))
        _     (doseq [name ps]
                (-> (js/fetch (str "puzzles/" name))
                  (.then #(.text %))
                  (.then (fn [text]
                           (let [arr (->> (str/split text #"\n")
                                       (map parse-puzzle)
                                       vec)]
                           (assoc! puzzles name arr)
                           (swap! *pending dec))))
                  (.catch (fn [err]
                            (println "Error loading" name err)
                            (swap! *pending dec)))))]))

(defn get-history []
  (vec
    (for [line (str/split (or (js/localStorage.getItem "sapper/h") "") #"\n")
          :when (not (str/blank? line))
          :let [[id op t] (str/split line #"\s")]]
      {:id   id
       :op   (case op "s" :start "l" :lose "w" :win)
       :date (Date. (* (+ t t0) 1000))})))

(defn append-history [id op]
  (let [v  (or (js/localStorage.getItem "sapper/h") "")
        v' (str v id " " (subs op 0 1) " " (-> (js/Date.now) (- t0) (/ 1000) js/Math.floor) "\n")]
    (js/localStorage.setItem "sapper/h" v')))

(defn maybe-upgrade-storage []
  (let [v (-> (js/localStorage.getItem "sapper/v") (or "1") parse-long)]
    (when (<= v 1)
      (let [history (-> (or (js/localStorage.getItem "history") "")
                      (str/split #"\n")
                      (->> (remove str/blank?)
                        (mapv js/JSON.parse)))
            history' (for [{:keys [id op date]} history]
                       (str "[V]5x5-10-" id " " (subs op 0 1) " " (-> date (- t0) (/ 1000) js/Math.floor)))]
        (println (str/join "\n" history'))
        (js/localStorage.setItem "sapper/h" (str (str/join "\n" history') "\n"))
        (js/localStorage.setItem "sapper/v" "2")
        (js/localStorage.removeItem "history")))))

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

    (set! (.-width notes) dw)
    (set! (.-height notes) dh)
    (.resetTransform notes-ctx)
    (.scale notes-ctx canvas-scale canvas-scale)))

(defn on-load []
  (println "core/on-load")
  (set! canvas    (.querySelector js/document "#canvas"))
  (set! ctx       (.getContext canvas "2d"))
  (set! notes     (.querySelector js/document "#notes"))
  (set! notes-ctx (.getContext notes "2d"))
  (on-resize)
  (maybe-upgrade-storage))

(.addEventListener js/window "load" on-load)
