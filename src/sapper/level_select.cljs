(ns sapper.level-select
  (:require
   [clojure.string :as str]
   [sapper.core :as core :refer [ctx safe-w safe-h]])
  (:require-macros
   [sapper.macros :refer [defn-log cond+]]))

(def statuses)
(def hover-idx nil)
(def type)
(def buttons)

(defn on-enter [[_ t]]
  (set! type t)
  (set! statuses (core/puzzle-statuses t))
  (set! buttons
    [{:l 10             :t 10 :w 50 :h 50 :icon "btn_back.png"     :on-click #(core/navigate [:menu])}
     {:l (- safe-w 120) :t 10 :w 50 :h 50 :icon "btn_random.png"   :on-click #(core/load-random-puzzle type)}
     {:l (- safe-w  60) :t 10 :w 50 :h 50 :icon "btn_settings.png" :on-click #(core/navigate [:settings])}])
  (core/sync-history t
    (fn [lines]
      (let [parsed (keep #(core/parse-history-line t %) lines)]
        (set! statuses (core/puzzle-statuses t parsed))
        (core/request-render)))))

(defn on-render []
  (doseq [b buttons]
    (core/button-render b))

  ;; Title
  (set! (.-font ctx) "bold 24px font")
  (set! (.-textAlign ctx) "center")
  (set! (.-textBaseline ctx) "middle")
  (set! (.-fillStyle ctx) "#FFF")
  (.fillText ctx type (quot safe-w 2) 35)

  ;; puzzles
  (let [puzzles (get core/puzzles-by-type type)
        {:keys [won lost started]} statuses
        img     (get core/images "level_select.png")
        rng     (core/make-rng (js/parseInt (subs type 3 4)))]
    (doseq [[i puzzle] (core/indexed puzzles)
            :let [random       (core/advance-rng rng)
                  x            (mod i 18)
                  y            (quot i 18)
                  hover?       (= hover-idx i)
                  {:keys [id]} puzzle
                  sprite-left  (cond
                                 (and hover? (contains? won id))     700
                                 (contains? won id)                  600
                                 (and hover? (contains? lost id))    500
                                 (contains? lost id)                 400
                                 (and hover? (contains? started id)) 300
                                 (contains? started id)              200
                                 hover?                              100
                                 :else                               0)
                  sprite-top   (if (= @core/*last-puzzle-id id)
                                 500
                                 (-> random (* 5) js/Math.floor (* 100)))]]
      (when (< (+ (* y 18) x) (count puzzles))
        (.drawImage ctx img
          sprite-left sprite-top 100 100
          (+ 20 (* x 30) -10)
          (+ 140 (* y 30) -10) 50 50)))

    ;; progress
    (set! (.-font ctx) "16px font")
    (set! (.-textAlign ctx) "left")
    (set! (.-textBaseline ctx) "middle")
    (set! (.-fillStyle ctx) "#FFF")
    (let [won    (count (:won statuses))
          total  (count puzzles)
          pct    (-> won (/ total) (* 100) js/Math.round)
          text   (str won " / " total " (" pct "%)")
          text-w (:width (.measureText ctx text))
          left   (-> (quot safe-w 2) (- (quot text-w 2)))]
      (.fillText ctx text left (- safe-h 40))

      (set! (.-fillStyle ctx) "#2E4D6F")
      (.fillRect ctx left (- safe-h 25) text-w 3)
      (set! (.-fillStyle ctx) "#4FCD6F")
      (.fillRect ctx left (- safe-h 25) (-> text-w (* (/ won total)) js/Math.round) 3))))

(defn mouse->idx [x y]
  (when (core/inside? x y 20 140 (* 30 18) (* 30 23))
    (let [gx      (quot (- x 20) 30)
          gy      (quot (- y 140) 30)
          i       (+ (* gy 18) gx)
          puzzles (get core/puzzles-by-type type)]
      (when (< i (count puzzles))
        i))))

(defn on-pointer-move [e]
  (doseq [b buttons]
    (core/button-on-pointer-move b e))
  (let [{:keys [x y]} e
        hover-idx' (mouse->idx x y)]
    (when (not= hover-idx hover-idx')
      (set! hover-idx hover-idx')
      (core/request-render))))

(defn on-pointer-up [e]
  (doseq [b buttons]
    (core/button-on-pointer-up b e))
  (let [{:keys [x y start-x start-y]} e]
    (when (core/both-inside? start-x start-y x y 20 100 (* 30 18) (* 30 23))
      (when-some [idx (mouse->idx x y)]
        (let [id (-> core/puzzles-by-type (get type) (nth idx) :id)]
          (core/navigate [:game id]))))))

(assoc! core/screens :level-select
  {:on-enter        on-enter
   :on-render       on-render
   :on-pointer-move on-pointer-move
   :on-pointer-up   on-pointer-up
   :resources       #{"[V]5x5-10.txt" "[V]6x6-14.txt" "[V]7x7-20.txt" "[V]8x8-26.txt"
                      "level_select.png"}})
