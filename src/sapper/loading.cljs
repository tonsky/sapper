(ns sapper.loading
  (:require
   [sapper.core :as core :refer [ctx canvas-w canvas-h]]))

(def *show-text?
  (atom false))

(defn on-enter []
  (reset! *show-text? false)
  (core/set-timeout 500
    #(do
       (reset! *show-text? true)
       (core/request-render))))

(defn on-render []
  (when @*show-text?
    (set! (.-font ctx) "bold 24px sans-serif")
    (set! (.-fillStyle ctx) "#FFF")
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (.fillText ctx "Loading..." (/ canvas-w 2) (/ canvas-h 2))))

(assoc! core/screens :loading
  {:on-enter on-enter
   :on-render on-render})
