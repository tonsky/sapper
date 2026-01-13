(ns sapper.loading
  (:require
   [sapper.core :as core :refer [ctx canvas-w canvas-h]]))

(defn on-render []
  (set! (.-font ctx) "24px sans-serif")
  (set! (.-fillStyle ctx) "#FFF")
  (set! (.-textAlign ctx) "center")
  (set! (.-textBaseline ctx) "middle")
  (.fillText ctx "Loading..." (/ canvas-w 2) (/ canvas-h 2)))

(assoc! core/screens :loading
  {:on-render on-render})
