(ns sapper.macros)

(defmacro defn-log [name args & body]
  `(defn ~name ~args
     (let [start#  (js/performance.now)
           result# (do ~@body)
           end#    (js/performance.now)]
       #_(println ~(str name) (js/Math.round (- end# start#)) "ms")
       result#)))
