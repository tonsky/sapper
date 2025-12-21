(ns sapper.macros)

(defmacro defn-log [name args & body]
  `(defn ~name ~args
     (let [start#  (js/performance.now)
           result# (do ~@body)
           end#    (js/performance.now)]
       (println ~(str name) (- end# start#) "ms")
       result#)))
