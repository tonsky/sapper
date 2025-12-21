(ns sapper.macros)

(defmacro defn-log [name args & body]
  `(defn ~name ~args
     (let [start#  (js/performance.now)
           result# (do ~@body)
           end#    (js/performance.now)]
       #_(println ~(str name) (js/Math.round (- end# start#)) "ms")
       result#)))

(defmacro cond+ [& clauses]
  (when-some [[test expr & rest :as clause] clauses]
    (cond
      (= :do test)         `(do  ~expr (cond+ ~@rest))
      (= :let test)        `(let ~expr (cond+ ~@rest))
      (= 1 (count clause)) test
      :else                `(if ~test ~expr (cond+ ~@rest)))))
