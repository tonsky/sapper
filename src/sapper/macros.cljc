(ns sapper.macros)

(defmacro cond+ [& clauses]
  (when-some [[test expr & rest :as clause] clauses]
    (cond
      (= :do test)         `(do  ~expr (cond+ ~@rest))
      (= :let test)        `(let ~expr (cond+ ~@rest))
      (= 1 (count clause)) test
      :else                `(if ~test ~expr (cond+ ~@rest)))))

(defmacro either [f & args]
  (let [first-args     (butlast args)
        second-arg     (last args)
        second-arg-sym (gensym "second-arg")]
    `(let [~second-arg-sym ~second-arg]
       (or
         ~@(for [first-arg first-args]
             `(~f ~first-arg ~second-arg-sym))))))
