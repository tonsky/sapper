(ns sapper.macros)

(defmacro cond+ [& clauses]
  (when-some [[test expr & rest :as clause] clauses]
    (cond
      (= :do test)         `(do  ~expr (cond+ ~@rest))
      (= :let test)        `(let ~expr (cond+ ~@rest))
      (= 1 (count clause)) test
      :else                `(if ~test ~expr (cond+ ~@rest)))))
