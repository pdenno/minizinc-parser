(ns pdenno.mznp.mzn-fns
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc functions (generator exps, etc.)"
  (:refer-clojure :exclude [range])
  (:require [clojure.string :as str]
            [clojure.set    :as sets]))

(alias 'c 'clojure.core)

(defn range [start stop]
  "Create a clojure vector of values specifed by the MiniZinc range args."
  (vec (c/range start (inc stop))))

(defn aref
  "Return the value of the MiniZinc array at the argument indexes."
  [array & indexes]
  (reduce (fn [a ix] (nth a ix)) array indexes))

(defn alldifferent
  "Returns true if all values are different."
  [vals]
  (== (count vals) (-> vals distinct count)))
          
