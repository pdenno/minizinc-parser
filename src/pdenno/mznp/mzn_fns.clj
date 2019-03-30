(ns pdenno.mznp.mzn-fns
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc functions (generator exps, etc.)"
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]))
      
(defn range-op [start stop]
  (range start stop))
