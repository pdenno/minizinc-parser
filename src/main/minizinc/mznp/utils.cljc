(ns minizinc.mznp.utils
  "Parse MiniZinc to records."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec.alpha :as s]))

(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n))
  ;;(subs <a long string> 0 n))         about as good.
  ;;(cl-format nil "~v{~A~:*~}" n " ")) slower  for 20 spaces
  ;;(apply str (repeat n " ")))         slowest for 20 spaces 
  )

(defn keysym [keyw] ; POD get rid of this. Too simple!
  "Turn a keyword into a symbol."
  (symbol (name keyw)))

;;;=== General =========================
(defn ppp []
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint *1)))

(defn ppprint [arg]
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint arg)))

(defn break
  ([] (throw (ex-info "Break!" {})))
  ([text] (throw (ex-info text {})))
  ([text args] (throw (ex-info text args))))




