(ns pdenno.mznp.mzn-analyze
  "Functions to analyze mzn constraints"
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string       :as str]
            [clojure.set          :as sets]
            [clojure.spec-alpha2  :as s]
            [clojure.walk         :as walk]
            [pdenno.mznp.mznp     :as mznp]
            [pdenno.mznp.mzn-fns  :as mznf :refer (forall exists sum aref)]
            [pdenno.mznp.mzn-data :as mznd :refer (uget)]
            [pdenno.mznp.mzn-user :as mznu]))

(def diag (atom nil))

;;; Make this so that it defines a function that takes one or more missing vars
;;; (maybe just the decision variables???)
(defn intern-constraint
  "ueval a constraint, returning its function object."
  [info body]
  (reset! diag body)
  (let [dvars (->> info :core :var-decls vals (filter :var?) (map :name) (map symbol))]
    (mznd/user-eval
     `(fn [& {:keys [~@dvars]}]
        ~body))))

