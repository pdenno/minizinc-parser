(ns pdenno.mznp.mzn-analyze
  "Functions to analyze mzn constraints"
  (:require
   [pdenno.mznp.mzn-data :as mznd]))

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
