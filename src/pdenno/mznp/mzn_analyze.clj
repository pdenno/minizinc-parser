(ns pdenno.mznp.mzn-analyze
  "Functions to analyze mzn constraints"
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string       :as str]
            [clojure.set          :as sets]
            [clojure.spec-alpha2  :as s]
            [pdenno.mznp.mznp     :as mznp]
            [pdenno.mznp.mzn-fns  :as mznf :refer (forall sum aref)]
            [pdenno.mznp.mzn-data :as mznd :refer (uget)]
            [pdenno.mznp.mzn-user :as mznu]))

;;; Make this so that it defines a function that takes one or more missing vars
;;; (maybe just the decision variables???)
(defn instrument
  "ueval a constraint"
  [info c-num]
  (let [dvars (->> info :core :var-decls vals (filter :var?) (map :name) (map symbol))
        body  (->  info :core :constraints (nth c-num))]
    (mznd/user-eval
     `(def ~(symbol (str "constraint-" c-num))
        (fn [~@dvars]
          ~body)))))



(instrument [info
            (forall [[w Weeks]]
                    true
                    (<= (sum [[j Jobs]]
                             true
                             (* (+ (aref TeamLowsByLine (aref LineOfJob j)) (aref TeamHighsByLine (aref LineOfJob j)))
                                (aref TeamsOnJob j w)))
                        workforceSize)))

