(ns pdenno.mznp.mzn-user
  "Not much here. It is where the user's data structures and code for constraints is interned."
  (:refer-clojure :exclude [range max min])
  (:require [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec-alpha2 :as s]
            [clojure.spec-alpha2.gen :as gen]
            [clojure.spec-alpha2.test :as test]
            [pdenno.mznp.mzn-fns :as mznf :refer :all]))

(def diag (atom nil))

(defn run-constraint-loop
  "Run the constraint picked out by index into the constraint vector
   against sample data until interrupted, printing out accumulated results."
  [info cnum]
  (let [cfn (-> info :core :constraints (nth cnum) :fn)
        timer (atom (future (Thread/sleep 2000)))
        results (atom {:true 0 :false 0})]
    (while (not (.. Thread currentThread isInterrupted))
      (when-let [TeamsOnJob (gen/generate (s/gen ::TeamsOnJob))]
        (if (cfn :TeamsOnJob TeamsOnJob)
          (swap! results #(update % :true  inc))
          (swap! results #(update % :false inc)))
        (when (future-done? @timer)
          (println @results)
          (reset! timer (future (Thread/sleep 2000))))))))


(def toj [[0, 20, 20, 20, 20, 20, 20, 20],
          [0, 20, 20, 20, 20, 20, 20, 20],
          [0, 20, 20, 20, 20, 20, 20, 20],
          [0, 1, 20, 20, 20, 20, 20, 20],
          [0, 0, 18, 18, 20, 20, 20, 20],
          [0, 0, 0, 0, 14, 18, 18, 18],
          [0, 1, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [18, 0, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [20, 19, 0, 0, 0, 0, 0, 0],
          [20, 17, 0, 0, 0, 0, 0, 0],
          [20, 20, 20, 20, 4, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [6, 0, 0, 0, 0, 0, 0, 0],
          [2, 0, 0, 0, 0, 0, 0, 0],
          [2, 0, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [2, 0, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [3, 0, 0, 0, 0, 0, 0, 0],
          [4, 0, 0, 0, 0, 0, 0, 0],
          [4, 0, 0, 0, 0, 0, 0, 0],
          [4, 0, 0, 0, 0, 0, 0, 0],
          [1, 0, 0, 0, 0, 0, 0, 0],
          [2, 0, 0, 0, 0, 0, 0, 0]])
