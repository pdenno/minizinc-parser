(ns mznp.mzn-user
  "Not much here. It is where the user's data structures and code for constraints is interned."
  (:refer-clojure :exclude [range max min])
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.gen :as gen]
            [mznp.mzn-fns :as mznf])) ; Needed for eval

(defn run-constraint-loop
  "Run the constraint picked out by index into the constraint vector
   against sample data until interrupted, printing out accumulated results."
  [info cnum]
  (let [cfn (-> info :core :constraints (nth cnum) :fn)
        timer (atom (future (Thread/sleep 2000))) ; clj-kondo doesn't know future is in clojure.core?
        results (atom {:true 0 :false 0})]
    (while (not (.. Thread currentThread isInterrupted))
      (when-let [TeamsOnJob (gen/generate (s/gen ::TeamsOnJob))]
        (if (cfn :TeamsOnJob TeamsOnJob)
          (swap! results #(update % :true  inc))
          (swap! results #(update % :false inc)))
        (when (future-done? @timer)
          (println @results)
          (reset! timer (future (Thread/sleep 2000))))))))
