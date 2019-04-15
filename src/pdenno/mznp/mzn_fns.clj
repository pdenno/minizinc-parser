(ns pdenno.mznp.mzn-fns
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc functions (generator exps, etc.)"
  (:refer-clojure :exclude [range max min])
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
  (= (count vals) (-> vals distinct count)))

(defmacro forall [args where body]
  `(for ~@args (when ~where ~body)))

(defmacro sum [args where body]
  `(let [current# (atom 0)]
     (for ~@args
       (when ~where (swap! current# #(+ % ~body))))
     (deref current#)))

(defmacro max [args where body]
  `(let [current# (atom false)]
     (for ~@args
       (when ~where
         (swap! current# #(let [bigger?# ~body]
                            (if (or (-> current# deref not)
                                    (> bigger?# %))
                              bigger?#
                              %)))))
     (deref current#)))

(defmacro min [args where body]
  `(let [current# (atom false)]
     (for ~@args
       (when ~where
         (swap! current# #(let [smaller?# ~body]
                            (if (or (-> current# deref not)
                                    (< smaller?# %))
                              smaller?#
                              %)))))
     (deref current#)))

(defn small-forall [Weeks, Jobs, LineOfJob TeamLowsByLine TeamHighsByLine workforceSize]
  (forall [[w Weeks]]
          true
          (<= (sum [[j Jobs]]
                   true
                   (* (+ (aref TeamLowsByLine (aref LineOfJob j)) (aref TeamHighsByLine (aref LineOfJob j)))
                      (aref TeamsOnJob j w)))
              workforceSize)))



(defn big-forall [Weeks, Jobs, LineOfJob TeamLowsByLine TeamHighsByLine workforceSize]
  (forall
   [[lin Lines] [w1 w2 Weeks]]
   (and (and (< w1 w2) (forall [[j Jobs]]
                               true
                               (and (= (aref LineOfJob j) lin)
                                    (not= (aref TeamsOnJob j w1) 0))))
        (= w2
            (max
             [[j Jobs] [w (range (+ w1 1) schedulingWeeks)]]
             true
             (if
                 (and (= (aref LineOfJob j) lin)
                      (not= (aref TeamsOnJob j w) 0))
               w
               0))))
   (= (sum [[j Jobs]]
            true
            (if (= (aref LineOfJob j) lin) (aref TeamsOnJob j w1) 0))
       (sum [[j Jobs]] true
            (if (= (aref LineOfJob j) lin) (aref TeamsOnJob j w2) 0)))))


   
          
