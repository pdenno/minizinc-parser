(ns pdenno.mznp.mzn-fns
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc functions (generator exps, etc.)"
  (:refer-clojure :exclude [range max min])
  (:require [clojure.string :as str]
            [clojure.walk   :as walk]
            [clojure.set    :as sets]))

(alias 'c 'clojure.core)

(defn range [start stop]
  "Create a clojure vector of values specifed by the MiniZinc range args."
  (vec (c/range start (inc stop))))

#_(defn aref
  "Return the value of the MiniZinc array at the argument indexes."
  [array & indexes]
    (reduce (fn [a ix] (nth a ix)) array indexes))

;;; POD It was probably wrong to use arrays for arrays. Because index sets can
;;;     be anything, I should have used maps. But for now.
(defn aref
  "Return the value of the MiniZinc array at the argument indexes."
  ([array ix]
   ;(println "array = " array "ix = " ix)
   (nth array (dec ix)))
  ([array ix1 ix2]
   ;(println "array = " array "ix1 = " ix1 "ix2 = " ix2)
   (nth (nth array (dec ix1)) (dec ix2))))

(defn alldifferent
  "Returns true if all values are different."
  [vals]
  (= (count vals) (-> vals distinct count)))

(defn for-args
  "Args can look like e.g. [[lin Lines] [w1 w2 Weeks]]. They need to match syntax of clojure/for."
  [args]
  (let [split (reduce (fn [res arg-set]
                        (let [vars (butlast arg-set)
                              src  (last arg-set)]
                          (into res (mapv #(vector % src) vars))))
                      []
                      args)]
    (reduce (fn [res v]
              (-> res
                  (conj (first v))
                  (conj (second v))))
            [] split)))
               
(defmacro forall [args where body]
  `(let [current# (atom true)]
     (doseq ~(for-args args)
        (when (deref current#) ; POD doesn't exit early. 
          (when ~where (swap! current# (fn [arg#] ~body))))) ; arg not used.
     (deref current#)))

(defmacro exists [args where body]
  `(let [current# (atom false)]
     (doseq ~(for-args args)
       (when (-> current# deref not) ; POD doesn't exit early. 
         (when ~where (swap! current# (fn [arg#] ~body))))) ; arg not used.
     (deref current#)))

(defmacro sum [args where body]
  `(let [current# (atom 0)]
     (doseq ~(for-args args)
        (when ~where (swap! current# (fn [arg#] (+ arg# ~body)))))
     (deref current#)))

(defmacro max [args where body]
  `(let [current# (atom false)]
     (doseq ~(for-args args)
       (when ~where
         (swap! current# #(let [bigger?# ~body]
                            (if (or (-> current# deref not)
                                    (> bigger?# %))
                              bigger?#
                              %)))))
     (deref current#)))

(defmacro min [args where body]
  `(let [current# (atom false)]
     (doseq ~(for-args args)
       (when ~where
         (swap! current# #(let [smaller?# ~body]
                            (if (or (-> current# deref not)
                                    (< smaller?# %))
                              smaller?#
                              %)))))
     (deref current#)))
