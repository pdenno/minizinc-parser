(ns pdenno.mznp.mzn-fns
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc functions (generator exps, etc.)"
  (:refer-clojure :exclude [range max min])
  (:require [clojure.string :as str]
            [clojure.walk   :as walk]
            [clojure.set    :as sets]))

(alias 'c 'clojure.core)

(def built-in-globals
  #{"all_different"  "all_disjoint" "all_equal" "nvalue" "symmetric_all_different"
    "lex_greater" "lex_greatereq" "lex_less" "lex_lesseq" "seq_precede_chain"
    "strict_lex2" "value_precede" "value_precede_chain" "arg_sort" "decreasing"
    "increasing" "sort" "int_set_channel" "inverse" "inverse_set" "link_set_to_booleans"
    "among" "at_least" "at_most" "at_most1" "count" "count_eq" "count_geq" "count_gt"
    "count_leq" "count_lt" "count_neq" "distribute" "exactly" "global_cardinality"
    "global_cardinality_closed" "global_cardinality_low_up" "global_cardinality_low_up_closed"
    "bin_packing" "bin_packing_capa" "bin_packing_load" "diffn" "diffn_nonstrict"
    "diffn_nonstrict_k" "geost" "geost_bb" "geost_nonoverlap_k" "geost_smallest_bb" "knapsack"
    "alternative" "cumulative" "disjunctive" "disjunctive_strict" "span" "bounded_dpath"
    "bounded_path" "connected" "d_weighted_spanning_tree" "dag" "dconnected" "dpath"
    "dreachable" "dsteiner" "dtree" "path" "reachable" "steiner" "subgraph" "tree"
    "weighted_spanning_tree" "cost_mdd" "cost_regular" "mdd" "regular" "regular_nfa"
    "table"})

(defn range [start stop]
  "Create a clojure vector of values specifed by the MiniZinc range args."
  (vec (c/range start (inc stop))))

;;; POD It was probably wrong to use arrays for arrays. Because indexing can
;;;     be anything, I should have used maps. But for now.
;;; 2020: Really about the indexing???
(defn aref
  "Return the value of the MiniZinc array at the argument indexes."
  ([array ix]
   (nth array (dec ix)))
  ([array ix1 ix2]
   (nth (nth array (dec ix1)) (dec ix2))))

(defn all_different
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
