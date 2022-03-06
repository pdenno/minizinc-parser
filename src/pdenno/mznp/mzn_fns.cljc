(ns pdenno.mznp.mzn-fns
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc functions (generator exps, etc.)")

;; POD ToDo: Can't mzn-max mzn-min use c/min c/max.

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

(defn mzn-range [start stop]
  "Create a clojure vector of values specifed by the MiniZinc range args."
  (vec (range start (inc stop))))

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
