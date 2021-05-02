(ns minizinc.mznp.mznp-test
  (:require [clojure.test :refer :all]
            [clojure.set :as sets]
            [minizinc.mznp.mznp :refer :all :as mznp])
  (:import (minizinc.mznp.mznp
            MznVarDecl)))

(deftest single-lines
  (testing "type returned from parsing a short-ish string."
    (let [db? @debugging?]
      (reset! debugging? false)
      (is (parse-ok? ::mznp/model                 "include \"all_different.mzn\"; int: i = 0;"))
      (is (parse-ok? ::mznp/model                 "var 0..total: end; % Hello, world! \n constraint end == 0; "))
      
      (is (parse-ok? ::mznp/include-item          "include \"all_different.mzn\""))
      (is (parse-ok? ::mznp/var-decl-item         "int: n = 3"))
      (is (parse-ok? ::mznp/var-decl-item         "set of int: Workers = 1..n"))
      (is (parse-ok? ::mznp/var-decl-item         "array[W, T] of int: cost = [|20, 13, |11, 31, |20, 18|]"))
      (is (parse-ok? ::mznp/var-decl-item         "var set of Items: knapsack"))
      (is (parse-ok? ::mznp/var-decl-item         "array [Jobs,Weeks] of var int: WorkersOnJob"))
      (is (parse-ok? ::mznp/var-decl-item         "array [Jobs,Weeks] of var 1..workforce_size: WorkersOnJob"))
      (is (parse-ok? ::mznp/var-decl-item         "int: digs = ceil(log(10.0,int2float(total)))"))
      (is (parse-ok? ::mznp/var-decl-item         "array[Resources] of var 0..max(capacity): used"))
      (is (parse-ok? ::mznp/constraint-item       "constraint all_different(doesTask)"))
      (is (parse-ok? ::mznp/solve-item            "solve minimize sum (w in Workers) (cost[w,doesTask[w]])"))
      (is (parse-ok? ::mznp/solve-item            "solve maximize sum (j in Jobs) (endWeek[j] - startWeek[j])"))
      (is (parse-ok? ::mznp/solve-item            "solve :: set_search(foo) satisfy"))
      (is (parse-ok? ::mznp/output-item           "output [show(doesTask),\"\\n\"]"))
      (is (parse-ok? ::mznp/output-item           "output [x] ++ [ s[i] | i in 1..n]"))
      (is (parse-ok? ::mznp/output-item           "output [\"end = \", show(end), \"\\n\"] ++
       [ show_int(digs,s[i,j]) ++ \" \" ++ if j == tasks then \"\\n\" else \"\" endif | i in 1..jobs, j in 1..tasks ]"))
      (is (parse-ok? ::mznp/expr                  "true"))
      (is (parse-ok? ::mznp/expr                  "x / 3"))

      (is (parse-ok? ::mznp/generator             "w in Workers"))
      (is (parse-ok? ::mznp/gen-call-expr         "sum (w in Workers) (cost[w,doesTask[w]])"))
      (is (parse-ok? ::mznp/gen-call-expr         "max (w1 in Weeks where TeamsOnJob[j,w1] != 0) (w1)"))
      (is (parse-ok? ::mznp/gen-call-expr         "forall (i,j in Domain where i<j) (noattack(i, j, queens[i], queens[j]))"))
      (is (parse-ok? ::mznp/call-expr             "noattack(i, j, queens[i], queens[j])"))
      (is (parse-ok? ::mznp/set-ti-expr-tail      "set of int"))
      (is (parse-ok? ::mznp/base-ti-expr-tail     "int"))
      (is (parse-ok? ::mznp/array-literal-2d      "[|10, 13, |22, 31, |14, 18|]"))
      (is (parse-ok? ::mznp/array-comp            "[i + j | i, j in 1..3 where j < i]"))
      (is (parse-ok? ::mznp/array-comp            "[ s[i] | i in 1..n]"))
      (is (parse-ok? ::mznp/expr                  "[ s[i] | i in 1..n]"))
      (is (not (parse-ok? ::mznp/array-literal-2d "[|10, 13, |22, 31, |14, 9, 18|]"))) ; sublist must be equal size
      (is (parse-ok? ::mznp/if-then-else-expr     "if foo then bar else baz endif"))

      (is (parse-ok? ::mznp/annotations           ":: set_search(foo)"))
      
      (reset! debugging? db?))))

(deftest whole-models
  (testing "that valid models compile okay."
    (is (-> (parse-file "data/simplest.mzn") :error not))
    (is (-> (parse-file "data/peckham.mzn") :error not))
    (is (-> (parse-file "data/assignment-inv.mzn") :error not))
    (is (-> (parse-file "data/assignment.mzn") :error not))
    (is (-> (parse-file "data/assignment-zero-one.mzn") :error not))
    (is (-> (parse-file "data/aust.mzn") :error not))
    (is (-> (parse-file "data/cakes2.mzn") :error not))
    (is (-> (parse-file "data/cakes.mzn") :error not))
    (is (-> (parse-file "data/jobshop.mzn") :error not))
    (is (-> (parse-file "data/knapsack.mzn") :error not))
    (is (-> (parse-file "data/laplace.mzn") :error not))
    (is (-> (parse-file "data/loan.mzn") :error not))
    (is (-> (parse-file "data/send-more-money.mzn") :error not))
    (is (-> (parse-file "data/simple-prod-planning.mzn") :error not))
    (is (-> (parse-file "data/social-golfers.mzn") :error not))
    (is (-> (parse-file "data/stable-marriage.mzn") :error not))
    (is (-> (parse-file "data/sudoku.mzn") :error not))
    (is (-> (parse-file "data/penalty.mzn") :error not))))

;;; POD Would be more useful to identify where an element is in any two of them. 
#_(deftest builtins-partition 
  (testing "That the builtins are partitioned."
    (is (empty? (sets/intersection builtin-bin-op
                                   builtin-un-op
                                   builtin-arithmetic-op
                                   builtin-logic-op)))))

;;; This is used to find new builtins from Mzn source code. 
#_(defn get-operators []
  "Throwaway to collect operators from builtins.mzn"
  (let [pattern (re-pattern #"^function\s+.+\s+(\')?([\w,\d,\_]+)(\')?\s*\(.*$")]
    (->> (slurp "data/minizinc-git/builtins.mzn")
         clojure.string/split-lines
         (filter #(re-matches pattern %))
         (map    #(re-matches pattern %))
         (map #(nth % 2))
         distinct
         sort)))

