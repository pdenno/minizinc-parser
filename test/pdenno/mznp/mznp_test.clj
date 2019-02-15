(ns pdenno.mznp.mznp-test
  (:require [clojure.test :refer :all]
            [clojure.set :as sets]
            [pdenno.mznp.mznp :refer :all])
  (:import (pdenno.mznp.mznp
            MznVarDecl)))

"[ s[i] | i in 1..foo]"

;;; POD refactor for simple parse-ok? and parse-string. 
(deftest single-lines
  (testing "type returned from parsing a short-ish string."
    (let [db? @debugging?]
      (reset! debugging? false)
      (is (parse-ok? :pdenno.mznp.mznp/model                 "include \"alldifferent.mzn\"; int: i = 0;"))
      (is (parse-ok? :pdenno.mznp.mznp/model                 "var 0..total: end; % Hello, world! \n constraint end == 0; "))
      
      (is (parse-ok? :pdenno.mznp.mznp/include-item          "include \"alldifferent.mzn\""))
      (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "int: n = 3"))
      (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "set of int: Workers = 1..n"))
      (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "array[W, T] of int: cost = [|20, 13, |11, 31, |20, 18|]"))
      (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "var set of Items: knapsack"))
      (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "array [Jobs,Weeks] of var int: WorkersOnJob"))
      (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "array [Jobs,Weeks] of var 1..workforce_size: WorkersOnJob"))
      (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "int: digs = ceil(log(10.0,int2float(total)))"))
      (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "array[Resources] of var 0..max(capacity): used"))
      (is (parse-ok? :pdenno.mznp.mznp/constraint-item       "constraint alldifferent(doesTask)"))
      (is (parse-ok? :pdenno.mznp.mznp/solve-item            "solve minimize sum (w in Workers) (cost[w,doesTask[w]])"))
      (is (parse-ok? :pdenno.mznp.mznp/solve-item            "solve maximize sum (j in Jobs) (endWeek[j] - startWeek[j])"))
      (is (parse-ok? :pdenno.mznp.mznp/solve-item            "solve :: set_search(foo) satisfy"))
      (is (parse-ok? :pdenno.mznp.mznp/output-item           "output [show(doesTask),\"\\n\"]"))
      (is (parse-ok? :pdenno.mznp.mznp/output-item           "output [x] ++ [ s[i] | i in 1..n]"))
      (is (parse-ok? :pdenno.mznp.mznp/output-item           "output [\"end = \", show(end), \"\\n\"] ++
       [ show_int(digs,s[i,j]) ++ \" \" ++ if j == tasks then \"\\n\" else \"\" endif | i in 1..jobs, j in 1..tasks ]"))
      (is (parse-ok? :pdenno.mznp.mznp/expr                  "true"))

      (is (parse-ok? :pdenno.mznp.mznp/generator             "w in Workers"))
      (is (parse-ok? :pdenno.mznp.mznp/gen-call-expr         "sum (w in Workers) (cost[w,doesTask[w]])"))
      (is (parse-ok? :pdenno.mznp.mznp/gen-call-expr
                     "forall (i,j in Domain where i<j) (noattack(i, j, queens[i], queens[j]))"))
      (is (parse-ok? :pdenno.mznp.mznp/call-expr             "noattack(i, j, queens[i], queens[j])"))
      (is (parse-ok? :pdenno.mznp.mznp/set-ti-expr-tail      "set of int"))
      (is (parse-ok? :pdenno.mznp.mznp/base-ti-expr-tail     "int"))
      (is (parse-ok? :pdenno.mznp.mznp/array-literal-2d      "[|10, 13, |22, 31, |14, 18|]"))
      (is (parse-ok? :pdenno.mznp.mznp/array-comp            "[i + j | i, j in 1..3 where j < i]"))
      (is (parse-ok? :pdenno.mznp.mznp/array-comp            "[ s[i] | i in 1..n]"))
      (is (parse-ok? :pdenno.mznp.mznp/expr                  "[ s[i] | i in 1..n]"))
      (is (not (parse-ok? :pdenno.mznp.mznp/array-literal-2d "[|10, 13, |22, 31, |14, 9, 18|]"))) ; sublist must be equal size
      (is (parse-ok? :pdenno.mznp.mznp/if-then-else-expr     "if foo then bar else true endif"))

      (is (parse-ok? :pdenno.mznp.mznp/annotations           ":: set_search(foo)"))
      
      (reset! debugging? db?))))

;;; POD Use refactored parse-ok? here. Do spec work separately, starting at ::model. (default when not like parse-string usage).
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
    (is (-> (parse-file "data/sudoku.mzn") :error not))))

;;; POD Would be more useful to identify where an element is in any two of them. 
#_(deftest builtins-partition 
  (testing "That the builtins are partitioned."
    (is (empty? (sets/intersection builtin-bin-op
                                   builtin-un-op
                                   builtin-arithmetic-op
                                   builtin-logic-op)))))
