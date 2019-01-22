(ns pdenno.mznp.mznp-test
  (:require [clojure.test :refer :all]
            [pdenno.mznp.mznp :refer :all])
  (:import (pdenno.mznp.mznp
            MznVarDecl)))

;;; POD refactor for simple parse-ok? and parse-string. 
(deftest single-lines
  (testing "type returned from parsing a short-ish string with a small grammar element."
    (is (parse-ok? :pdenno.mznp.mznp/include-item          "include \"alldifferent.mzn\""))
    (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "int: n = 3"))
    (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "set of int: Workers = 1..n"))
    (is (parse-ok? :pdenno.mznp.mznp/var-decl-item         "array[Workers, Tasks] of int: cost = [|10, 20, 13, |22, 11, 31, |14, 20, 18|]"))
    (is (parse-ok? :pdenno.mznp.mznp/constraint-item       "constraint alldifferent(doesTask)"))
    (is (parse-ok? :pdenno.mznp.mznp/solve-item            "solve minimize sum (w in Workers) (cost[w,doesTask[w]])"))
    (is (parse-ok? :pdenno.mznp.mznp/output-item           "output [show(doesTask),\"\\n\"]"))
    (is (parse-ok? :pdenno.mznp.mznp/set-ti-expr-tail      "set of int"))
    (is (parse-ok? :pdenno.mznp.mznp/gen-call-expr     "sum (w in Workers) (cost[w,doesTask[w]])"))
    (is (parse-ok? :pdenno.mznp.mznp/array-literal-2d      "[|10, 13, |22, 31, |14, 18|]")) 
    (is (not (parse-ok? :pdenno.mznp.mznp/array-literal-2d "[|10, 13, |22, 31, |14, 9, 18|]"))))) ; sublist must be equal size

;;; POD Use refactored parse-ok? here. Do spec work separately, starting at ::model. (default when not like parse-string usage).
(deftest whole-models
  (testing "that valid models compile okay."
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
    (is (-> (parse-file "data/opt5.mzn") :error not))
    (is (-> (parse-file "data/send-more-money.mzn") :error not))
    (is (-> (parse-file "data/simple-prod-planning.mzn") :error not))
    (is (-> (parse-file "data/social-golfers.mzn") :error not))
    (is (-> (parse-file "data/stable-marriage.mzn") :error not))
    (is (-> (parse-file "data/sudoku.mzn") :error not))))
