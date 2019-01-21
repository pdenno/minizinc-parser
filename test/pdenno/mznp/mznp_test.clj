(ns pdenno.mznp.mznp-test
  (:require [clojure.test :refer :all]
            [pdenno.mznp.mznp :refer :all])
  (:import (pdenno.mznp.mznp
            MznVarDecl)))


(deftest single-lines
  (testing "type returned from parsing a short-ish string with a small grammar element."
    (is (parse-ok? :pdenno.mznp.mznp/var-decl-item     " array[Workers, Tasks] of int: cost;"))
    (is (parse-ok? :pdenno.mznp.mznp/set-ti-expr-tail  "set of int"))
    (is (parse-ok? :pdenno.mznp.mznp/array-literal-2d  "[|10, 13, |22, 31, |14, 18|]")) ; sublist must be equal size
    (is (not (parse-ok? :pdenno.mznp.mznp/array-literal-2d  "[|10, 13, |22, 31, |14, 9, 18|]"))))) ; sublist must be equal size




