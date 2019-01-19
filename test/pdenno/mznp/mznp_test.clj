(ns pdenno.mznp.mznp-test
  (:require [clojure.test :refer :all]
            [pdenno.mznp.mznp :refer :all])
  (:import (pdenno.mznp.mznp
            MznVarDecl)))

(deftest single-lines
  (let [parse-ok? #(-> (parse-string %1 %2) :error not)]
  (testing "type returned from parsing a short-ish string with a small grammar element."
    (is (instance? MznVarDecl  (parse-ok? :var-decl-item  " array[Workers, Tasks] of int: cost;"))))))

