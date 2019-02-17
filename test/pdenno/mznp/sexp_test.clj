(ns pdenno.mznp.sexp-test
  "Simplify the parsed structure using s-expressions in some places."
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [pdenno.mznp.mznp :as mznp]
            [pdenno.mznp.sexp :refer :all]))

(deftest simple-sexp
  (testing "type returned from parsing a short-ish string."
    (let [db? @debugging?
          mznp-db? @mznp/debugging?]
      (reset! debugging? false)
      (reset! mznp/debugging? false)
      (is (= (test-rewrite ::mznp/expr "1*2+3")   '(+ (* 1 2) 3)))
      (is (= (test-rewrite ::mznp/expr "1+2*3")   '(+ 1 (* 2 3))))
      (is (= (test-rewrite ::mznp/expr "1+2*3+4") '(+ (+ 1 (* 2 3)) 4)))

      (reset!      debugging? db?)
      (reset! mznp/debugging? mznp-db?))))

