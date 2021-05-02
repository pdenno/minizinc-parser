(ns minizinc.mznp.mzn-fns-test
  "Test mzn-fn functions and macros."
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [minizinc.mznp.sexp :as sexp]
            [minizinc.mznp.mzn-fns :refer :all]))

(deftest mzn-fns
  (testing "that functions work as expected"
    (is (= 6 (sum [[w [1 2 3]] true k))))
    (is (= 150 (let [a [10 20 30 40 50]]
                 (sum [[i (range 0 4)]] true (aref a i)))))))

                  

