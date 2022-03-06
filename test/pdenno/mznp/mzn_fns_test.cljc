(ns pdenno.mznp.mzn-fns-test
  "Test mzn-fn functions and macros."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [pdenno.mznp.rewrite :as rewrite]
            [pdenno.mznp.mzn-fns :refer :all]))

;;; POD 2021-10-28 Needs investigation!
#_(deftest mzn-fns
  (testing "that functions work as expected"
    (is (= 6 (sum [w [1 2 3]] true k)))
    (is (= 150 (let [a [10 20 30 40 50]]
                 (sum [[i (range 0 4)]] true (aref a i)))))))
