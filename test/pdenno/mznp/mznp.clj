(ns pdenno.mznp.mznp-test
  (:require [clojure.test :refer :all]
            [pdenno.mznp :refer :all]
            [clojure.set :as sets]))

(deftest builtins-partition
  (testing "That the builtins are partitioned."
    (is (empty? (sets/intersection builtin-bin-op
                                   builtin-un-op
                                   builtin-arithmetic-op
                                   builtin-logic-op)))))

