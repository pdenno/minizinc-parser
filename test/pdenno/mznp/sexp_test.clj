(ns pdenno.mznp.sexp-test
  "Simplify the parsed structure using s-expressions in some places."
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [pdenno.mznp.mznp :as mznp]
            [pdenno.mznp.sexp :refer :all]))

(deftest simple-sexp
  (testing "Test atomic rewriting tasks."
    (let [db? @debugging?
          mznp-db? @mznp/debugging?]
      (reset! debugging? false)
      (reset! mznp/debugging? false)
      (is (= (reduce-bin-ops-str "4 * 3 + 2 -1")
             {:type :MznExpr, :bin-ops [4 \* 3 \+ 2 \- 1]}))
      (reset!      debugging? db?)
      (reset! mznp/debugging? mznp-db?))))


#_(test-rewrite
 ::mznp/gen-call-expr
  "sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif)")


#_(test-rewrite
 ::mznp/constraint-item
 "constraint forall (lin in Lines, w1, w2 in Weeks
                        where w1 < w2                                       /\\
                        forall (j in Jobs) ((LineOfJob[j] = lin) /\\ (WorkersOnJob[j,w1] != 0))  /\\
                        (w2 == max (j in Jobs, w in (w1+1)..numWeeksScheduled)
	                (if ((LineOfJob[j] == lin) /\\ (WorkersOnJob[j,w] != 0)) then w else 0 endif)))
             ((sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif))
              ==
              (sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w2] else 0 endif)))"
      :debug? true)


