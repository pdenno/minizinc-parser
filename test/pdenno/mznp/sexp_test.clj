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
      (is (= (reduce-bin-ops* "1 + 2 * 3 + 4 * 5")
             '(+ (+ 1 (* 2 3)) (* 4 5))))
      (is (= (reduce-bin-ops* "x + 2 * 3 + 4 * 5")
             '(+ (+ {:type :MznId, :name "x"} (* 2 3)) (* 4 5))))
      (is (= (reduce-bin-ops* "1 + 2 * 3 + 4 * 5" :reduce? true)
             {:type :MznExpr, :bin-ops [1 \+ 2 \* 3 \+ 4 \* 5]}))
      (is (= (-> (rewrite* ::mznp/expr "1 + 2 * 3")) '(+ 1 (* 2 3))))
      ;; Some with primaries
      (is (= (reduce-bin-ops* "3 * (1 + 2)") '(* 3 (+ 1 2))))
      (is (= (reduce-bin-ops* "(1 + 2) * 3") '(* (+ 1 2) 3)))
      (reset!      debugging? db?)
      (reset! mznp/debugging? mznp-db?))))

(deftest primary-sexp
  (testing "Test handling of mznp :primary expression."
    (let [db? @debugging?
          mznp-db? @mznp/debugging?]
      (reset! debugging? false)
      (reset! mznp/debugging? false)
      (is (= 
           (rewrite* ::mznp/expr "1 + 2" :simplify? true)
           {:type :MznExpr,
            :atom {:type :MznExprAtom, :head 1},
            :tail
            {:type :MznExprBinopTail,
             :bin-op \+,
             :expr {:type :MznExpr, :atom {:type :MznExprAtom, :head 2}}}}))
      (is (= true (primary? (rewrite* ::mznp/expr "(1 + 2)" :simplify? true))))
      (reset!      debugging? db?)
      (reset! mznp/debugging? mznp-db?))))

#_(rewrite*
 ::mznp/gen-call-expr
  "sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif)")


#_(rewrite*
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


