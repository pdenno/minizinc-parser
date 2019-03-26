(ns pdenno.mznp.sexp-test
  "Simplify the parsed structure using s-expressions in some places."
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [pdenno.mznp.mznp :as mznp]
            [pdenno.mznp.sexp :refer :all]))

(defmacro debug-off [& body]
   `(let [db?# @debugging?
          mznp-db?# @mznp/debugging?]
      (reset! debugging?      false)
      (reset! mznp/debugging? false)
      ~@body
      (reset!      debugging? db?#)
      (reset! mznp/debugging? mznp-db?#)))

(deftest simple-sexp
  (testing "Test atomic rewriting tasks."
    (debug-off
     (is (= (rewrite* ::mznp/expr "1") 1))
     (is (= (rewrite* ::mznp/expr "1 + x") '(+ 1 x)))
     (is (= (rewrite* ::mznp/expr "(1 + x)") '(+ 1 x))))))

(deftest reducing-bin-ops
  (testing "Test atomic rewriting tasks."
    (debug-off
      (is (= (form-bin-ops* "1 + 2 * 3 + 4 * 5")
             '(+ (+ 1 (* 2 3)) (* 4 5))))
      (is (= (form-bin-ops* "x + 2 * 3 + 4 * 5")
             '(+ (+ x (* 2 3)) (* 4 5))))
      (is (= (form-bin-ops* "1 + 2 * 3 + 4 * 5" :reduce? true)
             {:type :MznExpr, :bin-ops [1 \+ 2 \* 3 \+ 4 \* 5]}))
      (is (= (-> (rewrite* ::mznp/expr "1 + 2 * 3")) '(+ 1 (* 2 3))))
      ;; Some with primaries
      (is (= (form-bin-ops* "3 * (1 + 2)") '(* 3 (+ 1 2))))
      (is (= (form-bin-ops* "(1 + 2) * 3") '(* (+ 1 2) 3)))
      ;;; Some array references
      (is (= (form-bin-ops*  "a * b[i]")    '(* a (mzn-array-access b i))))
      (is (= (form-bin-ops*  "a * b[i]")    '(* a (mzn-array-access b i))))
      (is (= (form-bin-ops*  "a[i] * b[i]") '(* (mzn-array-access a i) (mzn-array-access b i))))
      (is (= (form-bin-ops*  "a[i] * b")    '(* (mzn-array-access a i) b))))))

(deftest bigger-rewriting-tasks
  (debug-off
   (is (= (rewrite*
           ::mznp/gen-call-expr
           "sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif)")
          '(sum  [[j Jobs]]  :where true
                 (if  (== (mzn-array-access LineOfJob j) lin) (mzn-array-access WorkersOnJob j w1) 0))))
   
   (is (= (rewrite*
           ::mznp/constraint-item
           "constraint forall (lin in Lines, w1, w2 in Weeks
                        where w1 < w2                                       /\\
                        forall (j in Jobs) ((LineOfJob[j] = lin) /\\ (WorkersOnJob[j,w1] != 0))  /\\
                        (w2 == max (j in Jobs, w in (w1+1)..numWeeksScheduled)
	                (if ((LineOfJob[j] == lin) /\\ (WorkersOnJob[j,w] != 0)) then w else 0 endif)))
             ((sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif))
              ==
              (sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w2] else 0 endif)))")
          ;; POD This is pretty close to what I want, but there is ":and-op" in some places.
          '(forall [[lin Lines] [w1 w2 Weeks]]
                   :where (and-op (:and-op (< w1 w2) ; <========== :and-op ALSO ARE THE TWO CORRECT??? 
                                           (forall [[j Jobs]] :where true
                                                   (and-op
                                                    (assign (mzn-array-access LineOfJob j) lin)
                                                    (not= (mzn-array-access WorkersOnJob j w1) 0))))
                                  (== w2
                                      (max
                                       [[j Jobs] [w (..-op (+ w1 1) numWeeksScheduled)]]
                                       :where
                                       true
                                       (if
                                           (and-op
                                            (== (mzn-array-access LineOfJob j) lin)
                                            (not= (mzn-array-access WorkersOnJob j w) 0))
                                         w
                                         0))))
                   (==
                    (sum  [[j Jobs]] :where true
                          (if  (== (mzn-array-access LineOfJob j) lin)
                            (mzn-array-access WorkersOnJob j w1)
                            0))
                    (sum [[j Jobs]] :where true
                         (if
                             (== (mzn-array-access LineOfJob j) lin)
                           (mzn-array-access WorkersOnJob j w2)
                           0))))))))


