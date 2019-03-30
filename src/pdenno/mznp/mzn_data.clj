(ns pdenno.mznp.mzn-data
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc data structures"
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.walk :as walk]
            [clojure.spec-alpha2 :as s]
            [clojure.spec-alpha2.gen :as gen]
            [clojure.spec-alpha2.test :as test]
            [pdenno.mznp.mzn-fns :as mznf]))

;;; ((user-intern "x") 2)
;;; ((user-intern "y") 3)
;;; (user-eval '(+ x y))
(defn user-intern
  "Provide a namestring of a variable to intern in mzn-user; the returned function takes one
   argument, the value to set the variable to."
  [sym-str]
  (fn [arg]
    (binding [*ns* (find-ns (symbol "mzn-user"))]
      (intern (find-ns (symbol "mzn-user"))
              (symbol sym-str)
              arg))))

;;; POD It would be nice to say *what symbol* is unresolved. In tracking this down, 
;;; of course, I will have to watch for cycles. 
(defn user-eval
  "Do clojure eval in namespace mzn-user.
   If the sexp has unresolvable symbols, catch them and return :unresolved-symbol."
  [form]
  (binding [*ns* (find-ns (symbol "mzn-user"))]
     (eval form)))

;;; (-> var-decl :vartype :datatype #{:int :float :mzn-set :mzn-array :mzn-2d-array})
(defn make-data-dispatch [var-decl] (-> var-decl :vartype :datatype))

(defmulti make-data "Make a clojure object representing the MiniZinc object." #'make-data-dispatch)

;;; (make-data {:name "n", :vartype {:datatype :int}, :init 3})
(defmethod make-data :int 
  [var-decl]
  ((user-intern (:name var-decl)) (-> var-decl :init user-eval)) 
  (s/register (keyword "mzn-user" (:name var-decl)) (s/spec integer?)))

;;; (make-data {:name "n", :vartype {:datatype :float}, :init 3.0})
(defmethod make-data :float
  [var-decl]
  ((user-intern (:name var-decl)) (-> var-decl :init user-eval))
  (s/register (keyword "mzn-user" (:name var-decl)) (s/spec float?)))

(def btype2clj {:int ::int, :float ::float :string ::string})

(s/def ::int integer?)

;;; (make-data {:name "Workers",
;;;             :vartype {:datatype :mzn-set, :base-type :int},
;;;             :init (range-op 1 n)},
(defmethod make-data :mzn-set
  [var-decl]
  ((user-intern (:name var-decl)) (set (-> var-decl :init user-eval)))
  (let [btype (-> var-decl :vartype :base-type btype2clj)]
    (s/register (keyword "mzn-user" (:name var-decl))
                (s/spec (s/coll-of ::int :kind set?)))))

  
#_(defmethod make-data :mzn-2d-array
  "Make a clojure object representing the argument MiniZinc 2D array."
  [var-decl]
  (let [data (-> var-decl :init user-eval)
        sizes (mapv user-eval (-> var-decl :vartype :index))] ; This needs to count, not as above.
    (s/def (-> var-decl :name keyword) (s/coll-of ::int-vec-3)))) ; NYI
