(ns pdenno.mznp.mzn-data
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc data structures"
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec-alpha2 :as s]
            [pdenno.mznp.sexp :as sexp]
            [pdenno.mznp.mzn-fns :as mznf]))

(declare model-types intern-data! intern-data)

(defn process-model [file]
  (as-> {} ?m
    (assoc ?m :core (sexp/rewrite* ::mznp/model file :file? true))
    (assoc ?m :model-types (model-types ?m))
    #_(intern-data! ?m)))

(defn intern-data!
  "Intern data objects and define specs wherever possible in the model."
  [model]
  (doall (map #(intern-data model %) (-> model :core :var-decls vals))))

;;; ((user-intern "x") 2)
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
;;; (user-eval '(+ x y))
(defn user-eval
  "Do clojure eval in namespace mzn-user.
   If the sexp has unresolvable symbols, catch them and return :unresolved-symbol."
  [form]
  (binding [*ns* (find-ns (symbol "mzn-user"))]
    (eval form)))

(defn model-types
  "Return the set of the mzn-user-qualified type name keyword in the model."
  [model]
  (set (map #(keyword "mzn-user" %)
            (-> model :core :var-decls keys))))

(defn type2spec
  "Depending on type arg, return either
  (1) the clojure type predicate corresponding to the MiniZinc base type, or
  (2) namespace (mzn-user) qualifed user-defined type keyword corresponding to TYPE."
  [model type]
  (or ({:int integer?, :float float? :string string?} type)
      ((model-types model) type)))

;;; (-> var-decl :vartype :datatype) are  #{:int :float :mzn-set :mzn-array :mzn-2d-array})
(defn intern-data-dispatch [model var-decl] (-> var-decl :vartype :datatype))

(defmulti intern-data
  "Make a clojure object representing the MiniZinc object; define its spec."
  #'intern-data-dispatch)

(defmethod intern-data :int
  [model id]
  (let [var-decl (-> model :var-decls id)]
    ((user-intern (:name var-decl)) (-> var-decl :value user-eval)) 
    (s/register (keyword "mzn-user" (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated integer?)))))

(defmethod intern-data :float
  [model id]
  (let [var-decl (-> model :var-decls id)]
    ((user-intern (:name var-decl)) (-> var-decl :value user-eval))
    (s/register (keyword "mzn-user" (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated float?)))))

(defmethod intern-data :mzn-set
  [model id]
  (let [var-decl (-> model :var-decls id)]
    ((user-intern (:name var-decl)) (set (-> var-decl :value user-eval)))
    (s/register (keyword "mzn-user" (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated (s/coll-of ~(type2spec model (-> var-decl :vartype :base-type))
                                                 :kind set?))))))
  
(defmethod intern-data :mzn-array
  [model id]
  (let [var-decl (-> model :var-decls id)]
    ((user-intern (:name var-decl)) (-> var-decl :value user-eval))
    (let [size (-> var-decl :index first :value count)]
      (s/register (keyword "mzn-user" (:name var-decl))
                  (s/spec* `(s/or
                             :not-populated nil?
                             :populated (s/coll-of
                                         ~(type2spec model (-> var-decl :vartype :base-type))
                                         :kind vector?
                                         ~@(size `(:count ~size)))))))))

(defmethod intern-data :mzn-2d-array
  [model id]
  (let [var-decl (-> model :var-decls id)
        size (-> var-decl :index first :value count)
        inner-size (-> var-decl :index second :value count)
        inner-key (keyword "mzn-user" (str (:name var-decl) "-inner"))]
    ((user-intern (:name var-decl)) (-> var-decl :value user-eval))
    (s/register inner-key
                (s/spec
                 (s/spec* `(s/or
                            :not-populated nil?
                            :populated (s/coll-of
                                        ~(type2spec model (-> var-decl :vartype :base-type))
                                        :kind vector?
                                        ~@(size `(:count ~inner-size)))))))
    (s/register (keyword "mzn-user" (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated (s/coll-of
                                       vector?
                                       :kind vector?
                                       ~@(size `(:count ~size))))))))
(defn populated?
  "Returns the data associated with the data object, if any."
  [model id]
  (-> model :var-decls id :value))

(defn variable?
  [model id]
  (-> model :var-decls id :var?))
  
