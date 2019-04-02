(ns pdenno.mznp.mzn-data
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc data structures"
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec-alpha2 :as s]
            [pdenno.mznp.mznp :as mznp]
            [pdenno.mznp.sexp :as sexp]
            [pdenno.mznp.mzn-fns :as mznf]
            [mzn-user :as mznu]))

(declare model-types intern-data! intern-data)

(def diag (atom nil))

;;; ToDo: Make alldifferent a spec!

(defn process-model! [file]
  (as-> {} ?m
    (assoc ?m :core (sexp/rewrite* ::mznp/model file :file? true))
    (assoc ?m :model-types (model-types ?m))
    (intern-data! ?m)))

(defn literal?
  "Return true if the data is literal"
  [d]
  (cond (or (number? d) (string? d)) true
        (seq? d) false
        (symbol? d) false
        (nil? d) false ; not yet defined. 
        (or (vector? d) (set? d)) (every? literal? d)))

(defn indexes-used
  "Return the set of the indexes used by the data."
  [model id]
  (set (map keyword (-> model :core :var-decls id :vartype :index))))

;;; The 2019-03-30 version of this might have been sufficient. It depends on whether
;;; or not you expect that mzn code already passed mzn parsing (not my parsing).
;;; If it did, then no undefined variables are used. I don't assume that here. 
(def collect (atom #{}))
(defn vars-used! [form]
  (cond (symbol? form) (swap! collect #(conj % (keyword form)))
        (literal? form) :whatever
        (seq? form) (doall (map vars-used! (rest form)))))
(defn vars-used [form]
  "Return the set of variables (any symbol actually) used in the form."
  (reset! collect #{})
  (vars-used! form)
  @collect)

(defn definable?
  "Returns true if data is established to evaluate the object. The assume argument
   is a collection of ids (strings) that can be assumed to be established in the
   scope of the investigation of the id."
  ([model id] (definable? model id #{}))
  ([model id assume]
   (let [used (sets/union (sets/difference (vars-used (-> model :core :var-decls id :value)) assume)
                          (sets/difference (indexes-used model id) assume))]
     (cond (literal? (-> model :core :var-decls id :value)) true
           (-> model :core :var-decls id :var?) false
           ;; Everything used is assumed defined and nothing used uses this id. 
           (and (every? #(not ((vars-used (-> model :core :vars-used % :value)) id)) used)
                (every? #(not ((indexes-used model %) id)) used)
                (every? assume used)) true
           :else false))))

;;; POD Need to look for cycles? 
(defn data-dependency-order
  "Sort the var-decls into an order in which they can be evaluated."
  [model]
  (let [ids (remove #(-> model :core :var-decls % :var?) (-> model :core :var-decls keys))]
    (loop [result (vec (filter #(literal? (-> model :core :var-decls % :value)) ids))
           remaining (sets/difference (set ids) (set result))]
      (let [more? (filter #(definable? model % (set result)) remaining)]
        (if (empty? more?)
          result
          (recur (into result more?)
                 (sets/difference remaining (set more?))))))))
      
(defn intern-data!
  "Intern data objects and define specs wherever possible in the model."
  [model]
  (doall (map #(intern-data model %) (data-dependency-order model)))
  (doall (map #(intern-data model %) (map #(-> % :name keyword)
                                          (filter :var? (-> model :core :var-decls vals)))))
  model)

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
  (set (map #(keyword "mzn-user" (name %))
            (-> model :core :var-decls keys))))

(s/def ::int integer?)
(s/def ::float float?)
(s/def ::string string?)
(s/def ::anything (fn [_] true))

(defn type2spec!
  "If the arg names a MiniZinc base type, return the corresponding spec (::int etc.).
   If the arg names a MiniZinc data object, then define a new spec for *elements* of the named
   data object (not the type of data object). The new spec is named :mznu/<Dataobject name>-elem.
   Specifically, if the datatype is a index set and populated, you can specify exactly what values
   are allowed."
  [model type]
  (let [user-obj (when (not (#{:int :float :string} type))
                   (-> (intern 'mzn-user (-> type name symbol)) var-get))]
    (cond (set? user-obj)
          (s/register
           (keyword "mzn-user" (str (name type) "-elem"))
           (s/spec* user-obj)),
          (#{:int :float :string} type)
          ({:int ::int, :float ::float :string ::string} type),
          :else ::anything)))

;;; (-> var-decl :vartype :datatype) are  #{:int :float :mzn-set :mzn-array :mzn-2d-array})
(defn intern-data-dispatch [model id] (-> model :core :var-decls id :vartype :datatype))

(defmulti intern-data
  "Make a mzn-user interned clojure object representing the MiniZinc object; define its spec."
  #'intern-data-dispatch)

(defmethod intern-data :int
  [model id]
  (let [var-decl (-> model :core :var-decls id)] 
    ((user-intern (:name var-decl)) (-> var-decl :value user-eval)) 
    (s/register (keyword "mzn-user" (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated integer?)))))

(defmethod intern-data :float
  [model id]
  (let [var-decl (-> model :core :var-decls id)] 
    ((user-intern (:name var-decl)) (-> var-decl :value user-eval))
    (s/register (keyword "mzn-user" (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated float?)))))

(defmethod intern-data :mzn-set
  [model id]
  (let [var-decl (-> model :core :var-decls id)] 
    ((user-intern (:name var-decl)) (set (-> var-decl :value user-eval)))
    (s/register (keyword "mzn-user" (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated (s/coll-of ~(type2spec! model (-> var-decl :vartype :base-type))
                                                 :kind set?))))))

(defn index-set-size
  "If the sym corresponds to a sym in mzn-user, and its var's value is a set, 
   get its size. Otherwise nil."
  [sym]
  (let [user-obj (-> (intern 'mzn-user (-> sym name symbol)) var-get)]
    (when (set? user-obj) (count user-obj))))
      
(defmethod intern-data :mzn-array
  [model id]
  (let [var-decl   (-> model :core :var-decls id)
        size-sym   (-> var-decl :vartype :index first)
        size       (if (number? size-sym) size-sym (index-set-size size-sym))]
      ((user-intern (:name var-decl)) (-> var-decl :value user-eval))
      (s/register (keyword "mzn-user" (:name var-decl))
                  (s/spec* `(s/or
                             :not-populated nil?
                             :populated (s/coll-of
                                         ~(type2spec! model (-> var-decl :vartype :base-type))
                                         :kind vector?
                                         ~@(when size `(:count ~size))))))))

(defmethod intern-data :mzn-2d-array
  [model id]
  (let [var-decl   (-> model :core :var-decls id)
        size-sym   (-> var-decl :vartype :index first)
        size       (if (number? size-sym) size-sym (index-set-size size-sym))
        inner-sym  (-> var-decl :vartype :index second)
        inner-size (if (number? inner-sym) inner-sym (index-set-size inner-sym))
        inner-key (keyword "mzn-user" (str (:name var-decl) "-inner"))]
      ((user-intern (:name var-decl)) (-> var-decl :value user-eval))
      (s/register inner-key
                  (s/spec* `(s/or
                             :not-populated nil?
                             :inners (s/coll-of
                                      ~(type2spec! model (-> var-decl :vartype :base-type))
                                      :kind vector?
                                      ~@(when inner-size `(:count ~inner-size))))))
      (s/register (keyword "mzn-user" (:name var-decl))
                  (s/spec* `(s/or
                             :not-populated nil?
                             :populated (s/coll-of
                                         ~inner-key
                                         :kind vector?
                                         ~@(when size `(:count ~size))))))))

;;; POD As is, this is just getting the :value. Would it be more useful to look what is interned?
;;; Of course, will have to keep what is interned up to date. (Includes ns-umapping values).
(defn populated?
  "Returns the data associated with the data object, if any."
  [model id]
  (-> model :core :var-decls id :value))

(defn unmap-data!
  "Remove from mzn-user any vars defined there."
  []
  (let [mznu (find-ns 'mzn-user)]
    (doall (map (fn [v]
                  (let [m (meta v)]
                    (when (= (:ns m) mznu)
                      (ns-unmap 'mzn-user (-> m :name symbol)))))
                (ns-map 'mzn-user)))
    true))
