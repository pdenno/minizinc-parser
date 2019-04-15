(ns pdenno.mznp.mzn-data
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc data structures"
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string       :as str]
            [clojure.set          :as sets]
            [clojure.spec-alpha2  :as s]
            [pdenno.mznp.mznp     :as mznp]
            [pdenno.mznp.sexp     :as sexp]
            [pdenno.mznp.mzn-fns  :as mznf]
            [pdenno.mznp.mzn-user :as mznu]))

(declare model-types intern-data! intern-data)

(def mznu-string "pdenno.mznp.mzn-user")
(def mznu-symbol 'pdenno.mznp.mzn-user)

(def diag (atom nil))

;;; ToDo: Make alldifferent a spec!

(defn process-model!
  "This is used mostly for debugging."
  [file]
  (as-> {} ?m
    (assoc ?m :core (sexp/rewrite* ::mznp/model file :file? true))
    (intern-data! ?m)
    (assoc ?m :spec-types (model-types ?m))))

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
  [info id]
  (set (map keyword (-> info :core :var-decls id :vartype :index))))

;;; The 2019-03-30 version of this might have been sufficient. It depends on whether
;;; or not you expect that mzn code already passed mzn parsing (not my parsing).
;;; If it did, then no undefined variables are used. I don't assume that here.
;;; For that matter, if it has used mine I could intersect this with notebook info var-decls.
(def collect (atom #{}))
(defn vars-used! [form]
  (cond (symbol? form) (swap! collect #(conj % (keyword form)))
        (literal? form) :whatever
        (seq? form) (doall (map vars-used! (rest form)))))
(defn vars-used [form & {:keys [consider-only]}]
  "Return the set of variables (any symbol actually) used in the form.
   If :consider-only is provided, it should be a set of uninterned symbols."
  (reset! collect #{})
  (vars-used! form)
  (if consider-only
    (sets/intersection @collect consider-only)
    @collect))

(defn definable?
  "Returns true if data is established to evaluate the object. The assume argument
   is a collection of ids (strings) that can be assumed to be established in the
   scope of the investigation of the id."
  ([info id] (definable? info id #{}))
  ([info id assume]
   (let [used (sets/union (sets/difference (vars-used (-> info :core :var-decls id :val)) assume)
                          (sets/difference (indexes-used info id) assume))]
     (cond (literal? (-> info :core :var-decls id :val)) true
           (-> info :core :var-decls id :var?) false
           ;; Everything used is assumed defined and nothing used uses this id. 
           (and (every? #(not ((vars-used (-> info :core :vars-used % :val)) id)) used)
                (every? #(not ((indexes-used info %) id)) used)
                (every? assume used)) true
           :else false))))

;;; POD Need to look for cycles? 
(defn data-dependency-order
  "Sort the var-decls into an order in which they can be evaluated."
  [info]
  (let [ids (remove #(-> info :core :var-decls % :var?) (-> info :core :var-decls keys))]
    (loop [result (vec (filter #(literal? (-> info :core :var-decls % :val)) ids))
           remaining (sets/difference (set ids) (set result))]
      (let [more? (filter #(definable? info % (set result)) remaining)]
        (if (empty? more?)
          result
          (recur (into result more?)
                 (sets/difference remaining (set more?))))))))
      
(defn intern-data!
  "Intern data objects and define specs wherever possible in the model.
   Return the object untouched."
  [info]
  (doall (map #(intern-data info %) (data-dependency-order info)))
  ;; Not sure why I did this twice!
  (doall (map #(intern-data info %) (map #(-> % :name keyword)
                                          (filter :var? (-> info :core :var-decls vals)))))
  info)

;;; ((user-intern "x") 2)
(defn user-intern
  "Provide a namestring of a variable to intern in mzn-user; the returned function takes one
   argument, the value to set the variable to."
  [sym-str]
  (fn [arg]
    (binding [*ns* (find-ns (symbol mznu-string))]
      (intern (find-ns (symbol mznu-string))
              (symbol sym-str)
              arg))))

;;; POD It would be nice to say *what symbol* is unresolved. In tracking this down, 
;;; of course, I will have to watch for cycles.
;;; (user-eval '(+ x y))
(defn user-eval
  "Do clojure eval in namespace mzn-user.
   If the sexp has unresolvable symbols, catch them and return :unresolved-symbol."
  [form]
  (println "eval form: " form)
  (binding [*ns* (find-ns (symbol mznu-string))]
    (eval form)))

(defn model-types
  "Return the set of the mzn-user-qualified type name keyword in the model."
  [info]
  (set (map #(keyword mznu-string (name %))
            (-> info :core :var-decls keys))))

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
  [info type]
  (let [user-obj (when (not (#{:int :float :string} type))
                   (if (seq? type)
                     (user-eval type)
                     (-> (intern mznu-symbol (-> type name symbol)) var-get)))]
    (cond (set? user-obj)
          (s/register
           (keyword mznu-string (str (name type) "-elem"))
           (s/spec* user-obj)),
          (#{:int :float :string} type)
          ({:int ::int, :float ::float :string ::string} type),
          :else ::anything)))

;;; (-> var-decl :vartype :datatype) are  #{:int :float :mzn-set :mzn-array :mzn-2d-array})
(defn intern-data-dispatch [info id] (-> info :core :var-decls id :vartype :datatype))

(defmulti intern-data
  "Make a mzn-user interned clojure object representing the MiniZinc object; define its spec."
  #'intern-data-dispatch)

(defmethod intern-data :int
  [info id]
  (let [var-decl (-> info :core :var-decls id)
        val      (or (:kval var-decl) (:val var-decl))]
    ((user-intern (:name var-decl)) (user-eval val))
    (s/register (keyword mznu-string (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated integer?)))))

(defmethod intern-data :float
  [info id]
  (let [var-decl (-> info :core :var-decls id)
        val      (or (:kval var-decl) (:val var-decl))]
    ((user-intern (:name var-decl)) (user-eval val))
    (s/register (keyword mznu-string (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated float?)))))

(defmethod intern-data :mzn-set
  [info id]
  (let [var-decl (-> info :core :var-decls id)
        val      (or (:kval var-decl) (:val var-decl))]
    ((user-intern (:name var-decl)) (user-eval val)) ; was (set (user eval..)) ???
    (s/register (keyword mznu-string (:name var-decl))
                (s/spec* `(s/or
                           :not-populated nil?
                           :populated (s/coll-of ~(type2spec! info (-> var-decl :vartype :base-type))
                                                 :kind set?))))))

(defn index-set-size
  "If the sym corresponds to a sym in mzn-user, and its var's value is a set, 
   get its size. Otherwise nil."
  [sym]
  (let [user-obj (-> (intern mznu-symbol (-> sym name symbol)) var-get)]
    (when (coll? user-obj) (count user-obj))))
      
(defmethod intern-data :mzn-array
  [info id]
  (let [var-decl   (-> info :core :var-decls id)
        size-sym   (-> var-decl :vartype :index first)
        size       (if (number? size-sym) size-sym (index-set-size size-sym))
        val        (or (:kval var-decl) (:val var-decl))]
      ((user-intern (:name var-decl)) (user-eval val))
      (s/register (keyword mznu-string (:name var-decl))
                  (s/spec* `(s/or
                             :not-populated nil?
                             :populated (s/coll-of
                                         ~(type2spec! info (-> var-decl :vartype :base-type))
                                         :kind vector?
                                         ~@(when size `(:count ~size))))))))

(defmethod intern-data :mzn-2d-array
  [info id]
  (let [var-decl   (-> info :core :var-decls id)
        size-sym   (-> var-decl :vartype :index first)
        size       (if (number? size-sym) size-sym (index-set-size size-sym))
        inner-sym  (-> var-decl :vartype :index second)
        inner-size (if (number? inner-sym) inner-sym (index-set-size inner-sym))
        inner-key  (keyword mznu-string (str (:name var-decl) "-inner"))
        val      ( or (:kval var-decl) (:val var-decl))]
      ((user-intern (:name var-decl)) (user-eval val))
      (s/register inner-key
                  (s/spec* `(s/or
                             :not-populated nil?
                             :inners (s/coll-of
                                      ~(type2spec! info (-> var-decl :vartype :base-type))
                                      :kind vector?
                                      ~@(when inner-size `(:count ~inner-size))))))
      (s/register (keyword mznu-string (:name var-decl))
                  (s/spec* `(s/or
                             :not-populated nil?
                             :populated (s/coll-of
                                         ~inner-key
                                         :kind vector?
                                         ~@(when size `(:count ~size))))))))

;;; POD As is, this is just getting the :val. Would it be more useful to look what is interned?
;;; Of course, will have to keep what is interned up to date. (Includes ns-umapping values).
(defn populated?
  "Returns the data associated with the data object, if any."
  [info id]
  (let [decl (-> info :core :var-decls id)]
    (or (:val decl)
        (:kval decl))))

(defn unmap-data!
  "Remove from mzn-user any vars defined there."
  []
  (let [mznu (find-ns mznu-symbol)]
    (doall (map (fn [v]
                  (let [m (meta v)]
                    (when (= (:ns m) mznu)
                      (ns-unmap mznu-symbol (-> m :name symbol)))))
                (ns-map mznu-symbol)))
    true))

(defn uget
  "Return the value interned for the argument, which can be a keyword
   or mznu interned symbol."
  [sym]
  (-> (intern mznu-symbol (-> sym name symbol)) var-get))

  
