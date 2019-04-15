(ns pdenno.mznp.mzn-data
  "Functions and macros that 'implement' (for execution and explanation) MiniZinc data structures
   To establish data values, data from the minizinc model (:mval) can be used, or data from the
   running Jupyter notebook (:kval)."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string       :as str]
            [clojure.set          :as sets]
            [clojure.spec-alpha2  :as s]
            [pdenno.mznp.mznp     :as mznp]
            [pdenno.mznp.sexp     :as sexp]
            [pdenno.mznp.mzn-fns  :as mznf]
            [pdenno.mznp.mzn-user :as mznu]))

;;; Data can be found by three ways:
;;; (1) :mval : It can be set in MiniZinc. In that case, it is expressed as a literal or
;;;            expression found in (-> info :var-decls <name> :mval)
;;; (2) :kval : It can be set in Python. In that case, it can be found by purefoo.notebook/kquery-var.
;;; (3) :uval : It can be found in clojure namespace pdenno.mznp.mzn-user. In this case,
;;;             there was also at least an expression, for example (mznf/range 0 numJobs) in
;;;             (-> info :var-delcs <name> :uval) 

(declare add-specs uget)
(def diag (atom nil))

;;; ======================== Interning in mznu ====================================================
(def mznu-string "pdenno.mznp.mzn-user")
(def mznu-symbol 'pdenno.mznp.mzn-user)

;;; ToDo: Make alldifferent a spec!

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
   (let [used (sets/union (sets/difference (vars-used (-> info :core :var-decls id :mval)) assume)
                          (sets/difference (indexes-used info id) assume))]
     (cond (literal? (-> info :core :var-decls id :mval)) true
           (-> info :core :var-decls id :var?) false
           ;; Everything used is assumed defined and nothing used uses this id. 
           (and (every? #(not ((vars-used (-> info :core :vars-used % :mval)) id)) used)
                (every? #(not ((indexes-used info %) id)) used)
                (every? assume used)) true
           :else false))))

;;; POD Need to look for cycles? 
(defn data-dependency-order
  "Sort the var-decls into an order in which they can be evaluated."
  [info]
  (let [ids (remove #(-> info :core :var-decls % :var?) (-> info :core :var-decls keys))]
    (loop [result (vec (filter #(literal? (-> info :core :var-decls % :mval)) ids))
           remaining (sets/difference (set ids) (set result))]
      (let [more? (filter #(definable? info % (set result)) remaining)]
        (if (empty? more?)
          result
          (recur (into result more?)
                 (sets/difference remaining (set more?))))))))
      
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

(defn intern-model-data
  "Intern data objects and define specs wherever possible in the model.
   Return the info object with :uval set in each var-decl."
  [info]
  (reduce (fn [I id]
            (let [var-decl (-> I :core :var-decls id)
                  needed-syms (vars-used (or (:mval var-decl) (:kval var-decl)))
                  val  (or (:kval var-decl) (:mval var-decl))]
              (if (and (apply bound? (map #(intern mznu-symbol (-> % name symbol)) needed-syms))
                       (every? #(-> % uget nil? not) needed-syms))
                (do ((user-intern (name id)) (user-eval val))
                    (assoc-in I [:core :var-decls id :uval] (uget id)))
                (do (println (name id) "cannot be interned; missing data.")
                    I))))
          info
          (data-dependency-order info)))

(defn uget
  "Return the value interned for the argument, which can be a keyword
   or a symbol (need not be interned)."
  [sym]
  (-> (intern mznu-symbol (-> sym name symbol)) var-get))

(defn executable-form?
  "Return true if the thing is form and the data it needs is interned."
  [info form]
  (when (seq? form)
    (let [vars (vars-used form)]
      (and (apply bound? (map #(intern mznu-symbol (-> % name symbol)) vars)) 
           (every? #(-> % uget nil? not) vars)))))

;;; ======================== Specs ====================================================
(defn base-type-spec
  "Sets :temp-model-spec in info object.
   If the arg names a MiniZinc base type, return the corresponding spec (::int etc.).
   If the arg names a MiniZinc data object, then define a new spec for *elements* of the named
   data object (not the type of data object). The new spec is named :mznu/<Dataobject name>-elem.
   Specifically, if the datatype is a index set and populated, you can specify exactly what values
   are allowed."
  [info id]
  (let [var-decl  (-> info :core :var-decls id)
        base-type (-> var-decl :vartype :base-type)
        obj       (when (not (#{:int :float :string} base-type))
                    (if (executable-form? info base-type) 
                     (user-eval base-type) ; typically an index set 
                     (var-get (intern mznu-symbol (-> var-decl :name symbol)))))
        user-obj  (if (coll? obj) (set obj) (-> obj list set))] ; make it a set (predicate function).
    (cond (set? user-obj)
          (register-spec-info
           info
           (keyword mznu-string (str (:name var-decl) "-elem"))
           (s/spec* user-obj)),
          (#{:int :float :string} base-type)
          (register-spec-info info ({:int ::int, :float ::float :string ::string} base-type)),
          :else
          (register-spec-info info ::anything))))

(defn register-model-specs
  "Register specs for all var-decls. Note that some vars add more than one spec.
   This function updates the value of :model-specs as well as having the side-effect
   of registering specs."
  [info]
  (as-> info ?info
    (assoc ?info :model-specs #{})
    (reduce (fn [info id] (add-specs info id))
            ?info
            (-> ?info :core :var-decls keys))
    (dissoc ?info :temp-model-spec)))

(defn register-spec-info
  "Update the (-> model :model-specs) with a spec; set :temp-type-spec."
  ([info key] (assoc info :temp-type-spec key))
  ([info key body]
   (let [spec (s/register key body)]
     (-> info
         (update :model-specs conj spec)
         (assoc  :temp-model-spec spec)))))

(defn add-specs-dispatch [info id]
  (-> info :core :var-decls id :vartype :datatype))

(defmulti add-specs
  "Make a mzn-user interned clojure object representing the MiniZinc object; define its spec."
  #'add-specs-dispatch)

(defmethod add-specs :int
  [info id]
  (register-spec-info
   info
   (keyword mznu-string (name id))
   (s/spec* `(s/or
              :not-populated nil?
              :populated integer?))))

(defmethod add-specs :float
  [info id]
  (register-spec-info
   info
   (keyword mznu-string (name id))
   (s/spec* `(s/or
              :not-populated nil?
              :populated float?))))

(defmethod add-specs :mzn-set
  [info id]
  (let [var-decl (-> info :core :var-decls id)]
    (as-> info ?info
      (base-type-spec ?info id)
      (register-spec-info
       ?info
       (keyword mznu-string (name id))
       (s/spec* `(s/or
                  :not-populated nil?
                  :populated (s/coll-of ~(:temp-model-spec ?info)
                                        :kind set?)))))))
(defn index-set-size
  "If the sym corresponds to a sym in mzn-user, and its var's value is a set, 
   get its size. Otherwise nil."
  [sym]
  (let [user-obj (-> (intern mznu-symbol (-> sym name symbol)) var-get)]
    (when (coll? user-obj) (count user-obj))))
      
(defmethod add-specs :mzn-array
  [info id]
  (let [var-decl   (-> info :core :var-decls id)
        size-sym   (-> var-decl :vartype :index first)
        size       (if (number? size-sym) size-sym (index-set-size size-sym))]
    (as-> info ?info
      (base-type-spec info id)
      (register-spec-info
       ?info
       (keyword mznu-string (name id))
       (s/spec* `(s/or
                  :not-populated nil?
                  :populated (s/coll-of ~(:temp-model-spec ?info)
                                        :kind vector?
                                        ~@(when size `(:count ~size)))))))))

(defmethod add-specs :mzn-2d-array
  [info id]
  (let [var-decl   (-> info :core :var-decls id)
        size-sym   (-> var-decl :vartype :index first)
        size       (if (number? size-sym) size-sym (index-set-size size-sym))
        inner-sym  (-> var-decl :vartype :index second)
        inner-size (if (number? inner-sym) inner-sym (index-set-size inner-sym))
        inner-key  (keyword mznu-string (str (name id) "-inner"))]
    (as-> info ?info
      (base-type-spec ?info id)
      (register-spec-info
       ?info
       inner-key
       (s/spec* `(s/coll-of ~(:temp-model-spec ?info)
                            :kind vector?
                            ~@(when inner-size `(:count ~inner-size)))))
      (register-spec-info
       ?info
       (keyword mznu-string (name id))
       (s/spec* `(s/or
                  :not-populated nil?
                  :populated (s/coll-of
                              ~inner-key
                              :kind vector?
                              ~@(when size `(:count ~size)))))))))

(s/def ::int integer?)
(s/def ::float float?)
(s/def ::string string?)
(s/def ::anything (fn [_] true))


;;; ================== Utils ========================================
(defn populated?
  "Returns the data associated with the data object, if any."
  [info id]
  (let [decl (-> info :core :var-decls id)]
    (or (:mval decl)
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

(defn process-model!
  "This is used mostly for debugging."
  [file]
  (as-> {} ?m
    (assoc ?m :core (sexp/rewrite* ::mznp/model file :file? true))
    (intern-model-data ?m)
    (assoc ?m :spec-types (model-types ?m))))



  
