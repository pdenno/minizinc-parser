(ns pdenno.mznp.sexp
  "Simplify the parsed structure using s-expressions in some places."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec.alpha :as s]
            [pdenno.mznp.utils :as util]
            [pdenno.mznp.mznp :as mznp]))

(def debugging? (atom false))
(def diag (atom {}))
(def tags (atom []))
(def locals (atom [{}]))

(defmacro mzn-array-access [& body]
  ~@body) ; POD NYI

;;; Similar to mznp/defparse except that it serves no role except to make debugging nicer.
;;; You could eliminate this by global replace of "defrewrite" --> "defmethod rewrite" and removing defn rewrite. 
(defmacro defrewrite [tag [obj & keys-form] & body] 
  `(defmethod rewrite-meth ~tag [~'tag ~obj ~@(or keys-form '(& _))]
     (when @debugging? (cl-format *out* "~A==> ~A~%" (util/nspaces (count @tags)) ~tag))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [result# (do ~@body)]
     (swap! tags #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when @debugging? (cl-format *out* "~A<-- ~A returns ~S~%" (util/nspaces (count @tags)) ~tag result#))
         result#))))

(declare map-simplify remove-nils rewrite precedence op-precedence mzn2dsl-map)

(defn sexp-simplify
  "Toplevel function to transform the structure produced by the parser to something useful."
  [m]
  (-> m
      map-simplify
      rewrite))

(defn map-simplify
  "Recursively traverse the map structures changing records to maps, 
   removing nil map values, and adding :type value named after the record."
  [m]
  (cond (record? m)
        (-> {:type (-> m .getClass .getSimpleName keyword)} ; BTW, nothing from the parse has a :type. 
            (into (zipmap (keys m) (map map-simplify (vals m))))
            remove-nils),
        (vector? m) (mapv map-simplify m),
        :else m))
        
(defn remove-nils
  "Remove map values that are nil."
  [m]
  (reduce-kv (fn [m k v]
               (if (= nil v)
                 m
                 (assoc m k v)))
             {}
             m))

(defn rewrite-dispatch [tag obj & keys] tag)

(defmulti rewrite-meth #'rewrite-dispatch)

(defn rewrite [obj & keys]
  (let [tag
        (cond (map? obj)     (:type obj)
              (char? obj)    :char
              (keyword? obj) :keyword
              :else          :default)]
    (rewrite-meth tag obj keys)))

;;;----------------------- Rewriting ---------------------------------------
(defrewrite :MznModel [m]
  (reduce (fn [res item]
            (cl-format *out* "~%**************item = ~A" item)
            (let [type (:type item)]
              (cond (= :MznConstraint type)
                    (update res :constraints conj (-> item :expr rewrite))
                    (#{:MznAssignment} type)
                    (let [[k v] (rewrite item)]
                      (assoc-in res [:assignments k] v)))))
          {:constraints []
           :assignments {}}
          (:items m)))

;;; This needed only for testing constraints; see above for typical usage.
(defrewrite :MznConstraint [m]
  (-> m :expr rewrite))

(defrewrite :MznExpr [m]
  (let [atom (-> m :atom rewrite)
        primary? (:primary? m)] ; Need to grab it now.
    (as-> m ?m
      (cond (not (:tail ?m)) atom,
            (= :MznExprBinopTail (-> ?m :tail :type))
            (into {:type :bin-op :lhs atom } (-> ?m :tail rewrite)), ; into: get :lhs? on the lhs
            :else (throw (ex-info "Some other tail" {:tail (:tail ?m)})))
      (reset! diag ?m)
      (cond-> ?m ; <================== ?m (sum [[j Jobs]] (if {:type :bin-op-primary, :lhs (pdenno.mznp.sexp/mzn-array-access LineOfJob j), :op ==, :rhs lin} (pdenno.mznp.sexp/mzn-array-access WorkersOnJob j w1) 0))
        (and primary? (map? ?m)) (assoc :type :bin-op-primary))
      (rewrite ?m)))) ; This fixes precedence (needs works; see NYI test case)

(defrewrite :MznIfExpr [m]
  `(if ~(-> m :condition rewrite)
     ~(-> m :then rewrite)
     ~(-> m :else rewrite)))

(defrewrite :MznExprBinopTail [m]
  {:op    (-> m :bin-op rewrite)
   :rhs   (-> m :expr   rewrite)})

(defrewrite :MznExprAtom [m]
  (if (contains? m :tail)
    `(~'mzn-array-access ~(-> m :head rewrite)
                       ~@(-> m :tail rewrite))
    (-> m :head rewrite)))

(defrewrite :MznArray [m]
  (mapv rewrite (:elems m)))

(defrewrite :MznArrayAccess [m]
  (mapv rewrite (:exprs m)))

(defrewrite :MznId [m]
  (-> m :name symbol))

;;; POD this might be unnecessary. See remarks at op-precedence-table. 
(defn safe2symbol
  "Some characters don't convert!"
  [c]
  (let [these-fail {\< '<, \> '>, \= 'assign, \+ '+, \- '-, \* '* \/ '/}]
    (or (get these-fail c)
        (symbol c))))

(defrewrite :char [c]
  (reset! diag {:c c})
  (let [sc (safe2symbol c)]
    (if (contains? mzn2dsl-map sc)
      (mzn2dsl-map sc)
      (throw (ex-info "Unknown syntactic character" {:char c})))))

(defrewrite :keyword [k]
  (if (contains? mzn2dsl-map k)
    (mzn2dsl-map k)
    (throw (ex-info "Unknown keyword" {:key k}))))

(defrewrite :default [m]
  m)

;;;(forall [[j Jobs]]
;;;  (<= (mznIdx endWeek j) (mznIdx WeeksTillDue j)))
(defrewrite :MznGenCallExpr [m]
  (let [op (-> m :gen-call-op util/keysym)
        gens (mapv rewrite (-> m :generators))
        where (-> m :where rewrite)
        body (-> m :body rewrite)]
    `(~op ~gens :where ~(or where true) ~body)))

;;; This is not nice; I'm going to try to eliminate it. 
#_(defrewrite :MznCompTail [m]
  (mapv rewrite (:generators m)))

(defrewrite :MznGenerator [m]
  (conj (mapv rewrite (:ids m))
        (rewrite (:expr m))))

(defrewrite :MznAssignment [m]
  [(-> m :lhs rewrite keyword) (-> m :rhs rewrite)])

;;; (test-rewrite ::mznp/expr "1*2-3") => {:lhs 1, :op *, :rhs? {:lhs 2, :op -, :rhs? 3}} *Not as intended!*
;;; (def foo   {:lhs 1,                      :op '*, :rhs {:lhs 2, :op '-, :rhs 3}})
;;; (def defoo {:lhs {:lhs 1 :op '* :rhs 2}, :op '-, :rhs 3})
;;; A lower :val means tighter binding. For example 1+2*3 means 1+(2*3) because * binds tighter than +.
;;; This fixes precedence, but as discussed 2019-02-16, it only does it for 3 expressions. 
#_(defrewrite :bin-op [exp] ; <====================== caller should make :bin-op-seq This would handle it. 
  (let [op1 (:op exp)
        op2 (-> exp :rhs :op)]
    (as-> exp ?exp
      (if (and op2 (< (precedence op1) (precedence op2)))
        (let [rhs-save (:rhs ?exp)]
          (println "?exp =" ?exp)
          (-> ?exp
              (assoc :lhs {:lhs (:lhs ?exp) :op (:op ?exp) :rhs (-> ?exp :rhs :lhs)})
              (assoc :op (:op rhs-save))
              (assoc :rhs (:rhs rhs-save))
              (update :lhs rewrite)
              (update :rhs rewrite)))
        ?exp) ; March: Why :type in next?
      (assoc ?exp :type `(~(:op ?exp) ~(:lhs ?exp) ~(:rhs ?exp)))
      (rewrite ?exp))))

(defrewrite :bin-op [exp] ; <====================== caller should make :bin-op-seq This would handle it.
  `(~(:op exp) ~(-> exp :lhs rewrite) ~(-> exp :rhs rewrite)))
 

;;; POD I think something here needs work: I return, for example, \>, not '>.
;;; 2019-03-23 Can't user characters; mzn2dsl-map complains. 
;;; MiniZinc Specification, section 7.2.
;;; A lower :val means tighter binding. For example 1+2*3 means 1+(2*3) because * (300) binds tighter than + (400).
(def op-precedence-tbl ; lower :val means binds tighter. 
  {:<->-op    {:assoc :left :val 1200}
   :->-op     {:assoc :left :val 1100}
   :<--op     {:assoc :left :val 1100}
   :or-op     {:assoc :left :val 1000}
   :xor       {:assoc :left :val 1000}
   :and-op    {:assoc :left :val 900}
   '<         {:assoc :none :val 800}
   '>         {:assoc :none :val 800}
   :<=        {:assoc :none :val 800}
   :>=        {:assoc :none :val 800}
   :==        {:assoc :none :val 800}
   'assign    {:assoc :none :val 800}
   :not=      {:assoc :none :val 800}
   :in        {:assoc :none :val 700}
   :subset    {:assoc :none :val 700}
   :superset  {:assoc :none :val 700}
   :union     {:assoc :left :val 600}
   :diff      {:assoc :left :val 600}
   :symdiff   {:assoc :left :val 600}
   :..-op     {:assoc :none :val 500}
   '+         {:assoc :left :val 400}
   '-         {:assoc :left :val 400}
   '*         {:assoc :left :val 300}
   :div       {:assoc :left :val 300}
   :mod       {:assoc :left :val 300}
   '/         {:assoc :left :val 300}
   :intersect {:assoc :left :val 300}   
   :++-op     {:assoc :right :val 200}
   :<ident>   {:assoc :left  :val 100}})

;;; The DSL operators are just the Mzn keywords things as symbols. (This will probably change; ..-op yuk!)
(def mzn2dsl-map 
  (zipmap (keys op-precedence-table)
          (map util/keysym (keys op-precedence-table))))

(defn precedence [op]
  (if (contains? op-precedence-table op)
    (-> op op-precedence-table :val)
    100))

;;; (ops-with-precedence 300) ==> (:mod * :div / :intersect)
(def ops-with-precedence
  "Holds vectors of operators that share a precedence value (the index of the map)." 
  (reduce-kv (fn [m k v] (assoc m k (map first v)))
             {}
             (group-by second (map #(let [[k v] %] [k (:val v)])
                                   op-precedence-table))))

(defn test-rewrite
  "mzn/parse-string and sexp-simplify, but with controls for partial evaluation, debugging etc.
   With no keys it does all steps without debug output."
  [tag str & {:keys [none? simplify? rewrite? file? debug? debug-mznp?] :as opts}]
  (let [all? (not (or (contains? opts :simplify?)
                      (contains? opts :rewrite?)
                      (contains? opts :none?)))
        mznp-db? @mznp/debugging?
        db?           @debugging?]
    (reset! mznp/debugging? debug-mznp?)
    (reset!      debugging? debug?)
    (let [result (-> (mznp/parse-string tag (if file? (slurp str) str))
                     :result
                     (cond->
                         (or all? rewrite? simplify?) map-simplify
                         (or all? rewrite?)           rewrite))] 
      (reset! mznp/debugging? mznp-db?)
      (reset!      debugging?      db?)
      result)))

;;;--- This would be part of a reconceptualization of fix-precedence See notes 2019-02-16. 
(defn flatten-binop
  ([exp] (flatten-binop exp []))
  ([exp res]
   (cond (= (:type exp) :bin-op)
         (-> res 
             (into [(flatten-binop (:lhs exp))])
             (conj (:op exp))
             (into [(flatten-binop (:rhs exp))])),
         (= (:type exp) :bin-op-primary)
         (conj res
               (vector (flatten-binop (:lhs exp))
                       (:op exp)
                       (flatten-binop (:rhs exp)))),
         :else exp)))

;;;============================== Reduce-bin-ops ===================================
(defn simplify-primary 
  "Primary (which is syntactically '(' <expr> ')') isn't identical with expr; this makes it so."
  [exp]
  (let [front-part (-> exp 
                       :atom
                       :head
                       (dissoc :primary?))]
    (if (:tail exp)
      {:type :MznExpr
       :atom front-part
       :tail (-> exp :tail (dissoc :primary?))}
      exp)))

(defn primary? [exp] (-> exp :atom :head :primary?))

(defn reduce-bin-ops
  "Replace nested binary operations/operands with a flat vector [operand, op, operand...] in :bin-ops"
  [exp]
  (cond (primary? exp) ; Thing has parentheses around it. 
        (-> exp simplify-primary reduce-bin-ops vector),
        (and (= (:type exp) :MznExpr) (:tail exp))
        (let [tail    (-> exp :tail :expr reduce-bin-ops)
              bin-ops (-> (or (:bin-ops exp) []) ; Reduce the head and op... 
                          (conj (-> exp :atom :head reduce-bin-ops))
                          (conj (-> exp :tail :bin-op))
                          (into (-> tail :bin-ops)))]
          (as-> exp ?e
            (assoc  ?e :bin-ops bin-ops)
            (dissoc ?e :atom) ; eliminate the parts reduced (atom and tail). 
            (dissoc ?e :tail))),
        (= (:type exp) :MznExpr) ; no tail. 
        (-> exp
            (cond-> (not (:bin-ops exp)) (assoc :bin-ops []))
            (update :bin-ops #(conj % (-> exp :atom :head reduce-bin-ops)))
            (dissoc :atom)),
        (map? exp) ; this is the 'preserve' condition
        (reduce-kv 
         (fn [e k v] (assoc e k (reduce-bin-ops v)))
         {}
         exp),
        :else
        exp)) ; this is the 'recursion cutoff' condition

(defn reduce-bin-ops-str [str]
  "Convenience function for testing reduce-bin-ops."
  (-> (test-rewrite ::mznp/expr str :simplify? true)
      reduce-bin-ops))

(defn bvec2info
  "Using the bin-op-vec (at any level of processing), create a map containing information about it."
  [bvec]
  (reduce-kv
   (fn [res k v]
     (if (odd? k)
       (-> res
           (update :operators  #(let [sym (safe2symbol v)]
                                  (conj % {:pos k
                                           :op sym
                                           :prec (-> op-precedence-tbl sym :val)})))
           (update :operands #(conj % nil)))
       (update res :operands #(conj % v))))
   {:operators [] :operands [] :mods #{}}
   (zipmap (range (count bvec)) bvec)))

(defn info2bvec
  "Compute a new bvec from an info object after some order-bin-ops processing."
  [info]
  info) ; NYI

(defn order-bin-ops
  "Process a :bin-ops vector into sexps conforming to precedence rules. Crazy!"
  [bvec]
  (let [mods (atom #{})]
    (reduce (fn [bvec pval]
              (reset! mods #{})
              (as-> bvec ?x
                (bvec2info ?x)
                (update ?x :operators
                        (fn [ops]
                          (doall
                           (map (fn [omap]
                                  (if (= pval (:prec omap))
                                    (let [pos (:pos omap)]
                                      (swap! mods (fn [mods] (conj mods pos))) ; track what position changed.
                                      (list (:op omap)
                                            (nth (:operands ?x) (dec pos))
                                            (nth (:operands ?x) (inc pos))))
                                    omap))
                                ops))))
                (assoc ?x :mods @mods)
                (info2bvec ?x)))
            bvec
            [300])))
            
                        
                      
    
