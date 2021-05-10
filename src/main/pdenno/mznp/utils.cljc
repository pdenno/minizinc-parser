(ns pdenno.mznp.utils
  "Utilities for MiniZinc parsing and rewriting."
  (:require
   [clojure.pprint :refer [cl-format]]))

;;;============ These are all used in macros.clj The macros are used in mznp.cljc and sexp.cljc ==============
(def debugging? (atom false))
(def debugging-rewrite? (atom false))
(def tags (atom []))
(def locals (atom [{}]))

(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n)))

(defn class-name
  "Return a keyword representing the class of the object.
  For example (class-name 1) ==> :Long. Class name is not namespace-qualified."
  [obj]
  (->> obj type str (re-matches #"^.+\.(.*)$") second keyword))

(defn index-of-elem
  "ClojureScript-compatible version of .indexOf."
  [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) nil
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))

;;;================== These are used in mznp.cljc ============================================================
;;; <num-bin-op> ::= <builtin-num-bin-op> | ‘<ident>‘
;;; <ti-variable-expr-tail> ::= $[A-Za-z][A-Za-z0-9_]*
;;; <base-type> ::= "bool" | "int" | "float" | "string"
;;; <builtin-un-op> ::= not | <builtin-num-un-op>
;;; <builtin-num-un-op> ::= + | -
;;; <builtin-bin-op> ::= <-> | -> | <- | \/ | xor | /\ | < | > | <= | >= | == | = | != | in |
;;;                      subset | superset | union | diff | symdiff | .. | intersect| ++ | <builtin-num-bin-op>
;;; <builtin-num-bin-op> ::= + | - | * | / | div | mod
(def builtin-num-bin-op #{\+ \- \* \/ :div :mod})
(def builtin-bin-op
  (into #{:<->-op  :->-op  :<-op  :or-op :xor-op :and-op \< \> :<= :>= :== \= :not= :in,
          :subset, :superset, :union, :diff, :symdiff, :range-op,  :intersect, :++-op}
        builtin-num-bin-op))
(def builtin-num-un-op #{\+, \-})
(def builtin-un-op (conj builtin-num-un-op :not))

(def auto-parse-map
  "A map of parse tags and associated test for tokens of that tag type."
    {:mznp/bool-literal          #{:false :true}
     :mznp/int-literal           #(integer? %)
     :mznp/float-literal         #(float? %)
     :mznp/string-literal        #(= (class-name %) :MznString) ; Use class-name because referencing records is problematic.
     :mznp/ti-variable-expr-tail #(= (class-name %) :MznTypeInstVar)
     :mznp/num-bin-op            #(or (= (class-name %) :MznId) (builtin-num-bin-op %))
     :mznp/base-type             #{:bool :int :float :string}
     :mznp/builtin-un-op         builtin-un-op         ; Not yet used for auto-parse; the var is used. 
     :mznp/builtin-num-un-op     builtin-num-un-op     ; Not yet used for auto-parse; the var is used. 
     :mznp/builtin-bin-op        builtin-bin-op        ; Not yet used for auto-parse; the var is used. 
     :mznp/builtin-num-bin-op    builtin-num-bin-op})  ; Not yet used for auto-parse; the var is used.

#_(defn auto-parse?
  "Return true if the token passes the test indicated by the parse tag."
  [tag tkn]
  (when-let [func (get auto-parse-map tag)]
    (func tkn)))

(defn match-tkn
  "Return true if token matches test, which is a string, character, fn or regex."
  [test tkn]
  (cond (= test tkn) true
        (set? test) (test tkn)
        (fn? test) (test tkn)
        (instance? #?(:clj java.util.regex.Pattern :cljs js/RegExp) test) (re-matches test tkn)
        :else false))

(defn eat-token
  "Move head of :tokens to :tkn ('consuming' the old :tkn) With 2 args, test :tkn first."
  ([pstate]
   (when @debugging?
     (cl-format *out* "~%*** Consuming ~S in (~S (~S ~S)) next = ~S"
                (:tkn pstate) (-> pstate :tags last) (:line pstate) (:col pstate) (-> pstate :tokens second :tkn)))
   (let [next-up (-> pstate :tokens second)]
     (-> pstate
         (assoc :tkn  (or (:tkn next-up) :eof))
         (assoc :line (:line next-up))
         (assoc :col  (:col next-up))
         (assoc :tokens (vec (rest (:tokens pstate)))))))
  ([pstate test]
   (let [next-up (-> pstate :tokens second)]
     (if (match-tkn test (:tkn pstate))
       (do
         (when @debugging?
           (cl-format *out* "~%*** Consuming ~S in (~S (~S ~S)) test = ~A next = ~S "
                      (:tkn pstate) (-> pstate :tags last) (:line pstate) (:col pstate) test (-> pstate :tokens second :tkn)))
         (-> pstate ; replicated (rather than called on one arg) for println debugging.
             (assoc :tkn  (or (:tkn next-up) :eof))
             (assoc :line (:line next-up))
             (assoc :col  (:col next-up))
             (assoc :tokens (vec (rest (:tokens pstate))))))
       (do
         (when @debugging?
           (cl-format *out* "~%*** FAILURE ~S in (~S (~S ~S)) test = ~A next = ~S "
                      (:tkn pstate) (-> pstate :tags last) (:line pstate) (:col pstate) test (-> pstate :tokens second :tkn))
           (cl-format *out* "~%*** :error = ~S" (:error pstate)))
         (-> pstate
             (assoc :error {:expected test :got (:tkn pstate) :in "eat-token" :line (:line pstate) :col (:col pstate)})
             (assoc :tkn :eof)))))))

