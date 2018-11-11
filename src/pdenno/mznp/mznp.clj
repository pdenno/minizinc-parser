(ns pdenno.mznp.mznp
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]))

(use 'clojure.inspector) ; POD Temporary

;;; Purpose: Parse minizinc .mzn. 
;;; The defparse parsing functions pass around complete state. 
;;; The 'parse state' (AKA pstate) is a map with keys:
;;;   :result  - the parse structure from the most recent call to (parse :<some-rule-tag> pstate)
;;;   :tokens  - tokenized content that needs to be parsed into :model
;;;   :tags    - a stack describing where in the grammar it is parsing (used for debugging)
;;;   :tkn     - current token, not yet consumed. 
;;;   :error   - non-nil when things go wrong
;;;   :local   - temporarily stored parse content used later to form a complete grammar element. It is a vector of maps.
;;;             :local is used by the macros 'store' and 'recall'. 
;;;   :model   - the resulting cummulative parse structure

;;; Returns pstate: parse, consume-token, store, recall
;;; Returns something else: look, find-token

;;; The grammar implemented is 2.2.0. <Footnote 1> 
;;; The grammar is here: https://www.minizinc.org/doc-2.2.0/en/spec.html
;;; The grammar is here: https://www.minizinc.org/doc-2.2.0/en/spec.html#spec-grammar

;;; Footnote 1: Though I might have grabbed productions from an earlier version of MiniZinc,
;;;             I am updating to 2.2.0 wherever I find discrepancies. 

(def ^:private diag (atom nil))

;;; ============ Lexer ===============================================================
;;; POD Could add to this from library...
(def ^:private mzn-keywords
  #{"ann", "annotation", "any", "array", "bool", "case", "constraint", "diff", "div", "else",
    "elseif", "endif", "enum", "false", "float", "function", "if", "in", "include", "int",
    "intersect", "let", "list", "maximize", "minimize", "mod", "not", "of", "op", "opt", "output", ; website shows "opt"
    "par", "predicate", "record", "satisfy", "set", "solve", "string", "subset", "superset",
    "symdiff", "test", "then", "true", "tuple", "type", "union", "var", "where", "xor"
    ;; constraints
    "alldifferent", "all_equal" ; https://github.com/MiniZinc/libminizinc/tree/master/share/minizinc/std ... many more! 
    ;; See https://github.com/MiniZinc/libminizinc/blob/master/share/minizinc/std/builtins.mzn ... many more!
    ;; builtins.arithmetic 
    "sum", "product", "min", "max", "arg_min", "arg_max", "abs", "pow"   
    ;; builtins.logic
    "forall", "exists", "xorall", "clause", "iffall"})

(def ^:private mzn-syntactic ; chars that are valid syntax. 
  #{\[, \], \(, \), \=, \^, \,, \:, \;, \|, \+, \-, \<, \>, \_})

(def ^:private mzn-long-syntactic ; chars that COULD start a multi-character syntactic. 
  #{\., \,, \\, \/, \<, \>, \=, \!, \+, \|, \] \:})

;;; POD multi-line coment (e.g. /* ... */ would go in here, sort of. 
(defn read-long-syntactic [st ws]
  (let [len (count st)
        c0  (nth st 0)
        c1  (and (> len 0) (nth st 1))
        c2  (and (> len 1) (nth st 2))]
    (when-let [result (cond (and (= c0 \.) (= c1 \.)) {:raw ".." :tkn :..-op}
                            (and (= c0 \-) (= c1 \>)) {:raw "->" :tkn :->-op}
                            (and (= c0 \<) (= c1 \-)) {:raw "<-" :tkn :<--op}
                            (and (= c0 \\) (= c1 \/)) {:raw "\\/" :tkn :or-op}
                            (and (= c0 \/) (= c1 \\)) {:raw "/\\" :tkn :and-op}
                            (and (= c0 \<) (= c1 \=)) {:raw "<=" :tkn :le-op}
                            (and (= c0 \>) (= c1 \=)) {:raw ">=" :tkn :ge-op}
                            (and (= c0 \=) (= c1 \=)) {:raw "==" :tkn :eq-op}
                            (and (= c0 \!) (= c1 \=)) {:raw "!=" :tkn :ne-op}
                            (and (= c0 \+) (= c1 \+)) {:raw "++" :tkn :++-op}
                            (and (= c0 \[) (= c1 \|)) {:raw "[|" :tkn :2d-array-open}
                            (and (= c0 \|) (= c1 \])) {:raw "|[" :tkn :2d-array-close}
                            (and (= c0 \:) (= c1 \:)) {:raw "::" :tkn :ann-sep}
                            (and (= c0 \<) (= c1 \-) (= c2 \>)) {:raw "<->" :tkn :<->-op})]
      (assoc result :ws ws))))

(defn position-break 
  "Return the first position in s containing a syntactic character, ws,
   or nil if it contains none."
  [s]
  (let [len (count s)]
    (loop [n 0]
      (let [c (get s n)]
        (cond
          (= len n) nil
          (mzn-syntactic c) n
          (mzn-long-syntactic c) n
          (#{\space \tab \newline} c) n
          :else (recur (inc n)))))))

(defn whitesp 
  "Evaluates to whitespace at head of string or empty string if none."
  [s] ; https://stackoverflow.com/questions/15020669/clojure-multiline-regular-expression
  (if s (or (nth (re-matches #"(?s)(\s+).*$" s) 1) "") ""))

(defrecord MznOp [name])
(defrecord MznId [name])
(defrecord MznString [str])
(defrecord MznEOLcomment [comment])
(defrecord MznTypeInstVar [name])

;;; https://www.regular-expressions.info/modifiers.html (?s) allows  .* to match all characters including line breaks. 
(defn token-from-string
  "Return a map with keys :ws, :raw and :tkn from the front of the argument string."
  [stream]
  (let [ws (whitesp stream)
        s (subs stream (count ws))
        c (first s)]
    ;(cl-format *out* "~%ws = ~S~%c = ~S~%STREAM = ~S" ws c stream)
    (or  (and (empty? s) {:ws ws :raw "" :tkn :eof})                    ; EOF
         (and (mzn-long-syntactic c) (read-long-syntactic s ws))        ; ++, <=, == etc. 
         (and (mzn-syntactic c) {:ws ws :raw (str c) :tkn c})           ; literal syntactic char.
         (when-let [[_ num] (re-matches #"(?s)(\d+(\.\d+e[+-]?\d+)?).*" s)] 
           {:ws ws :raw num :tkn (read-string num)}),                   ; number
         (when-let [[_ id] (re-matches #"(?s)('[^']*').*" s)]           ; identifer type 2 POD Need's work. 
           {:ws ws :raw id :tkn (->MznId id)})
         (when-let [[_ st] (re-matches #"(?s)(\"[^\"]*\").*" s)]        ; string literal
           {:ws ws :raw st :tkn (->MznString st)})
         (when-let [[_ cm] (re-matches #"(?s)(\%[^\n]*).*" s)]          ; EOL comment
           {:ws ws :raw cm :tkn (->MznEOLcomment cm)})
         (when-let [[_ tivar] (re-matches #"(?s)(\$[A-Za-z][A-Za-z0-9_]*)" s)]
           {:ws ws :raw tivar :tkn (->MznTypeInstVar tivar)})
         (when-let [pos (position-break s)]
           (let [word (subs s 0 pos)]
             ;(cl-format *out* "~%word = ~S" word)
            (or 
             (and (mzn-keywords word) {:ws ws :raw word :tkn (keyword word)})  
             (when-let [[_ id] (re-matches #"^([a-zA-Z][A-Za-z0-9_]*).*" word)]     ; identifer type 1 
               {:ws ws :raw id :tkn (->MznId id)}))))
         (throw (ex-info "Char starts no known token: " {:raw c})))))

(defn tokenize
  "Return a vector of tokens"
  [stream]
  (loop [s stream 
         tkns []]
    (let [lex (token-from-string s)]
      (if (= :eof (:tkn lex))
        (conj tkns :eof)
        (recur
         (subs s (+ (count (:raw lex)) (count (:ws lex))))
         (conj tkns (:tkn lex)))))))

;;; ============ Parser ===============================================================
(defn look
  "Returns a token, not the pstate."
  [pstate n]
  (if (> (dec n) (count (:tokens pstate)))
    :eof
    (nth (:tokens pstate) n)))

(defn match-tkn
  "Return true if token matches test, which is a string, character, fn or regex."
  [test tkn]
  (cond (= test tkn) true
        (set? test) (test tkn)
        (fn? test) (test tkn)
        (instance? java.util.regex.Pattern test) (re-matches test tkn)
        :else false))

(defn consume-token
  "Move head of :tokens to :tkn ('consuming' the old :tkn) With 2 args, test :tkn first."
  ([pstate]
   (cl-format *out* "~%==>consuming '~S' (no test)" (:tkn pstate))
   (-> pstate 
       (assoc :tkn (or (first (:tokens pstate)) :eof))
       (assoc :tokens (vec (rest (:tokens pstate))))))
  ([pstate test]
   (cl-format *out* "~%==>consuming '~S' test = ~A" (:tkn pstate) test)
   (if (match-tkn test (:tkn pstate))
        (-> pstate ; replicated (rather than called on one arg) for println debugging. 
            (assoc :tkn (or (first (:tokens pstate)) :eof))
            (assoc :tokens (vec (rest (:tokens pstate)))))
        (assoc pstate :error {:expected test :got (:tkn pstate) :in "consume-token"}))))

(defn find-token
  "Return position if tkn is found within the item (before semicolon)."
  [pstate tkn]
  (let [tkns (:tokens pstate)
        pos-semi (.indexOf tkns \;)
        pos-tkn  (.indexOf tkns tkn)]
    (cond (== pos-semi -1) nil, ; Hmm... this is an error! Every item ends with semicolon
          (== pos-tkn  -1) nil,
          (< pos-semi pos-tkn) nil,
          :else pos-tkn)))

(defmacro defparse [tag [pstate & keys-form] & body]
  `(defmethod parse ~tag [~'tag ~pstate ~@(or keys-form '(& ignore))]
    (println ~tag)
     (as-> ~pstate ~pstate
       (reset! diag (update-in ~pstate [:tags] conj ~tag))
       (update-in ~pstate [:local] #(into [{}] %))
       (if (:error ~pstate) ; Stop things
         (assoc ~pstate :tkn :eof)
         (as-> ~pstate ~pstate
           ~@body))
       (reset! diag (update-in ~pstate [:tags] pop))
       (update-in ~pstate [:local] #(vec (rest %))))))

;;; Abbreviated for simple forms such as builtins. 
(defmacro defparse-auto [tag test]
  (let [pstate# nil]
    `(defparse ~tag
       [pstate#]
       (-> pstate#
           (assoc :result (:tkn pstate#))
           (consume-token ~test)))))

;;; This is an abstraction over protecting :result while something else swapped in...
(defmacro store [ps key & [from]]
  `(let [ps# ~ps
         key# ~key]
     (assoc-in ps# [:local 0 key#]
               (~(or from :result) ps#))))

;;; ...and this is for getting the value back. 
(defmacro recall [ps tag]
  `(let [ps# ~ps]
     (-> ~ps :local first ~tag)))

(defn parse-dispatch [tag & keys] tag)

(defmulti parse #'parse-dispatch)

(defn parse-mz
  "Top-level parsing from tokenized stream TOKENS."
  [tokens]
  (let [pstate {:tokens (vec (rest tokens)) :tkn (first tokens) :tags [] :local []}]
    (parse :model pstate)))

(defn tryme []
  (-> "./data/simplest.mzn"
      slurp
      tokenize
      parse-mz))

;;; ========================Production rules ====================================================
;;; <builtin-num-bin-op> ::= + | - | * | / | div | mod
(def builtin-num-bin-op #{\+ \- \* \/ :dif :mod})
(defparse-auto :builtin-bin-op builtin-num-bin-op)

;;;  <builtin-bin-op> ::= <-> | -> | <- | \/ | xor | /\ | < | > | <= | >= | == | = | != | in |
;;;                       subset | superset | union | diff | symdiff | .. | intersect| ++ | <builtin-num-bin-op>
(def builtin-bin-op
  (into #{:<->-op  :->-op  :<-op  :or-op  :xor :and-op \< \> :le-op :ge-op :eq-op \= :ne-op :in,
          :subset, :superset, :union, :diff, :symdiff, :..-op,  :intersect, :++-op}
        builtin-num-bin-op))
(defparse-auto :builtin-bin-op builtin-bin-op)

(def builtin-num-un-op #{\+, \-})
;;; <builtin-num-un-op> ::= + | -
(defparse-auto :builtin-num-bin-op builtin-num-un-op)

;;; <builtin-un-op> ::= not | <builtin-num-un-op>
(def builtin-un-op (conj builtin-num-un-op :not))
(defparse-auto :builtin-num-bin-op builtin-un-op)

;;;-------------------Library Builtins --(these should be in mzn-keywords too)----
(def builtin-arithmetic-op #{:sum, :product, :min, :max, :arg_min, :arg_max, :abs, :pow})

;;; POD This is not complete!
(def builtin-logic-op #{:forall :exists :xorall :clause})

;;; <builtin-op> ::= <builtin-bin-op> | <builtin-un-op>
(def builtin-op
  (sets/union builtin-bin-op
              builtin-un-op
              builtin-arithmetic-op
              builtin-logic-op))
  
(defparse-auto :builtin-op builtin-op)
;;;--------------------End Library Builtins ------------------------------------

;;; <model>::= [ <item>; ... ]
(defrecord MznModel [items])
;; <model> ::= [ <item> ; ...]
(defparse :model ; top-level grammar element. 
  [pstate]
  (loop [ps (assoc pstate :model (->MznModel []))]
    (if (= :eof (:tkn ps))
      ps
      (recur 
       (as-> ps ?ps
         (parse :item ?ps)
         (update-in ?ps [:model :items] #(conj % (:result ?ps)))
         (consume-token ?ps \;))))))

(def var-decl?  #{:bool :int :float :string :var :par})
;;; <item>::= <include-item> | <var-decl-item> | <assign-item> | <constraint-item> | <solve-item> |
;;;            <output-item> | <predicate-item> | <test-item> | <function-item> | <annotation-item>
(defparse :item
  [pstate]
  (let [tkn  (:tkn pstate)
        tkn2 (look pstate 0)]
    (cond (= tkn :include)                (parse :include-item pstate),
          (var-decl? tkn)                 (parse :var-decl-item pstate),
          (and (instance? MznId tkn) (= tkn2 \=)) (parse :assign-item pstate),
          (= tkn :constraint)             (parse :constraint-item pstate),
          (= tkn :solve)                  (parse :solve-item pstate),
          (= tkn :output)                 (parse :output-item pstate),
          (= tkn :predicate)              (parse :predicate-item pstate),
          (= tkn :test)                   (parse :test-item pstate),
          (= tkn :function)               (parse :function-item pstate),
          (= tkn :ann)                    (parse :annotation-item pstate) ; I don't think "annotation" is a keyword. 
          :else (assoc pstate :error {:expected "a MZn item" :got (:tkn pstate) :in :item}))))

;;; 4.1.6 Each type has one or more possible instantiations. The instantiation of a variable or value indicates
;;;       if it is fixed to a known value or not. A pairing of a type and instantiation is called a type-inst.
;;;       Reading further the "instantiation" indicates "how fixed or unfixed" its value is.
;;;       "instance-time" as opposed to "run-time" is the model as defined in .mzn.

;;; <type-inst-syn-item> ::= type <ident> <annotations>= <ti-expr>
;;;; POD I don't see where this is used

;;; <ti-expr-and-id> ::= <ti-expr> ":" <ident>
(defrecord MznIdDef [id type])
(defparse :ti-expr-and-id
  [pstate]
  (as-> pstate ?ps
    (parse :ti-expr ?ps)
    (store ?ps :type)
    (consume-token ?ps \:)
    (store ?ps :id :tkn)
    (consume-token ?ps #(instance? MznId %))
    (assoc ?ps :result (->MznIdDef (recall ?ps :id)
                                   (recall ?ps :type)))))
    
(defrecord MznInclude [model-part])
;;; <include-item> ::= include <string-literal>
(defparse :include-item
  [pstate]
  (as-> pstate ?ps
    (consume-token ?ps :include)
    (parse :string-literal ?ps)
    (store ?ps :model-part)
    (assoc ?ps :result (->MznInclude (recall ?ps :model-part)))))

(defrecord MznVarDecl [type-instance id ann expr])
;;; <var-decl-item> ::= <ti-expr-and-id> <annotations> [ "=" <expr>]

;;; <assign-item> ::= <ident> = <expr>
(defrecord MznAssignment [lhs rhs])
(defparse :assign-item
  [pstate]
  (as-> pstate ?ps
    (assoc-in ?ps [:local 0 :lhs] (:tkn ?ps)) ; Not store, because it isn't :result.
    (consume-token ?ps #(instance? MznId %))
    (consume-token ?ps \=)
    (parse :expr ?ps)
    (assoc ?ps :result (->MznAssignment (recall ?ps :lhs) (:result ?ps)))))

(defrecord MznConstraint [expr])
;;; <constraint-item> ::= constraint <expr>
(defparse :constraint-item
  [pstate]
  (as-> pstate ?ps
    (consume-token ?ps :constraint)
    (parse :expr ?ps)
    (assoc ?ps :result (->MznConstraint (:result ?ps)))))

(defrecord MznSolve [action expr anns])
;;; <solve-item> ::= solve <annotations> satisfy | solve <annotations> minimize <expr> | solve <annotations> maximize <expr>
(defparse :constraint-item
  [pstate]
  (as-> pstate ?ps
    (consume-token ?ps :solve)
    (parse :annotations ?ps)
    (store ?ps :anns)
    (store ?ps :action :tkn) ; Stores value of :tkn, not :result.
    (consume-token ?ps #{:satisfy :minimize :maximize})
    (if (= :satisfy (recall ?ps :action))
      (assoc ?ps :result nil)
      (parse :expr ?ps))
    (assoc ?ps :result (->MznSolve (recall ?ps :action)
                                   (:result ?ps)
                                   (recall ?ps :anns)))))

(defrecord MznVarDecl [lhs rhs anns])
;;; <var-decl-item> ::= <ti-expr-and-id> <annotations> [ "=" <expr> ]
(defparse :var-decl-item
  [pstate]
  (as-> pstate ?ps
    (parse :ti-expr-and-id pstate)
    (store ?ps :lhs)
    (parse :annotations pstate)
    (store ?ps :anns)
    (if (= (:tkn ?ps) \=)
      (as-> ?ps ?ps1
          (consume-token ?ps1)
          (parse :expr ?ps1)
          (assoc ?ps1 :result (->MznVarDecl (recall ?ps1 :lhs)
                                            (:result ?ps1)
                                            (recall ?ps1 :anns))))
      (assoc ?ps :result (->MznVarDecl
                          (recall ?ps :lhs)
                          nil
                          (recall ?ps :anns))))))

;;; <output-item> ::= output <expr> 
(defparse :output-item
  [pstate]
  (assoc pstate :error {:msg "We don't use output"}))

;;; <annotation-item> ::= annotation <ident> <params>

;;; <annotations> ::= [ "::" <annotation> ]*
(defparse :annotations
  [pstate]
  (if (= (:tkn pstate) :ann-sep)
    (as-> pstate ?ps
      (assoc-in ?ps [:local 0 :anns] [])
      (loop [ps ?ps]
        (as-> ps ?ps1
          (consume-token ?ps1 :ann-sep)
          (parse :annotation ?ps1)
          (update-in ?ps1 [:local 0 :anns] conj (:result ?ps1))
          (if (not= (:tkn ?ps1) :ann-sep)
            (assoc ?ps1 :result (recall ?ps1 :anns))
            (recur ?ps1)))))
    pstate))

(defrecord MznAnnotation [head tail])
;;; <annotation> ::= <expr-atom-head> <expr-atom-tail>
(defparse :annotation
  [pstate]
  (as-> pstate ?ps
    (parse :expr-atom-head ?ps)
    (store ?ps :head)
    (parse :expr-atom-tail ?ps)
    (->MznAnnotation (recall ?ps :head) (:result ?ps))))

;;; <predicate-item> ::= predicate <operation-item-tail>

;;; <test-item> ::= test <operation-item-tail>

(defrecord MznFunction [])
;;; <function-item> ::= function <ti-expr>: <operation-item-tail>

;;; <operation-item-tail> ::= <ident> <params> <annotations> [ = <expr>]

;;; <params> ::= [ ( <ti-expr-and-id>, ... ) ]

;;; ;;; B.2. Type-Inst Expressions

;;; <ti-expr> ::= <base-ti-expr>
(defparse :ti-expr
  [pstate]
  (parse :base-ti-expr pstate))

(defrecord MznTypeInstExpr [var? par? expr])
;;; <base-ti-expr> ::= <var-par> <base-ti-expr-tail>
;;; <var-par> ::= var | par | funny-empty-thing >
(defparse :base-ti-expr
  [pstate]
  (let [var-par? (:tkn pstate)]
    (as-> pstate ?ps
      (cond-> ?ps (#{:var :par} var-par?) (consume-token))
      (parse :base-ti-expr-tail ?ps)
      (assoc ?ps :result (map->MznTypeInstExpr {:expr (:result ?ps)}))
      (cond-> ?ps (= :var var-par?) (assoc-in [:result :var?] true))
      (cond-> ?ps (= :par var-par?) (assoc-in [:result :par?] true)))))

;;; <base-type> ::= "bool" | "int" | "float" | "string"
(defparse-auto :base-type #{:bool :int :float :string})

(defrecord MznIntegerRange [from to])
(defrecord MznSetLiterals  [elems])
;;; <base-ti-expr-tail> ::= <ident> | <base-type> | <set-ti-expr-tail> |
;;;                          <array-ti-expr-tail> | ann | opt <base-ti-expr-tail> | "{" <expr>, ... "}" |
;;;                          <num-expr> .. <num-expr>
(defparse :base-ti-expr-tail
  [pstate]
  (let [tkn (:tkn pstate)]
    (cond (instance? MznId tkn) ; <ident>
          (-> pstate
              (assoc :result tkn)
              consume-token),
          (#{:bool :int :float :string} tkn) ; <base-type}
          (parse :base-type pstate),
          (= :set tkn)                       ; <set-ti-exp-tail>
          (parse :set-ti-expr-tail pstate),
          (instance? MznTypeInstVar tkn)     ; <ti-variable-expr-tail>
          (-> pstate
              (assoc :result tkn)
              consume-token),
          (#{:array :list} tkn)
          (parse :array-ti-expr-tail pstate),
          (= tkn :ann)                       ; ann
          (assoc pstate :error {:msg "Annotations NYI."}),
          (#{:opt :op} tkn)                  ; opt <base-ti-expr-tail>
          (as-> pstate ?ps
              (consume-token ?ps)
              (parse :base-ti-expr-tail ?ps)
              (assoc-in ?ps [:result :optional?] true)),
          (= tkn \{)                        ; "{" <expr>, ... "}" 
          (parse :set-literal pstate),
          (find-token pstate :..-op)        ; <num-expr> ".."  <num-expr> ; call it a range expression
          (assoc pstate :error {:msg "<num-expr> .. <num-expr> NYI."}))))

(defrecord MznSetType [base-type optional?])
;;; <set-ti-expr-tail> ::= set of <base-type>
(defparse :base-ti-expr-tail
  [pstate]
  (as-> pstate ?ps
      (consume-token ?ps :set)
      (consume-token ?ps :of)
      (parse :base-type ?ps)
      (assoc ?ps :result (->MznSetType (:result ?ps) nil))))
          
(defrecord MznArrayType [index base-type optional?])
(defrecord MznListType [base-type]) 
;;; <array-ti-expr-tail> ::= array "[" <ti-expr>, ... "]" of <ti-expr> | list of <ti-expr> ; POD added " around "[".
(defparse :array-ti-expr-tail
  [pstate]
  (cond (= (:tkn pstate) :array)
        (as-> pstate ?ps
          (consume-token ?ps :array)
          (consume-token ?ps \[)
          (consume-token ?ps \])
          (parse :ti-expr-list ?ps)
          (store ?ps :ti-list)
          (consume-token ?ps :of)
          (parse :ti-expr ?ps)
          (assoc ?ps :result (->MznArrayType (recall ?ps :ti-list)
                                             (:result ?ps))))
        (= (:tkn pstate) :list)
        (as-> pstate ?ps
          (consume-token ?ps :list)
          (consume-token ?ps :of)
          (parse :ti-expr ?ps)
          (assoc ?ps :result (->MznListType (:result ?ps))))
        :else
        (assoc pstate :error {:expected "array or list"
                              :got (:tkn pstate)
                              :in :array-ti-expr-tail})))

;;; <ti-variable-expr-tail> ::= $[A-Za-z][A-Za-z0-9_]*
(defparse-auto :ti-variable-expr-tail #(instance? MznTypeInstVar %))

;;; <op-ti-expr-tail> ::= opt ( <ti-expr>: ( <ti-expr>, ... ) )
;;; I don't see where in the grammar this is used!
;;; POD Shows "opt" not "op" on website.

(defrecord MznExpr [atom tail])
;;;   B.3. Expressions
;;; <expr> ::= <expr-atom> <expr-binop-tail>
(defparse :expr
  [pstate]
  (as-> pstate ?ps
    (parse ?ps :expr-atom)
    (store ?ps :atom)
    (parse ?ps :expr-binop-tail)
    (assoc ?ps :result (->MznExpr (recall ?ps :atom) (:result ?ps)))))

(defrecord MznExprAtom [head tail ann])
;;; <expr-atom> ::= <expr-atom-head> <expr-atom-tail> <annotations>
(defparse :expr-atom
  [pstate]
  (as-> pstate ?ps
    (parse :expr-atom-head ?ps)
    (store ?ps :head)
    (parse :expr-atom-tail ?ps)
    (store ?ps :tail)
    (parse :annotations ?ps)
    (assoc ?ps :result (->MznExprAtom
                        (recall ?ps :head)
                        (recall ?ps :tail) 
                        (:result ?ps)))))

(defrecord MznExprBinopTail [bin-op expr])
;;; <expr-binop-tail> ::= [ <bin-op> <expr> ]
(defparse :expr-binop-tail
  [pstate]
  (as-> pstate ?ps
    (parse :bin-op ?ps)
    (store ?ps :bin-op)
    (parse :expr ?ps)
    (assoc ?ps :result (->MznExprBinopTail
                        (recall ?ps :bin-op)
                        (:result ?ps)))))

(defn ident-or-quoted-op?
  "Returns true if head satisfies <ident-or-quoted-op> ::= <ident> | ’<builtin-op>’"
  [pstate]
  (let [tkn (:tkn pstate)
        tkns (:tokens pstate)]
    (or (instance? MznId tkn)
        (let [tkn2 (nth tkns 0)
              tkn3 (nth tkns 1)]
          (and (= tkn \')
               (builtin-op tkn2)
               (= tkn3 \'))))))

(defrecord MznExprUnOp [uni-op atom])
;;;  <expr-atom-head> ::= <builtin-un-op> <expr-atom> | ( <expr>) | <ident-or-quoted-op> |
;;;                       _ | <bool-literal> | <int-literal> | <float-literal> | <string-literal> |
;;;                       <set-literal> | <set-comp> | <array-literal> | <array-literal-2d> |
;;;                       <array-comp> | <ann-literal> | <if-then-else-expr> | <let-expr> | <call-expr> |
;;;                       <gen-call-expr>
(declare part1 part2 part3)
(defparse :expr-atom-head
  [pstate]
  (let [tkn (:tkn pstate)]
    (or (part1 pstate tkn)     ; through <string-literal>
        (part2 pstate tkn))))  ; through end 

(defn part1 [pstate tkn]
  (cond (builtin-un-op tkn)                     ; <builtin-un-op> <expr-atom>
        (as-> pstate ?ps
          (consume-token ?ps)
          (parse :expr-atom ?ps)
          (assoc ?ps :result (->MznExprUnOp tkn (:result ?ps)))),
        (= \( tkn)                              ; ( <expr )
        (as-> pstate ?ps
          (consume-token ?ps)
          (parse :expr ?ps)
          (consume-token ?ps \))
          (assoc ?ps :result (->MznExpr (:result ?ps)))),
        (ident-or-quoted-op? pstate)           ; <ident-or-quoted-op>
        (parse :ident-or-quoted-op pstate),
        (= \_ tkn)                             ; _
        (-> pstate                              
            consume-token
            (assoc :result \_)),
        (#{false true} tkn)                    ; bool-literal
        (parse :bool-literal pstate),
        (integer? tkn)                         ; int-literal
        (parse :int-literal pstate), 
        (instance? MznString tkn)              ; string-literal
        (parse :string-literal pstate)))

;;;  <expr-atom-head> ::= <part1> |
;;;                       <set-literal> | <set-comp> | <array-literal> | <array-comp> | <array-literal-2d> |
;;;                       <ann-literal> | <if-then-else-expr> | <let-expr> | <call-expr> |
;;;                       <gen-call-expr>
(defn part2 [pstate tkn]
  (let [pos1 (find-token pstate \})
        pos2 (find-token pstate \|)
        pos3 (find-token pstate \])]
    (cond (and (= tkn \{) (or (not pos2) (and pos1 (< pos1 pos2))))   ; <set-literal>
          (parse :set-literal pstate),                                
          (= tkn \{)                                                  ; <set-comp>
          (parse :set-comp pstate),                                   
          (and (= tkn \[) (or (not pos2) (and pos3 (< pos3 pos2))))   ; <array-literal>
          (parse :array-literal pstate),
          (= tkn \[)                                                  ; <array-comp>
          (parse :array-comp pstate)
          (= tkn :2d-array-open pstate)                               ; <array-literal-2d>
          (parse :array-literal-2d pstate),
          (= tkn :ann-sep) ; POD needs investigation!                 ; <ann-literal>
          (parse :ann-literal pstate),
          (= tkn :if)                                                 ; <if-then-else-expr>
          (parse :if-then-else-expr pstate),                          
          (= tkn :let)                                                ; <let-expr>
          (parse :let-expr),                                          
          (builtin-logic-op tkn) ; POD I'm making this up             ; <gen-call-expr>
          (parse :gen-call-expr pstate)
          (builtin-op tkn)                                            ; <call-expr>
          (parse :call-expr pstate))))

(defrecord MznQuotedOp [op])
;;; <ident-or-quoted-op> ::= <ident> | "'" <builtin-op> "'"
(defparse :ident-or-quoted-op
  [pstate]
  (cond (instance? MznId (:tkn pstate))
        (-> pstate
            (assoc :result (:tkn pstate))
            (consume-token))
        (ident-or-quoted-op? pstate)
        (as-> pstate ?ps
          (consume-token ?ps \')
          (assoc ?ps :result (->MznQuotedOp (:tkn ?ps)))
          (consume-token ?ps builtin-op)
          (consume-token ?ps \'))
        :else
        (assoc pstate :error {:expected "ident-or-quoted-op" :got (:tkn pstate)})))

;;;<expr-atom-tail> ::= > | <array-access-tail> <expr-atom-tail>

;;; <num-expr> ::= <num-expr-atom> <num-expr-binop-tail>

;;; <num-expr-atom> ::= <num-expr-atom-head> <expr-atom-tail> <annotations>

;;; <num-expr-binop-tail> ::= [ <num-bin-op> <num-expr>]

;;; <num-bin-op> ::= <builtin-num-bin-op> | ‘<ident>‘
(defparse-auto :num-bin--op #(or (instance? MznId %) (builtin-num-bin-op %)))

;;; <num-expr-atom-head> ::= <builtin-num-un-op> <num-expr-atom> | ( <num-expr> ) | <ident-or-quoted-op> |
;;;                          <int-literal> | <float-literal> | <if-then-else-expr> | <case-expr> | <let-expr> |
;;;                          <call-expr> | <gen-call-expr>

;;; <bin-op> ::= <builtin-bin-op> | ‘<ident>‘
(defparse-auto :bin-op #(or (instance? MznId %) (builtin-bin-op %)))

;;; <bool-literal> ::= false | true
(defparse-auto :bool-literal #{false true})
(defparse-auto :int-literal #(integer? %))
(defparse-auto :float-literal #(float? %))
(defparse-auto :string-literal #(instance? MznString %))

;;; <set-literal> ::= { [ <expr>, ... ] }
(defrecord MznSetLiteral [elems]) 
;;; "{" <expr>, ... "}"
;;; (parse :set-literal {:tkn \{ :tokens [1 \, 2 \, 3 \}] })
(defparse :set-literal
  [pstate]
  (as-> pstate ?ps
    (consume-token ?ps \{)
    (assoc-in ?ps [:local 0 :elems] [])
    (loop [ps ?ps]
      (if (= (:tkn ps) \})
        (as-> ps ?ps1
          (consume-token ?ps1)
          (assoc ?ps1 :result (->MznSetLiteral (-> ?ps1 :local first :elems))))
        (as-> ps ?ps1
          (parse :expr ?ps1)
          (update-in ?ps1 [:local 0 :elems] conj (:result ?ps1))
          (recur (if (= (:tkn ?ps1) \,) (consume-token ?ps1 \,) ?ps1)))))))

(defrecord MznSetComp [expr comp-tail])
;;; <set-comp> ::= "{" <expr> "|" <comp-tail> "}"
(defparse :set-comp
  [pstate]
  (as-> pstate ?ps
    (consume-token ?ps \{)
    (parse :expr ?ps)
    (store ?ps :expr)
    (consume-token ?ps \|)
    (parse :comp-tail ?ps)
    (consume-token ?ps \})
    (assoc ?ps :result (->MznSetComp
                        (recall ?ps :expr)
                        (:result ?ps)))))

;;; <array-literal> ::= "[" [ <expr>, ... ] "]"

;;; <array-literal-2d> ::= "[|" [ (<expr>, ... ) "|" ... ] "|]"

;;; <array-comp> ::= "[" <expr> "|" <comp-tail> "]"

;;; <array-access-tail> ::= [ <expr>, ... ]

;;; <ann-literal> ::= <ident> [ ( <expr>, ... ) ]

;;; <if-then-else-expr> ::= "if" <expr> "then" <expr>( "elseif" <expr> "then" <expr>)* "else" <expr> "endif"

;;; <call-expr> ::= <ident-or-quoted-op> [ ( <expr>, ... ) ]

;;; <let-expr> ::= "let" { <let-item>; ... } "in" <expr>

;;; <let-item> ::= <var-decl-item> | <constraint-item>


;;; <comp-tail> ::= <generator>, ... [ "where" <expr>]

;;; <generator> ::= <ident>, ... "in" <expr>



;;; <gen-call-expr> ::= <ident-or-quoted-op> "(" <comp-tail> ")" "(" <expr> ")"
;;; See https://www.minizinc.org/doc-2.2.0/en/spec.html#spec-generator-call-expressions
;;; I am improvizing since I don't understand the spec at this point!
;;;
;;; forall(i,j in Domain where i<j)
;;;     (noattack(i, j, queens[i], queens[j]));
(defrecord MzGenCallExpr [logic-op quantifiers predicate])
(defparse :gen-call-expr
  [pstate]
  (as-> pstate ?ps
    (store ?ps :logic-op :tkn)
    (consume-token ?ps builtin-logic-op)
    (consume-token ?ps \()
    (parse :comp-tail ?ps)
    (store ?ps :quantifier)
    (consume-token ?ps \))
    (consume-token ?ps \()
    (parse :expr ?ps)
    (assoc :result (->MzGenCallExpr
                    (recall ?ps :logic-op)
                    (recall ?ps :quantifierp)
                    (:result ?ps)))))


;;;=== General =========================
(defn ppp []
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint *1)))

(defn ppprint [arg]
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint arg)))

(defn break
  ([] (throw (ex-info "Break!" {})))
  ([text] (throw (ex-info text {})))
  ([text args] (throw (ex-info text args))))
