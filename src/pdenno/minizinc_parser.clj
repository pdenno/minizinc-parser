(ns pdenno.minizinc-parser
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]))

(use 'clojure.repl)      ; POD Temporary. For use of doc.
(use 'clojure.inspector) ; POD Temporary

;;; Purpose: Parse minizinc 
;;; The parsing functions are 'internally' functional (using threading macros on parse state).
;;; This seems a little weird at times, but it really does make debugging easier.

;;; <model>::= [ <item>; ... ]

(def ^:private diag (atom nil))
(defrecord Model [items])

;;; POD As written pstate really has to be a variable! (Otherwise will need to substitute in @body tree.)
(defmacro ^:private defparse [tag [pstate & keys-form] & body]
  `(defmethod parse ~tag [~'tag ~pstate ~@(or keys-form '(& ignore))]
;     (println ~tag)
     (as-> ~pstate ~pstate
       (reset! diag (update-in ~pstate [:tags] conj ~tag))
       (update-in ~pstate [:local] #(into [{}] %))
       (if (:error ~pstate) ; Stop things
         (assoc ~pstate :tkn :eof)
         (as-> ~pstate ~pstate
           ~@body))
       (reset! diag (update-in ~pstate [:tags] pop))
       (update-in ~pstate [:local] #(vec (rest %))))))

;;; ============ Lexer ===============================================================

(def ^:private mzn-keywords
  #{"ann", "annotation", "any", "array", "bool", "case", "constraint", "diff", "div", "else",
    "elseif", "endif", "enum", "false", "float", "function", "if", "in", "include", "int",
    "intersect", "let", "list", "maximize", "minimize", "mod", "not", "of", "op", "output",
    "par", "predicate", "record", "satisfy", "set", "solve", "string", "subset", "superset",
    "symdiff", "test", "then", "true", "tuple", "type", "union", "var", "where", "xor"})

(def ^:private mzn-syntactic
  #{\[, \], \(, \), \=, \^, \,, \:, \;, \|, \+, \-, \<, \>})

(def ^:private mzn-long-syntactic ; things that COULD be multi-character
  #{\., \,, \\, \/, \<, \>, \=, \!, \+})

;;; POD multi-line coment (e.g. /* ... */ would go in here, sort of. 
(defn read-long-syntactic [st ws]
  (let [len (count st)
        c0  (nth st 0)
        c1  (and (> len 0) (nth st 1))
        c2  (and (> len 1) (nth st 2))]
    (when-let [result (cond (and (= c0 \.) (= c1 \.)) {:raw ".." :tkn :..-op}
                            (and (= c0 \-) (= c1 \>)) {:raw "->" :tkn :->-op}
                            (and (= c0 \<) (= c1 \-)) {:raw "<-" :tkn :<--op}
                            (and (= c0 \\) (= c1 \/)) {:raw "<-" :tkn :or-op}
                            (and (= c0 \/) (= c1 \\)) {:raw "<-" :tkn :and-op}
                            (and (= c0 \<) (= c1 \=)) {:raw "<-" :tkn :le-op}
                            (and (= c0 \>) (= c1 \=)) {:raw "<-" :tkn :ge-op}
                            (and (= c0 \=) (= c1 \=)) {:raw "<-" :tkn :eq-op}
                            (and (= c0 \!) (= c1 \=)) {:raw "<-" :tkn :ne-op}
                            (and (= c0 \+) (= c1 \+)) {:raw "<-" :tkn :++-op}
                            (and (= c0 \<) (= c1 \-) (= c2 \>)) {:raw "<-" :tkn :<->-op})]
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

(defn- whitesp 
  "Evaluates to whitespace at head of string or empty string if none."
  [s] ; https://stackoverflow.com/questions/15020669/clojure-multiline-regular-expression
  (if s (or (nth (re-matches #"(?s)(\s+).*$" s) 1) "") ""))

(defrecord MznOperator [str])
(defrecord MznKeyword [str])
(defrecord MznIdentifier [str])
(defrecord MznString [str])
(defrecord MznEOLcomment [str])
(defn MznOperator? [x] (instance? MznOperator x))

;;; https://www.regular-expressions.info/modifiers.html (?s) allows  .* to match all characters including line breaks. 
(defn- token-from-string
  "Return a map with keys :ws, :raw and :tkn from the front of the argument string."
  [stream]
  (reset! diag {:stream stream})
  (let [ws (whitesp stream)
        s (subs stream (count ws))
        c (first s)]
    (cl-format *out* "~%ws = ~S~%c = ~S~%STREAM = ~S" ws c stream)
    (or  (and (empty? s) {:ws ws :raw "" :tkn :eof})                    ; EOF
         (and (mzn-long-syntactic c) (read-long-syntactic s ws))        ; ++, <=, == etc. 
         (and (mzn-syntactic c) {:ws ws :raw (str c) :tkn c})           ; literal syntactic char.
         (when-let [[_ num] (re-matches #"(?s)(\d+\.?\d*(e[+-]?\d+)?).*" s)] 
           {:ws ws :raw num :tkn (read-string num)}),                   ; number
         (when-let [[_ id] (re-matches #"(?s)('[^']*').*" s)]           ; identifer type 2 POD Need's work. 
           {:ws ws :raw id :tkn (->MznIdentifier id)})
         (when-let [[_ st] (re-matches #"(?s)(\"[^\"]*\").*" s)]        ; string literal
           {:ws ws :raw st :tkn (->MznString st)})
         (when-let [[_ cm] (re-matches #"(?s)(\%[^\n]*).*" s)]          ; EOL comment
           {:ws ws :raw cm :tkn (->MznEOLcomment cm)})
         (when-let [pos (position-break s)]
           (let [word (subs s 0 pos)]
            (cl-format *out* "~%word = ~S" word)
            (or 
             (and (mzn-keywords word) {:ws ws :raw word :tkn (->MznKeyword word)})  ; mzn keyword
             (when-let [[_ id] (re-matches #"^([a-zA-Z][A-Za-z0-9_]*).*" word)]     ; identifer type 1 
               {:ws ws :raw id :tkn (->MznIdentifier id)}))))
         (throw (ex-info "Char starts no known token: " {:raw c})))))

;;; When done, get rid of Lexeme!
(defn tokenize
  "Return a list of tokens"
  [stream]
  (loop [s stream 
         tkns []]
    (let [lex (token-from-string s)]
      (if (= :eof (:tkn lex))
        (conj tkns :eof)
        (recur
         (subs s (+ (count (:raw lex)) (count (:ws lex))))
         (conj tkns (:tkn lex)))))))

(defn tryme []
  (-> "./data/assignment.mzn" slurp tokenize))

(defn- read-token
  "Consume a token, setting :tkn."
  [pstate]
  (-> pstate 
      (assoc :tkn (or (first (:tokens pstate)) :eof))
      (assoc :tokens (rest (:tokens pstate)))))

(defn look
  "Returns a token, not the pstate."
  ([pstate]
   (look pstate 1))
  ([pstate n]
   (if (> n (count (:tokens pstate)))
     :eof
     (nth (:tokens pstate) (dec n)))))

(defn- assert-token
  "Read a token and check that it is what was expected."
  [pstate tkn]
  (as-> pstate ?pstate
    (if (= tkn (look ?pstate))
      (read-token ?pstate)
      (as-> ?pstate ?ps
        (assoc ?ps :error (str "expected: " tkn " got: " (:tkn ?ps)))
        (assoc ?ps :debug-tokens (:tokens ?ps))
        (assoc ?ps :tokens [])))))

(defn- assert-LaTeX
  "Read a token and check that it is what was expected."
  [pstate tkn]
  (as-> pstate ?pstate
    (if (and (LaTeX? (look ?pstate))
             (= (:name (look ?pstate)) tkn))
      (read-token ?pstate)
      (assoc ?pstate :error (str "expected LaTeX: " tkn " got: " (:tkn pstate))))))


;;; ============ Parser =============
;(remove-all-methods parse)
;(ns-unmap *ns* 'parse)

(defn- parse-dispatch [tag & keys] tag)

(defmulti parse #'parse-dispatch)

;;; math == relation | expression
(defparse :math
  [pstate]
  (as-> pstate ?pstate 
    (parse :expression ?pstate)
    (if (= :eof (look ?pstate))
      (assoc ?pstate :math (->MathExp (:result ?pstate)))
      (as-> ?pstate ?ps
        (parse :relation ?ps :lhs (:result ?ps))
        (assoc ?ps :math (->MathExp (:result ?ps)))))))

(defrecord Relation [lhs rel rhs])




;;; <item>::= <include-item> | <var-decl-item> | <assign-item> | <constraint-item> | <solve-item> |
;;;            <output-item> | <predicate-item> | <test-item> | <function-item> | <annotation-item>

;;; <type-inst-syn-item> ::= type <ident> <annotations>= <ti-expr>

;;; <ti-expr-and-id> ::= <ti-expr>: <ident>

;;; <include-item> ::= include <string-literal>

;;; <var-decl-item> ::= <ti-expr-and-id> <annotations> [ = <expr>]

;;; <assign-item> ::= <ident>= <expr>

;;; <constraint-item> ::= constraint <expr>
;;; <solve-item> ::= solve <annotations>satisfy | solve <annotations>minimize <expr> | solve <annotations>maximize <expr>

;;; <output-item> ::= output <expr> <annotation-item>

;;; <annotation-item> ::= annotation <ident> <params>

;;; <predicate-item> ::= predicate <operation-item-tail>

;;; <test-item> ::= test <operation-item-tail>

;;; <function-item> ::= function <ti-expr>: <operation-item-tail>

;;; <operation-item-tail> ::= <ident> <params> <annotations> [ = <expr>]

;;; <params> ::= [ ( <ti-expr-and-id>, ... ) ]

;;; ;;; B.2. Type-Inst Expressions

;;; <ti-expr> ::= <base-ti-expr>

;;; <base-ti-expr> ::= <var-par> <base-ti-expr-tail>

;;; <var-par> ::= var | par | >
;;; <base-ti-expr-tail> ::= <ident> | bool | int | float | string | <set-ti-expr-tail> |
;;;                          <array-ti-expr-tail> | ann | opt <base-ti-expr-tail> | { <expr>, ... } |
;;;                          <num-expr>.. <num-expr>

;;; <set-ti-expr-tail> ::= set of <base-type>

;;; <array-ti-expr-tail> ::= array [ <ti-expr>, ... ] of <ti-expr> | list of <ti-expr>

;;; <ti-variable-expr-tail> ::= $[A-Za-z][A-Za-z0-9_]*

;;; <op-ti-expr-tail> ::= op ( <ti-expr>: ( <ti-expr>, ... ) )

;;;   B.3. Expressions
;;; <expr> ::= <expr-atom> <expr-binop-tail>

;;; <expr-atom> ::= <expr-atom-head> <expr-atom-tail> <annotations>

;;; <expr-binop-tail> ::= [ <bin-op> <expr>]

;;;  <expr-atom-head> ::= <builtin-un-op> <expr-atom> | ( <expr>) | <ident-or-quoted-op> |
;;;                       _ | <bool-literal> | <int-literal> | <float-literal> | <string-literal> |
;;;                       <set-literal> | <set-comp> | <simple-array-literal> | <simple-array-literal-2d> |
;;;                       <indexed-array-literal> | <simple-array-comp> | <indexed-array-comp> | <ann-literal> |
;;;                       <if-then-else-expr> | <let-expr> | <call-expr> | <gen-call-expr>

;;;<expr-atom-tail> ::= > | <array-access-tail> <expr-atom-tail<

;;; <num-expr> ::= <num-expr-atom> <num-expr-binop-tail>

;;; <num-expr-atom> ::= <num-expr-atom-head> <expr-atom-tail> <annotations>

;;; <num-expr-binop-tail> ::= [ <num-bin-op> <num-expr>]

;;; <num-expr-atom-head> ::= <builtin-num-un-op> <num-expr-atom> | ( <num-expr>) | <ident-or-quoted-op> |
;;;                          <int-literal> | <float-literal> | <if-then-else-expr> | <case-expr> | <let-expr> |
;;;                          <call-expr> | <gen-call-expr?

;;; <builtin-op> ::= <builtin-bin-op> | <builtin-un-op>

;;; <bin-op> ::= <builtin-bin-op> | ‘<ident>‘

;;;  <builtin-bin-op> ::= <-> | -> | <- | \/ | xor | /\ | < | > | <= | >= | == | = | != | in |
;;;                       subset | superset | union | diff | symdiff | .. | intersect| ++ | <builtin-num-bin-op>

(def builtin-bin-op
  ^:private nil
  #{"<->"  "->"  "<-"  "\/"  "xor" "/\\", "<" ">" "<=" ">=" "==" "=" "!=" "in",
    "subset", "superset", "union", "diff", "symdiff", "..",  "intersect", "++"})


;;; <builtin-un-op> ::= not | <builtin-num-un-op>

;;; <num-bin-op> ::= <builtin-num-bin-op> | ‘<ident>‘

;;; <builtin-num-bin-op> ::= + | - | * | / | div | mod <builtin-num-un-op> ::= + | -

;;; <bool-literal> ::= false | true

;;; <int-literal> ::= [0-9]+ | 0x[0-9A-Fa-f]+ | 0o[0-7]+

;;; <float-literal> ::= [0-9]+.[0-9]+ | [0-9]+.[0-9]+[Ee][-+]<[0-9]+ | [0-9]+[Ee][-+]<[0-9]+

;;; <string-contents> ::= ([^"\n\] | \[^\n(])* 
;;; <string-literal> ::= "<string-contents>" | "<string-contents<\( <string-interpolate-tail>

;;; <string-interpolate-tail> ::= <expr>)<string-contents>" | <expr>)<string-contents>\( <string-interpolate-tail>

;;; <set-literal> ::= { [ <expr>, ... ] }

;;; <set-comp> ::= { <expr> | <comp-tail>}

;;; <comp-tail> ::= <generator>, ... [ where <expr>]

;;; <generator> ::= <ident>, ... in <expr>

;;; <array-literal> ::= [ [ <expr>, ... ] ]

;;; <array-literal-2d> ::= [| [ (<expr>, ... ) | ... ] |]

;;; <array-comp> ::= [ <expr> | <comp-tail>]

;;; <array-access-tail> ::= [ <expr>, ... ]

;;; <ann-literal> ::= <ident> [ ( <expr>, ... ) ]

;;; <if-then-else-expr> ::= if <expr>then <expr>( elseif <expr>then <expr>)* else <expr>endif

;;; <call-expr> ::= <ident-or-quoted-op> [ ( <expr>, ... ) ]

;;; <let-expr> ::= let { <let-item>; ... } in <expr> <let-item> ::= <var-decl-item> | <constraint-item<

;;; <gen-call-expr> ::= <ident-or-quoted-op>( <comp-tail>) ( <expr>)
;;; 
;;; ;;; B.4. Miscellaneous Elements


;;; <ident> ::= [A-Za-z][A-Za-z0-9_]* % excluding keywords | ’[^’\xa\xd\x0]*’

;;; <ident-or-quoted-op> ::= <ident> | ’<builtin-op>’
;;; <annotations> ::= ( :: <annotation>)*
;;; <annotation> ::= <expr-atom-head> <expr-atom-tail>
;;; 
;;; ;;; C. Content-types
;;; ;;; C.1. ‘application/x-zinc-output’ The content-type ‘application/x-zinc-output’ defines a text output format for Zinc.
;;; ;;; The format extends the abstract syntax and semantics given in Section 3.2, and is discussed in detail in Section 3.3.
;;; ;;;     The full syntax is as follows:
;;; <output> ::= <no-solutions> [ <warnings>] <free-text> | ( <solution>)* [ <complete>] [ <warnings>] <free-text>

;;; <solution> ::= <solution-text> [ \n ] ---------- \n

;;; <no-solutions> ::= =====UNSATISFIABLE===== \n

;;; <complete> ::= ========== \n

;;; <warnings> ::= ( <message>)+

;;; <message> ::= ( <line>)+ <line> ::= % [^\n]* \n

