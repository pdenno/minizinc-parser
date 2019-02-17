(ns pdenno.mznp.mznp
  "Parse MiniZinc to records."
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [clojure.set    :as sets]
            [clojure.spec.alpha :as s]
            [pdenno.mznp.utils :as util]))

;;; Purpose: Parse minizinc .mzn. 
;;; The 'defparse' parsing functions pass around complete state. 
;;; The 'parse state' (AKA pstate) is a map with keys:
;;;   :result  - the parse structure from the most recent call to (parse :<some-rule-tag> pstate)
;;;   :tokens  - tokenized content that needs to be parsed into :model. First on this vector is also :tkn.
;;;   :tags    - a stack describing where in the grammar it is parsing (used for debugging)
;;;   :tkn     - current token, not yet consumed. It is also the first token on :tokens. 
;;;   :line    - line in which token appears.
;;;   :col     - column where token starts. 
;;;   :error   - non-nil when things go wrong
;;;   :local   - temporarily stored parse content used later to form a complete grammar element. It is a vector of maps.
;;;             :local is used by the macros 'store' and 'recall'. 
;;;   :model   - the resulting cummulative parse structure

;;; Returns pstate: parse, eat-token, store, recall
;;; Returns something else: look, find-token

;;; The grammar implemented is 2.2.0. <Footnote 1> 
;;; The grammar is here: https://www.minizinc.org/doc-2.2.0/en/spec.html
;;; The grammar is here: https://www.minizinc.org/doc-2.2.0/en/spec.html#spec-grammar

;;; Footnote 1: Though I might have grabbed productions from an earlier version of MiniZinc,
;;;             I am updating to 2.2.0 wherever I find discrepancies. 

(def debugging? (atom true)) 

;;; ============ Tokenizer ===============================================================
;;; POD Could add to this from library...
(def ^:private mzn-keywords
  #{"ann", "annotation", "any", "array", "bool", "case", "constraint", "diff", "div", "else",
    "elseif", "endif", "enum", "false", "float", "function", "if", "in", "include", "int",
    "intersect", "let", "list", "maximize", "minimize", "mod", "not", "of", "op", "opt", "output", ; website shows "opt"
    "par", "predicate", "record", "satisfy", "set", "solve", "string", "subset", "superset",
    "symdiff", "test", "then", "true", "tuple", "type", "union", "var", "where", "xor"
    ;; global constraints  --  https://github.com/MiniZinc/libminizinc/tree/master/share/minizinc/std ... many more!
    "alldifferent", "all_equal", "all_equal_int", 
    ;; builtins.arithmetic -- https://github.com/MiniZinc/libminizinc/blob/master/share/minizinc/std/builtins.mzn ... many more!
    "sum", "product", "min", "max", "arg_min", "arg_max", "abs", "pow"   
    ;; builtins.logic
    "forall", "exists", "xorall", "clause", "iffall"})

(def ^:private mzn-long-syntactic ; chars that COULD start a multi-character syntactic elements. 
  #{\., \,, \\, \/, \<, \>, \=, \!, \+, \|, \[, \], \: \-})

(def ^:private mzn-syntactic ; chars that are valid tokens in themselves. 
  #{\[, \], \(, \), \{, \}, \=, \^, \,, \:, \;, \|, \*, \+, \-, \<, \>}) ; not \_

;;; POD multi-line comment (e.g. /* ... */ would go in here, sort of. 
(defn read-long-syntactic [st ws]
  (let [len (count st)
        c0  (nth st 0)
        c1  (and (> len 1) (nth st 1))
        c2  (and (> len 2) (nth st 2))]
    (when-let [result (cond (and (= c0 \.) (= c1 \.)) {:raw ".." :tkn :..-op}
                            (and (= c0 \-) (= c1 \>)) {:raw "->" :tkn :->-op}
                            (and (= c0 \<) (= c1 \-)) {:raw "<-" :tkn :<--op}
                            (and (= c0 \\) (= c1 \/)) {:raw "\\/" :tkn :or}
                            (and (= c0 \/) (= c1 \\)) {:raw "/\\" :tkn :and}
                            (and (= c0 \<) (= c1 \=)) {:raw "<=" :tkn :<=}
                            (and (= c0 \>) (= c1 \=)) {:raw ">=" :tkn :>=}
                            (and (= c0 \=) (= c1 \=)) {:raw "==" :tkn :==}
                            (and (= c0 \!) (= c1 \=)) {:raw "!=" :tkn :not=}
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
(defrecord MznEOLcomment [text line col])
(defrecord MznTypeInstVar [name])

;;; https://www.regular-expressions.info/modifiers.html (?s) allows  .* to match all characters including line breaks. 
(defn token-from-string
  "Return a map with keys :ws, :raw and :tkn from the front of the argument string."
  [stream line]
  (let [ws (whitesp stream)
        s (subs stream (count ws))
        c (first s)]
    ;(cl-format *out* "~%ws = ~S~%c = ~S~%STREAM = ~S" ws c stream)
    (or  (and (empty? s) {:ws ws :raw "" :tkn :eof})                    ; EOF
         (and (mzn-long-syntactic c) (read-long-syntactic s ws))        ; ++, <=, == etc. 
         (and (mzn-syntactic c) {:ws ws :raw (str c) :tkn c})           ; literal syntactic char.
         (when-let [[_ num] (re-matches #"(?s)(\d+(\.\d+(e[+-]?\d+)?)?).*" s)]
           {:ws ws :raw num :tkn (read-string num)}),                   ; number
         (when-let [[_ id] (re-matches #"(?s)('[^']*').*" s)]           ; identifer type 2 POD Need's work. 
           {:ws ws :raw id :tkn (->MznId id)})
         (when-let [[_ st] (re-matches #"(?s)(\"[^\"]*\").*" s)]        ; string literal
           {:ws ws :raw st :tkn (->MznString (read-string st))})
         (when-let [[_ cm] (re-matches #"(?s)(\%[^\n]*).*" s)]          ; EOL comment
           {:ws ws :raw cm :tkn (map->MznEOLcomment {:text cm})})
         (when-let [[_ tivar] (re-matches #"(?s)(\$[A-Za-z][A-Za-z0-9_]*)" s)]
           {:ws ws :raw tivar :tkn (->MznTypeInstVar tivar)})
         (let [pos (position-break s)
               word (subs s 0 (or pos (count s)))]
            (or 
             (and (mzn-keywords word) {:ws ws :raw word :tkn (keyword word)})  
             (when-let [[_ id] (re-matches #"^([a-zA-Z][A-Za-z0-9\_]*).*" word)]     ; identifer type 1 
               {:ws ws :raw id :tkn (->MznId id)})))
         (throw (ex-info "Char starts no known token: " {:raw c :line line})))))

(defn tokenize
  "Return a vector of tokens. A token is a map with keys :tkn, :line :col."
  [stream]
  (loop [s stream 
         tkns []
         line 1
         col 1]
    (let [lex (token-from-string s line) ; Returns a map with keys :ws :raw and :tkn.
          new-lines (count (re-seq #"\n" (:ws lex))) ; :ws is in front of token. 
          col (if (> new-lines 0)
                (- (count (:ws lex)) (str/last-index-of (:ws lex) "\n"))
                (+ (count (:ws lex)) col))]
      (if (= :eof (:tkn lex))
        (conj tkns {:tkn :eof :line line :col col})
        (recur
         (subs s (+ (count (:raw lex)) (count (:ws lex))))
         (conj tkns {:tkn (:tkn lex) :line (+ line new-lines) :col col})
         (+ line new-lines)
         (+ col (count (:raw lex))))))))

;;; ============ Parser Utilities ============================================================
(defn look
  "Returns a token, not the pstate."
  [pstate n]
  (if (>= n (count (:tokens pstate)))
    :eof
    (-> (nth (:tokens pstate) n) :tkn)))

(defn match-tkn
  "Return true if token matches test, which is a string, character, fn or regex."
  [test tkn]
  (cond (= test tkn) true
        (set? test) (test tkn)
        (fn? test) (test tkn)
        (instance? java.util.regex.Pattern test) (re-matches test tkn)
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

(defn token-vec [pstate] (mapv :tkn (:tokens pstate)))

(def balanced-map "What balances with the opening syntax?" { \{ \}, \( \), \[ \] :2d-array-open :2d-array-close})
(def balanced-inv (sets/map-invert balanced-map))
  
(defn find-token
  "Return position if tkn is found within the item (before semicolon)."
  [tvec tkn]
  (when (not-empty tvec)
    (let [pos-semi (.indexOf tvec \;) 
          pos-semi (if (pos? pos-semi) pos-semi (count tvec)) ; In testing, might not have full item; not semi. 
          pos-tkn  (.indexOf tvec tkn)]
      (cond (== pos-tkn  -1) nil,
            (and (pos? pos-semi) (< pos-semi pos-tkn)) nil,
            :else pos-tkn))))

;;; (find-token-balanced [ \{, \{, :foo, \}, \}, ] \}) ==> 4
(defn find-token-balanced
  "Return the position of a balanced instance of the argument token (a close-syntax token).
   Thus if tvec is [ \\{, \\{, foo, \\}, \\}, ] it is 4, not 3. Return nil if none."
  [tvec close-tkn]
  (when-let [open-tkn (balanced-inv close-tkn)]
    (assert (= open-tkn (first tvec)))
    (loop [cnt 1
           pos 0
           tvec (rest tvec)]
      (cond (== 0 cnt) pos
            (empty? tvec) nil
            :else
            (let [tkn (first tvec)]
              (recur (cond (= tkn open-tkn)  (inc cnt)
                           (= tkn close-tkn) (dec cnt)
                           :else cnt)
                     (inc pos)
                     (rest tvec)))))))

(defn balanced? 
  "Return true if, before position POS there is a closing syntax character for each 
  argument TKN opening syntax character." 
  [tvec open-tkn pos]
  (let [close-tkn (get balanced-map open-tkn)]
    (== 0 (reduce (fn [cnt tkn]
                    (cond (= tkn open-tkn) (inc cnt)
                          (= tkn close-tkn) (dec cnt)
                          :else cnt))
                  0
                  (subvec tvec 0 pos)))))

(defmacro defparse [tag [pstate & keys-form] & body]
  `(defmethod parse ~tag [~'tag ~pstate ~@(or keys-form '(& ignore))]
     (when @debugging? (cl-format *out* "~%~A==> ~A" (util/nspaces (-> ~pstate :tags count)) ~tag))
     (as-> ~pstate ~pstate
       (update-in ~pstate [:tags] conj ~tag)
       (update-in ~pstate [:local] #(into [{}] %))
       (if (:error ~pstate) ; Stop things
         ~pstate 
         ~@body)
       (cond-> ~pstate (not-empty (:tags ~pstate)) (update-in [:tags] pop))
       (update-in ~pstate [:local] #(vec (rest %)))
       (do (when @debugging? (cl-format *out* "~%~A<-- ~A" (util/nspaces (-> ~pstate :tags count)) ~tag))
           ~pstate))))

;;; Abbreviated for simple forms such as builtins. 
(defmacro defparse-auto [tag test]
  (let [pstate# nil]
    `(defparse ~tag
       [pstate#]
       (-> pstate#
           (assoc :result (:tkn pstate#))
           (eat-token ~test)))))

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

(defn make-pstate
  "Make a parse state map from tokens, includes separating comments from code."
  [tokens+comments]
  (let [tokens (remove #(instance? MznEOLcomment (:tkn %)) tokens+comments)
        comments (->> tokens+comments
                      (filter #(instance? MznEOLcomment (:tkn %)))
                      (mapv #(->MznEOLcomment (-> % :tkn :text) (:line %) (:col %))))]
  {:tokens (vec tokens)
   :tkn (-> tokens first :tkn)
   :tags []
   :local []
   :comments comments}))

;;; (parse-string ::var-decl-item "array[Workers, Tasks] of int: cost;")
(defn parse-string
  "Toplevel parsing function"
  ([str] (parse-string ::model str))
  ([tag str]
    (let [pstate (->> str tokenize make-pstate (parse tag))]
      (if (not= (:tkn pstate) :eof)
        (do (when @debugging? (println "\n\nPARSING ENDED PREMATURELY."))
            (assoc pstate :error {:reason "Parsing ended prematurely."}))
        pstate))))

(defn parse-file
  "Parse a whole file given a filename string."
  [filename]
  (parse-string ::model (slurp filename)))

(defn parse-ok?
  "Return true if the string parses okay."
  [tag text]
  (as-> (parse-string tag text) ?pstate
    (and (not (contains? ?pstate :error))
         (= :eof (-> ?pstate :tokens first :tkn))
         (or (not (contains? (s/registry) tag))
             (s/valid? tag (:result ?pstate))))))

;;;========================= Implementation of Grammar ==========================
(defn parse-list
  "Does parse parametrically for <open-char> [ <item> ','... ] <close-char>"
  ([pstate char-open char-close]
   (parse-list pstate char-open char-close ::expr))
  ([pstate char-open char-close parse-tag]
   (as-> pstate ?ps
     (eat-token ?ps char-open)
     (assoc-in ?ps [:local 0 :items] [])
     (loop [ps ?ps]
       (cond
         (= :eof (:tkn ps))
         (assoc ps :error {:while "parsing a list" :char-open char-open :parse-tag parse-tag :line (:line ps)}),
         (= char-close (:tkn ps))
         (as-> ps ?ps1
           (eat-token ?ps1)
           (assoc ?ps1 :result (recall ?ps1 :items))),
         :else
         (as-> ps ?ps1
           (parse parse-tag ?ps1)
           (update-in ?ps1 [:local 0 :items] conj (:result ?ps1))
           (if (contains? ?ps1 :error)
             ?ps1
             (recur (cond-> ?ps1 (= \, (:tkn ?ps1)) (eat-token \,))))))))))

(defn parse-list-terminated
  "Does parse parametrically for '[ <item> ','... ] <terminator>'. Does not eat terminator."
  ([pstate term-fn]
   (parse-list-terminated pstate term-fn ::expr))
  ([pstate term-fn parse-tag]
   (as-> pstate ?ps
     (assoc-in ?ps [:local 0 :items] [])
     (loop [ps ?ps]
       (cond
         (= :eof (:tkn ps))
         (assoc ps :error {:while "parsing a list" :parse-tag parse-tag :line (:line ps)}),
         (term-fn (:tkn ps))
         (assoc ps :result (recall ps :items)),
         :else
         (as-> ps ?ps
           (parse parse-tag ?ps)
           (update-in ?ps [:local 0 :items] conj (:result ?ps))
           (if (contains? ?ps :error)
             ?ps
             (recur (cond-> ?ps (= \, (:tkn ?ps)) (eat-token \,))))))))))

;;; <builtin-num-bin-op> ::= + | - | * | / | div | mod
(def builtin-num-bin-op #{\+ \- \* \/ :div :mod})
(defparse-auto :builtin-bin-op builtin-num-bin-op)

;;;  <builtin-bin-op> ::= <-> | -> | <- | \/ | xor | /\ | < | > | <= | >= | == | = | != | in |
;;;                       subset | superset | union | diff | symdiff | .. | intersect| ++ | <builtin-num-bin-op>
(def builtin-bin-op
  (into #{:<->-op  :->-op  :<-op  :or :xor :and \< \> :<= :>= :== \= :not= :in,
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
(def builtin-quantifier #{:forall :exists :xorall :clause})

(def builtin-constraint #{:alldifferent :all-equal})

;;; <builtin-op> ::= <builtin-bin-op> | <builtin-un-op>
(def builtin-op
  (sets/union builtin-bin-op
              builtin-un-op
              builtin-arithmetic-op
              builtin-quantifier
              builtin-constraint))

(def builtin-agg-fn #{:sum :product :min :max}) ; I'm guessing see page 23 of Tutorial

(def builtin-gen-call-fn
  (sets/union builtin-agg-fn builtin-quantifier))
  
(defparse-auto :builtin-op builtin-op)
;;;--------------------End Library Builtins ------------------------------------
(s/def ::model (s/keys :req-un [::items]))

;;; <model>::= [ <item> ; ... ]
(defrecord MznModel [items])
(defparse ::model ; top-level grammar element. 
  [pstate] 
  (loop [ps (assoc pstate :model (->MznModel []))]
    (if (= :eof (:tkn ps))
      (assoc ps :result {:items :success}) ; :result needed for spec
      (recur 
       (as-> ps ?ps
         (parse ::item ?ps)
         (update-in ?ps [:model :items] conj (:result ?ps))
         (eat-token ?ps \;))))))

(defn var-decl? 
  "Returns true if head looks like it can start a var-decl"
  [pstate]
  (let [tkn (:tkn pstate)]
    (or (#{:bool :int :float :string :var :par} tkn) ; base-ti-expr
        (instance? MznTypeInstVar tkn)               ; base-ti-expr-tail
        (#{:set :array :list} tkn)                   ; set-ti-expr-tail, :array-ti-expr-tail
        (= \{ tkn)                                   ; 
        (#{:ann :opt} tkn)                           ; others
        (instance? MznTypeInstVar tkn)               ; base-ti-expr-tail ... <ti-variable-expr-tail>
        (find-token (token-vec pstate) :..-op))))                ; base-ti-expr-tail ... <num-expr> ".."  <num-expr> 
      
;;; <item>::= <include-item> | <var-decl-item> | <assign-item> | <constraint-item> | <solve-item> |
;;;            <output-item> | <predicate-item> | <test-item> | <function-item> | <annotation-item>
(defparse ::item
  [pstate]
  (let [tkn  (:tkn pstate)
        tkn2 (look pstate 1)]
    (cond (= tkn :include)                (parse ::include-item pstate),
          (= tkn :constraint)             (parse ::constraint-item pstate),
          (= tkn :solve)                  (parse ::solve-item pstate),
          (= tkn :output)                 (parse ::output-item pstate),
          (= tkn :predicate)              (parse ::predicate-item pstate),
          (= tkn :test)                   (parse ::test-item pstate),
          (= tkn :function)               (parse ::function-item pstate),
          (= tkn :ann)                    (parse ::annotation-item pstate) ; I don't think "annotation" is a keyword.
          (and (instance? MznId tkn) (= tkn2 \=)) (parse ::assign-item pstate),
          (var-decl? pstate)              (parse ::var-decl-item pstate),
          :else (assoc pstate :error {:expected "a MZn item" :got (:tkn pstate) :in :item :line (:line pstate)}))))

;;; 4.1.6 Each type has one or more possible instantiations. The instantiation of a variable or value indicates
;;;       if it is fixed to a known value or not. A pairing of a type and instantiation is called a type-inst.
;;;       Reading further the "instantiation" indicates "how fixed or unfixed" its value is.
;;;       "instance-time" as opposed to "run-time" is the model as defined in .mzn.

;;; <type-inst-syn-item> ::= type <ident> <annotations>= <ti-expr>
;;;; POD I don't see where this is used

;;; <ti-expr-and-id> ::= <ti-expr> ":" <ident>
(defrecord MznIdDef [id id-type])
(defparse ::ti-expr-and-id
  [pstate]
  (as-> pstate ?ps
    (parse ::ti-expr ?ps)
    (store ?ps :type)
    (eat-token ?ps \:)
    (parse ::ident ?ps)
    (store ?ps :id)
    (assoc ?ps :result (->MznIdDef (recall ?ps :id)
                                   (recall ?ps :type)))))
    
(defrecord MznInclude [model-part])
;;; <include-item> ::= include <string-literal>
(s/def ::model-part #(do % true))

(s/def ::include-item
  (s/and
   (s/keys :req-un [::model-part])
   #(re-matches #"[a-z,A-Z,0-9\-_]+\.mzn" (-> % :model-part :str))))

(defparse ::include-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :include)
    (parse ::string-literal ?ps)
    (store ?ps :model-part)
    (assoc ?ps :result (->MznInclude (recall ?ps :model-part)))))

;;; <assign-item> ::= <ident> = <expr>
(defrecord MznAssignment [lhs rhs])
(defparse ::assign-item
  [pstate]
  (as-> pstate ?ps
    (parse ::ident ?ps)
    (store ?ps :lhs)
    (eat-token ?ps \=)
    (parse ::expr ?ps)
    (assoc ?ps :result (->MznAssignment (recall ?ps :lhs) (:result ?ps)))))

(defrecord MznConstraint [expr])
;;; <constraint-item> ::= constraint <expr>
(s/def ::constraint-item #(instance? MznConstraint %))

(defparse ::constraint-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :constraint)
    (parse ::expr ?ps)
    (assoc ?ps :result (->MznConstraint (:result ?ps)))))

(defrecord MznSolve [action expr anns])
;;; <solve-item> ::= solve <annotations> satisfy | solve <annotations> minimize <expr> | solve <annotations> maximize <expr>
(s/def ::solve-item #(instance? MznSolve %))

(defparse ::solve-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :solve)
    (parse ::annotations ?ps)
    (store ?ps :anns)
    (store ?ps :action :tkn) ; Stores value of :tkn, not :result.
    (eat-token ?ps #{:satisfy :minimize :maximize})
    (if (= :satisfy (recall ?ps :action))
      (assoc ?ps :result nil)
      (parse ::expr ?ps))
    (assoc ?ps :result (->MznSolve (recall ?ps :action)
                                   (:result ?ps)
                                   (recall ?ps :anns)))))

(defrecord MznVarDecl [lhs rhs anns])
(s/def ::var-decl-item
  (s/keys :req-un [::lhs ::rhs ::anns]))

;;; <var-decl-item> ::= <ti-expr-and-id> <annotations> [ "=" <expr> ]
(defparse ::var-decl-item
  [pstate]
  (as-> pstate ?ps
    (parse ::ti-expr-and-id ?ps)
    (store ?ps :lhs)
    (parse ::annotations ?ps)
    (store ?ps :anns)
    (if (= (:tkn ?ps) \=)
      (as-> ?ps ?ps1
          (eat-token ?ps1)
          (parse ::expr ?ps1)
          (assoc ?ps1 :result (->MznVarDecl (recall ?ps1 :lhs)
                                            (:result ?ps1)
                                            (recall ?ps1 :anns))))
      (assoc ?ps :result (->MznVarDecl
                          (recall ?ps :lhs)
                          nil
                          (recall ?ps :anns))))))

(defrecord MznOutputItem [expr])
(s/def ::output-item #(instance? MznOutputItem %))

;;; <output-item> ::= output <expr> 
(defparse ::output-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :output)
    (parse ::expr ?ps)
    (assoc ?ps :result (->MznOutputItem (:result ?ps)))))

(defrecord MznAnnItem [id params])
;;; <annotation-item> ::= annotation <ident> <params>
(defparse ::annotation-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :annotation)
    (parse ::ident ?ps)
    (store ?ps :id)
    (parse ::params ?ps)
    (assoc ?ps :result (->MznAnnItem (recall ?ps :id) (:result ?ps)))))

;;; <annotations> ::= [ "::" <annotation> ]*
(defparse ::annotations
  [pstate]
  (if (= (:tkn pstate) :ann-sep)
    (as-> pstate ?ps
      (assoc-in ?ps [:local 0 :anns] [])
      (loop [ps ?ps]
        (as-> ps ?ps1
          (eat-token ?ps1 :ann-sep)
          (parse ::annotation ?ps1)
          (update-in ?ps1 [:local 0 :anns] conj (:result ?ps1))
          (if (not= (:tkn ?ps1) :ann-sep)
            (assoc ?ps1 :result (recall ?ps1 :anns))
            (recur ?ps1)))))
    (assoc pstate :result nil)))

(defrecord MznAnnotation [head tail])
;;; <annotation> ::= <expr-atom-head> <expr-atom-tail>
(defparse ::annotation
  [pstate]
  (as-> pstate ?ps
    (parse ::expr-atom-head ?ps)
    (store ?ps :head)
    (parse ::expr-atom-tail ?ps)
    (assoc ?ps :result (->MznAnnotation (recall ?ps :head) (:result ?ps)))))

(defrecord MznPredItem [pred])
;;; <predicate-item> ::= predicate <operation-item-tail>
(defparse ::predicate-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :predicate)
    (parse ::operation-item-tail ?ps)
    (assoc ?ps :result (->MznPredItem (:result ?ps)))))

(defrecord MznTestItem [pred])
;;; <test-item> ::= test <operation-item-tail>
(defparse ::test-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :test)
    (parse ::operation-item-tail ?ps)
    (assoc ?ps :result (->MznTestItem (:result ?ps)))))

(defrecord MznFnItem [expr op])
;;; <function-item> ::= function <ti-expr> ":" <operation-item-tail>
(defparse ::function-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :function)
    (parse ::ti-expr ?ps)
    (store ?ps :expr (:result ?ps))
    (eat-token ?ps \:)
    (parse ::operation-item-tail ?ps)
    (assoc ?ps :result (->MznFnItem (recall ?ps :expr)(:result ?ps)))))

(defrecord MznOpItemTail [id params expr ann])
;;; <operation-item-tail> ::= <ident> <params> <annotations> [ "=" <expr>]
(defparse ::operation-item-tail
  [pstate]
  (as-> pstate ?ps
    (parse ::ident ?ps)
    (store ?ps :id :tkn)
    (parse ::params ?ps)
    (store ?ps :params)
    (parse ::annotations ?ps)
    (store ?ps :anns)
    (if (= (:tkn ?ps) \=)
      (as-> ?ps ?ps1
          (eat-token ?ps1)
          (parse ::expr ?ps1)
          (assoc ?ps1 :result (->MznOpItemTail (recall ?ps1 :id)
                                               (recall ?ps1 :params)
                                               (:result ?ps1)
                                               (recall ?ps1 :anns))))
      (assoc ?ps :result (->MznOpItemTail
                          (recall ?ps :id)
                          (recall ?ps :params)
                          nil
                          (recall ?ps :anns))))))

;;; Example: function var T: enum_next(set of T: X, var T: x);
;;; The whole thing is optional, but functions start with "function".
;;; <params> ::= [ "(" <ti-expr-and-id>, ... ")" ]
(defparse ::params
  [pstate]
  (if (= \( (:tkn pstate))
    (parse-list pstate \( \) ::ti-expr-and-id)
    (assoc pstate :result [])))

;;; ;;; B.2. Type-Inst Expressions

;;; <ti-expr> ::= <base-ti-expr>
(defparse ::ti-expr
  [pstate]
  (parse ::base-ti-expr pstate))

(defrecord MznTypeInstExpr [var? par? expr])
;;; <base-ti-expr> ::= <var-par> <base-ti-expr-tail>
;;; <var-par> ::= var | par | ε
(defparse ::base-ti-expr
  [pstate]
  (let [var-par? (:tkn pstate)]
    (as-> pstate ?ps
      (cond-> ?ps (#{:var :par} var-par?) (eat-token))
      (parse ::base-ti-expr-tail ?ps)
      (assoc ?ps :result (map->MznTypeInstExpr {:expr (:result ?ps)}))
      (cond-> ?ps (= :var var-par?) (assoc-in [:result :var?] true))
      (cond-> ?ps (= :par var-par?) (assoc-in [:result :par?] true)))))

;;; <base-type> ::= "bool" | "int" | "float" | "string"
(defparse-auto ::base-type #{:bool :int :float :string})

(defrecord MznIntegerRange [from to])
(defrecord MznSetLiterals  [elems])
;;; <base-ti-expr-tail> ::= <ident> | <base-type> | <set-ti-expr-tail> |
;;;                          <array-ti-expr-tail> | ann | opt <base-ti-expr-tail> | "{" <expr>, ... "}" |
;;;                          <num-expr> .. <num-expr>
(defparse ::base-ti-expr-tail
  [pstate]
  (let [tkn (:tkn pstate)]
    (cond (instance? MznId tkn)              ; <ident>
          (parse ::ident pstate)
          (#{:bool :int :float :string} tkn) ; <base-type}
          (parse ::base-type pstate),
          (= :set tkn)                       ; <set-ti-exp-tail>
          (parse ::set-ti-expr-tail pstate),
          (instance? MznTypeInstVar tkn)     ; <ti-variable-expr-tail>
          (-> pstate
              (assoc :result tkn)
              eat-token),
          (#{:array :list} tkn)
          (parse ::array-ti-expr-tail pstate),
          (= tkn :ann)                       ; ann
          (assoc pstate :error {:msg "Annotations NYI." :line (:line pstate)}),
          (#{:opt :op} tkn)                  ; opt <base-ti-expr-tail>
          (as-> pstate ?ps
              (eat-token ?ps)
              (parse ::base-ti-expr-tail ?ps)
              (assoc-in ?ps [:result :optional?] true)),
          (= tkn \{)                        ; "{" <expr>, ... "}" 
          (parse ::set-literal pstate),
          (find-token (token-vec pstate) :..-op)        ; <num-expr> ".."  <num-expr> ; call it a range expression
          (parse ::num-expr pstate)
          :else
          (assoc pstate :error {:expected :base-ti-expr-tail :tkn (:tkn pstate)
                                :line (:line pstate) :col (:col pstate)}))))

(defrecord MznSetType [base-type optional?])
(s/def ::set-ti-expr-tail (s/keys :req-un [::base-type ::optional?]))

;;; <set-ti-expr-tail> ::= set of <ti-expr>  POD It said <base-type> here, not <ti-expr> See knapsack.mzn
(defparse ::set-ti-expr-tail
  [pstate]
  (as-> pstate ?ps
      (eat-token ?ps :set)
      (eat-token ?ps :of)
      (parse ::ti-expr ?ps)
      (assoc ?ps :result (->MznSetType (:result ?ps) nil))))
          
(defrecord MznArrayType [index base-type optional?])
(defrecord MznListType [base-type]) 
;;; <array-ti-expr-tail> ::= array "[" <ti-expr>, ... "]" of <ti-expr> | list of <ti-expr> ; POD added " around "[".
(defparse ::array-ti-expr-tail
  [pstate]
  (cond (= (:tkn pstate) :array)
        (as-> pstate ?ps
          (eat-token ?ps :array)
          (parse-list ?ps \[ \] ::ti-expr) 
          (store ?ps :ti-list)
          (eat-token ?ps :of)
          (parse ::ti-expr ?ps)
          (assoc ?ps :result (->MznArrayType (recall ?ps :ti-list) (:result ?ps) nil)))
        (= (:tkn pstate) :list)
        (as-> pstate ?ps
          (eat-token ?ps :list)
          (eat-token ?ps :of)
          (parse ::ti-expr ?ps)
          (assoc ?ps :result (->MznListType (:result ?ps))))
        :else
        (assoc pstate :error {:expected "array or list"
                              :got (:tkn pstate)
                              :line (:line pstate)
                              :in :array-ti-expr-tail})))

;;; <ti-variable-expr-tail> ::= $[A-Za-z][A-Za-z0-9_]*
(defparse-auto ::ti-variable-expr-tail #(instance? MznTypeInstVar %))

;;; <op-ti-expr-tail> ::= opt ( <ti-expr>: ( <ti-expr>, ... ) )
;;; I don't see where in the grammar this is used!
;;; POD Shows "opt" not "op" on website.

(defn quoted-id? [pstate]
  (and (= \' (:tkn pstate))
       (instance? MznId (look pstate 2))
       (= \' (look pstate 3))))

(defrecord MznExpr [atom tail])
;;; I think the grammar is botched here. <expr-binop-tail> should be optional. That's 
;;;  4.1.7.1 Expression Overview
;;; <expr> ::= <expr-atom> <expr-binop-tail>
(defparse ::expr
  [pstate]
  (as-> pstate ?ps
    (parse ::expr-atom ?ps)
    (store ?ps :atom)
    (if (or (builtin-bin-op (:tkn ?ps))
            (quoted-id? pstate))
      (as-> ?ps ?ps1
        (parse ::expr-binop-tail ?ps1)
        (store ?ps1 :tail))
      (assoc-in ?ps [:local 0 :tail] nil))
    (assoc ?ps :result (->MznExpr (recall ?ps :atom) (recall ?ps :tail)))))

(defrecord MznExprAtom [head tail ann])
;;; <expr-atom> ::= <expr-atom-head> <expr-atom-tail> <annotations>
(defparse ::expr-atom
  [pstate]
  (as-> pstate ?ps
    (parse ::expr-atom-head ?ps)
    (store ?ps :head)
    (parse ::expr-atom-tail ?ps)
    (store ?ps :tail)
    (parse ::annotations ?ps)
    (assoc ?ps :result (->MznExprAtom
                        (recall ?ps :head)
                        (recall ?ps :tail) 
                        (:result ?ps)))))

(defrecord MznExprAtomTail [array-access])
;;;  <expr-atom-tail> ::= ε | <array-access-tail> <expr-atom-tail>
(defparse ::expr-atom-tail
  [pstate]
  (if (= \[ (:tkn pstate))
    (as-> pstate ?ps
      (assoc-in pstate [:local 0 :atoms] [])
      (loop [ps ?ps]
        (as-> ps ?ps1
          (parse ::array-access-tail ?ps1)
          (update-in ?ps1 [:local 0 :atoms] conj (:result ?ps1))
          (if (not= \[ (:tkn ?ps1))
            (assoc ?ps1 :result (->MznExprAtomTail (-> ?ps1 :local first :atoms)))
            (recur ?ps1)))))
    (assoc pstate :result nil)))

(defrecord MznExprBinopTail [bin-op expr])
;;; POD I think the grammar is botched here. The [ ] should be BNF optional, not terminals!
;;; <expr-binop-tail> ::= "[" <bin-op> <expr> "]"
(defparse ::expr-binop-tail
  [pstate]
  (as-> pstate ?ps
    (parse ::bin-op ?ps)
    (store ?ps :bin-op)
    (parse ::expr ?ps)
    (assoc ?ps :result (->MznExprBinopTail
                        (recall ?ps :bin-op)
                        (:result ?ps)))))

(defn ident-or-quoted-op?
  "Returns true if head satisfies <ident-or-quoted-op> ::= <ident> | ’<builtin-op>’"
  [pstate]
  (let [tkn (:tkn pstate)
        tkn2 (look pstate 1)
        tkn3 (look pstate 2)]
    (or (and (instance? MznId tkn)
             (not= tkn2 \())       ; Prevent identification when it is a call-op without known builtin (e.g. noattack)
        (and (= tkn \')
             (builtin-op tkn2)
             (= tkn3 \')
             (not= tkn2 \())))) ; Prevent identification when it is a call-op without known builtin (e.g. noattack)

(defrecord MznExprUnOp [uni-op atom])
;;;  <expr-atom-head> ::= <builtin-un-op> <expr-atom> | ( <expr>) | <ident-or-quoted-op> |
;;;                       _ | <bool-literal> | <int-literal> | <float-literal> | <string-literal> |
;;;                       <set-literal> | <set-comp> | <array-literal> | <array-literal-2d> |
;;;                       <array-comp> | <ann-literal> | <if-then-else-expr> | <let-expr> | <call-expr> |
;;;                       <gen-call-expr>
(declare part1 part2 part3)
(defparse ::expr-atom-head
  [pstate]
  (let [tkn (:tkn pstate)]
    (or (part1 pstate tkn)     ; through <string-literal>
        (part2 pstate tkn))))  ; through end 

(defn part1 [pstate tkn]
  (cond (builtin-un-op tkn)                     ; <builtin-un-op> <expr-atom>
        (as-> pstate ?ps
          (eat-token ?ps)
          (parse ::expr-atom ?ps)
          (assoc ?ps :result (->MznExprUnOp tkn (:result ?ps)))),
        (= \( tkn)                              ; ( <expr )
        (as-> pstate ?ps
          (eat-token ?ps)
          (parse ::expr ?ps)
          (eat-token ?ps \))
          (assoc ?ps :result (map->MznExpr {:primary? true :atom (:result ?ps)}))),
        (ident-or-quoted-op? pstate)           ; <ident-or-quoted-op>
        (parse ::ident-or-quoted-op pstate),
        (= \_ tkn)                             ; _
        (-> pstate                              
            eat-token
            (assoc :result \_)),
        (#{:false :true} tkn)                  ; bool-literal
        (parse ::bool-literal pstate),
        (integer? tkn)                         ; int-literal
        (parse ::int-literal pstate),
        (float? tkn)                           ; float-literal
        (parse ::float-literal pstate), 
        (instance? MznString tkn)              ; string-literal
        (parse ::string-literal pstate)))

(defn gen-call-expr? [tvec]
  (let [pos-close (find-token tvec \))
        pos-in (find-token tvec :in)]
    (and (builtin-gen-call-fn (first tvec))
         (= (nth tvec 1) \()
         pos-close
         pos-in
         (< pos-in pos-close))))

;;; From page 23 of the tutorial:
;;; A generator call expression has form
;;; 〈agg-func〉 ( 〈generator-exp〉 ) ( 〈exp〉 )
;;; The round brackets around the generator expression 〈generator-exp〉 and the constructor
;;; expression 〈exp〉 are not optional: they must be there.

;;;  <expr-atom-head> ::= <part1> |
;;;                       <set-literal> | <set-comp> | <array-literal> | <array-comp> | <array-literal-2d> |
;;;                       <ann-literal> | <if-then-else-expr> | <let-expr> | <call-expr> |
;;;                       <gen-call-expr>
(defn part2 [pstate tkn]
  (let [tvec (token-vec pstate)
        gen-bar-pos   (find-token tvec \|)
        close-set-pos (and (= (first tvec) \{) (find-token-balanced tvec \}))
        close-arr-pos (and (= (first tvec) \[) (find-token-balanced tvec \]))
        balanced-set? (or (not gen-bar-pos) (balanced? tvec \{ gen-bar-pos))
        balanced-arr? (or (not gen-bar-pos) (balanced? tvec \[ gen-bar-pos))
        tkn2 (look pstate 1)]
    (cond (and (= tkn \{) (or (not gen-bar-pos)
                              (and close-set-pos (< close-set-pos gen-bar-pos))
                              balanced-set?))                         ; <set-literal>
          (parse ::set-literal pstate),                                
          (= tkn \{)                                                  ; <set-comp>
          (parse ::set-comp pstate),                                   
          (and (= tkn \[) (or (not gen-bar-pos)
                              (and close-arr-pos (< close-arr-pos gen-bar-pos))
                              balanced-arr?))                         ; <array-literal>
          (parse ::array-literal pstate),
          (= tkn \[)                                                  ; <array-comp>  "[ s[i] | i in 1..n]"
          (parse ::array-comp pstate),
          (= tkn :2d-array-open)                                      ; <array-literal-2d>
          (parse ::array-literal-2d pstate),
          (= tkn :ann-sep) ; POD needs investigation!                 ; <ann-literal>
          (parse ::ann-literal pstate),
          (= tkn :if)                                                 ; <if-then-else-expr>
          (parse ::if-then-else-expr pstate),                          
          (= tkn :let)                                                ; <let-expr>
          (parse ::let-expr),                                          
          (gen-call-expr? tvec)     ; POD I'm making this up          ; <gen-call-expr> e.g. "sum (w in Workers) (cost[w,doesTask[w]])"
          (parse ::gen-call-expr pstate)                              ;                       OR forall    
          (or (builtin-op tkn)                                        ; <call-expr>
              (and (instance? MznId tkn)
                   (= \( tkn2)))
          (parse ::call-expr pstate)
          :else
          (assoc pstate :error {:expected "expr-atom-head (lots of stuff)"
                                :got (:tkn pstate) :line (:line pstate)}))))

(defrecord MznQuotedOp [op])
;;; 4.1.7.3. Expression Atoms suggests that they are serious about this.
;;; BTW whitespace is not allowed between the quotes. POD Currently impossible to enforce.
;;; <ident-or-quoted-op> ::= <ident> | "'" <builtin-op> "'"
(defparse ::ident-or-quoted-op
  [pstate]
  (cond (instance? MznId (:tkn pstate))
        (parse ::ident pstate),
        (ident-or-quoted-op? pstate)
        (as-> pstate ?ps
          (eat-token ?ps \')
          (assoc ?ps :result (->MznQuotedOp (:tkn ?ps)))
          (eat-token ?ps builtin-op)
          (eat-token ?ps \'))
        :else
        (assoc pstate :error {:expected "ident-or-quoted-op" :got (:tkn pstate) :line (:line pstate)})))

(defparse ::ident
  [pstate]
  (as-> pstate ?ps
    (assoc ?ps :result (:tkn ?ps))
    (eat-token ?ps #(instance? MznId %))))

;;; <num-expr> ::= <num-expr-atom> <num-expr-binop-tail>   ; Just like <expr>
(defparse ::num-expr
  [ps] 
  (parse ::expr ps)) ; POD Fix this. 

;;; <num-expr-atom> ::= <num-expr-atom-head> <expr-atom-tail> <annotations> ; Much like <expr-atom>
(defparse ::num-expr-atom
  [ps]
  (parse ::expr-atom ps)) ; POD Fix this. 


;;; <num-expr-binop-tail> ::= [ <num-bin-op> <num-expr>]
(defparse ::num-expr-binop-tail
  [ps]
  (parse ::expr-binop-tail ps)) ; POD Fix this. 


;;; <num-bin-op> ::= <builtin-num-bin-op> | ‘<ident>‘
(defparse-auto ::num-bin-op #(or (instance? MznId %) (builtin-num-bin-op %)))

;;; <num-expr-atom-head> ::= <builtin-num-un-op> <num-expr-atom> | ( <num-expr> ) | <ident-or-quoted-op> |
;;;                          <int-literal> | <float-literal> | <if-then-else-expr> | <case-expr> | <let-expr> |
;;;                          <call-expr> | <gen-call-expr>

;;; <bin-op> ::= <builtin-bin-op> | ‘<ident>‘
(defparse ::bin-op
  [pstate]
  (if (builtin-bin-op (:tkn pstate))
    (as-> pstate ?ps
      (assoc ?ps :result (:tkn ?ps))
      (eat-token ?ps))
    (as-> pstate ?ps
      (eat-token ?ps \')
      (parse ::ident ?ps)
      (assoc ?ps :result (->MznQuotedOp (:result ?ps)))
      (eat-token ?ps \'))))

;;; <bool-literal> ::= false | true
(defparse-auto ::bool-literal #{:false :true})
(defparse-auto ::int-literal #(integer? %))
(defparse-auto ::float-literal #(float? %))
(defparse-auto ::string-literal #(instance? MznString %))

;;; <set-literal> ::= "{" [ <expr>, ... ] "}"
(defrecord MznSetLiteral [elems]) 
;;; "{" <expr>, ... "}"
;;; (parse ::set-literal {:tkn \{ :tokens [1 \, 2 \, 3 \}] })
(defparse ::set-literal
  [pstate]
  (as-> pstate ?ps
    (parse-list ?ps \{ \} ::expr)
    (assoc ?ps :result (->MznSetLiteral (:result ?ps)))))

(defrecord MznSetComp [expr comp-tail])
;;; <set-comp> ::= "{" <expr> "|" <comp-tail> "}"
(defparse ::set-comp
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps \{)
    (parse ::expr ?ps)
    (store ?ps :expr)
    (eat-token ?ps \|)
    (parse ::comp-tail ?ps)
    (eat-token ?ps \})
    (assoc ?ps :result (->MznSetComp
                        (recall ?ps :expr)
                        (:result ?ps)))))

(defrecord MznArray [elems])
;;; <array-literal> ::= "[" [ <expr>, ... ] "]"
(defparse ::array-literal
  [pstate]
  (as-> pstate ?ps
    (parse-list ?ps \[ \])
    (assoc ?ps :result (->MznArray (:result ?ps)))))

;;; [|10, 20, 13, |22, 11, 31, |14, 20, 18|] -- Note the extra commas!
(defrecord Mzn2dArray [sublists])
(s/def ::sublists
  (s/and (s/coll-of vector?)
         #(apply = (map count %))))
(s/def ::array-literal-2d
  (s/keys :req-un [::sublists]))

;;; <array-literal-2d> ::= "[|" [ (<expr>, ... ) "|" ... ] "|]"
(defparse ::array-literal-2d
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :2d-array-open)
    (assoc-in ?ps [:local 0 :sublists] [])
    (loop [ps ?ps]
      (as-> ps ?ps1
        (parse-list-terminated ?ps1 #(or (= % \|) (= % :2d-array-close)))
        (update-in ?ps1 [:local 0 :sublists] conj (:result ?ps1))
        (if (= (:tkn ?ps1) :2d-array-close)
          (as-> ?ps1 ?ps2
            (assoc ?ps2 :result (->Mzn2dArray (recall ?ps2 :sublists)))
            (eat-token ?ps2 :2d-array-close))
          (recur (eat-token ?ps1 \|)))))))

(defrecord MznArrayComp [expr tail])
;;; <array-comp> ::= "[" <expr> "|" <comp-tail> "]"
(defparse ::array-comp
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps \[)
    (parse ::expr ?ps)
    (store ?ps :expr)
    (eat-token ?ps \|)
    (parse ::comp-tail ?ps)
    (assoc ?ps :result (->MznArrayComp (recall ?ps :expr) (:result ?ps)))
    (eat-token ?ps \])))

;;; <array-access-tail> ::= "[" <expr> "," ... "]"
(defparse ::array-access-tail
  [pstate]
  (parse-list pstate \[ \]))

(defrecord MznAnnLiteral [ident exprs])
;;; <ann-literal> ::= <ident> [ "(" <expr> "," ... ")" ]
(defparse ::ann-literal
  [pstate]
  (as-> pstate ?ps
    (parse ::ident ?ps)
    (store ?ps :id)
    (if (= \( (:tkn ?ps))
      (as-> ?ps ?ps1
        (parse-list ?ps1 \( \))
        (assoc ?ps1 :result (->MznAnnLiteral (recall ?ps1 :id) (:result ?ps1))))
      (assoc ?ps :result (->MznAnnLiteral (recall ?ps :id) [])))))

(defrecord MznElseIf [then else])
(defrecord MznIfExpr [condition then else elseif])
;;; <if-then-else-expr> ::= "if" <expr> "then" <expr> ("elseif" <expr> "then" <expr>)* "else" <expr> "endif"
(defparse ::if-then-else-expr
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :if)
    (parse ::expr ?ps)
    (store ?ps :condition)
    (eat-token ?ps :then)
    (parse ::expr ?ps)
    (assoc-in ?ps [:local 0 :elifs] [])
    (store ?ps :then)
    (if (= :elseif (:tkn ?ps))
      (loop [ps ?ps]
        (as-> ?ps ?ps1
          (eat-token ?ps1 :elseif)
          (parse ::expr ?ps1)
          (store ?ps1 :elif-cond)
          (eat-token ?ps1 :then)
          (parse ::expr ?ps1)
          (update-in ?ps1 [:local 0 :elifs] conj (->MznElseIf (recall ?ps1 :elif-cond) (:result ?ps)))
          (if (not= :elseif (:tkn ?ps1))
            ?ps1
            (recur ?ps1))))
      (eat-token ?ps :else))
    (parse ::expr ?ps)
    (assoc ?ps :result (->MznIfExpr (recall ?ps :condition)
                                    (recall ?ps :then)
                                    (:result ?ps)
                                    (recall ?ps :elseif)))
    (eat-token ?ps :endif)))

(s/def ::call-expr
  (s/keys :req-un [::op ::args]))

;;; This isn't picked up for e.g. noattack(i, j, queens[i], queens[j]). I suppose noattack is a 
(defrecord MznCallExpr [op args])
;;; <call-expr> ::= <ident-or-quoted-op> [ "(" <expr>, ... ")" ]
;;; This is called for e.g. constraint alldifferent(foo); :alldifferent is a builtin-op
;;; POD So I'm doing it different!
(defparse ::call-expr
  [pstate]
  (as-> pstate ?ps
    (store ?ps :op :tkn)
    (eat-token ?ps #(or (builtin-op %) (instance? MznId %)))
    (if (= \( (:tkn ?ps))
      (as-> ?ps ?ps1
        (parse-list ?ps1 \( \))
        (assoc ?ps1 :result (->MznCallExpr (recall ?ps1 :op) (:result ?ps1))))
      (assoc ?ps :result (->MznCallExpr (recall ?ps :op) nil)))))

(defrecord MznLetExpr [items expr])
;;; <let-expr> ::= "let" { <let-item>; ... } "in" <expr>
(defparse ::let-expr
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :let)
    (parse-list-terminated ?ps #(= :in %) ::let-item)
    (store ?ps :items)
    (eat-token ?ps :in)
    (parse ::expr ?ps)
    (assoc ?ps :result (->MznLetExpr (recall ?ps :items) (:result ?ps)))))
 
;;; <let-item> ::= <var-decl-item> | <constraint-item>
(defparse ::let-item
  [pstate]
  (cond (= :constraint (:tkn pstate)) (parse ::constraint-item pstate)
        (var-decl? pstate) (parse ::var-decl-item pstate)
        :else (assoc pstate :error {:expected "let-item" :got (:tkn pstate) :line (:line pstate)})))

(defrecord MznCompTail [generators where-expr])
;;; <comp-tail> ::= <generator>, ... [ "where" <expr>]
(defparse ::comp-tail
  [pstate]
  (as-> pstate ?ps
    (parse-list-terminated ?ps #(#{:where \) \]} %) ::generator)
    (store ?ps :generators)
    (if (= :where (:tkn ?ps))
      (as-> ?ps ?ps1
        (eat-token ?ps1 :where)
        (parse ::expr ?ps1))
      (assoc ?ps :result nil))
    (assoc ?ps :result (->MznCompTail (recall ?ps :generators) (:result ?ps)))))

(defrecord MznGenerator [ids expr])
;;; <generator> ::= <ident> "," ... "in" <expr>
(s/def ::generator
  #(instance? MznGenerator %))

(defparse ::generator
  [pstate]
  (as-> pstate ?ps
    (parse-list-terminated ?ps  #(= :in %) ::ident)
    (store ?ps :ids)
    (eat-token ?ps :in)
    (parse ::expr ?ps)
    (assoc ?ps :result (->MznGenerator (recall ?ps :ids) (:result ?ps)))))

;;; <gen-call-expr> ::= <ident-or-quoted-op> "(" <comp-tail> ")" "(" <expr> ")"

;;; See https://www.minizinc.org/doc-2.2.0/en/spec.html#spec-generator-call-expressions
;;; I am improvizing since I don't understand the spec at this point!
;;;
;;; forall(i,j in Domain where i<j) (noattack(i, j, queens[i], queens[j]));
;;; https://www.minizinc.org/doc-2.2.0/en/spec.html#spec-generator-call-expressions

;;; 2019-01-21: gen-call-expr must also include things like:
;;; "sum (w in Workers) (cost[w,doesTask[w]])"  (See pg 23 of the tutorial). 
(defrecord MznGenCallExpr [gen-call-op generator expr])

(s/def ::gen-call-expr
  #(instance? MznGenCallExpr %))

(defparse ::gen-call-expr
  [pstate]
  (as-> pstate ?ps
    (store ?ps :gen-call-op :tkn)
    (eat-token ?ps builtin-gen-call-fn) ; 'sum', 'forall' etc. 
    (eat-token ?ps \()
    (parse ::comp-tail ?ps)             ; '(w in worker), '(i,j in Domain where i<j)', etc. 
    (store ?ps :comp-tail)
    (eat-token ?ps \))
    (eat-token ?ps \()
    (parse ::expr ?ps)                  ; any expr. 
    (store ?ps :expr)    
    (eat-token ?ps \))
    (assoc ?ps :result
           (->MznGenCallExpr
            (recall ?ps :gen-call-op)
            (recall ?ps :comp-tail)
            (recall ?ps :expr)))))
