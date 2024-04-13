(ns mznp.parse
  "Parse MiniZinc to records."
  (:refer-clojure :exclude [slurp])
  (:require
   [clojure.edn        :as edn]
   [clojure.pprint :refer [cl-format]]
   [clojure.string     :as str]
   [clojure.set        :as sets]
   [clojure.alpha.spec :as s]
   [mznp.utils  :refer [debugging?] :as util]
   [mznp.macros :refer-macros [defparse store recall] :refer [defparse store recall]]))

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

;;; Returns pstate: parse, eat-token, store, recall
;;; Returns something else: look, find-token

;;; The grammar implemented is 2.2.0. <Footnote 1>
;;; The grammar is here: https://www.minizinc.org/doc-2.2.0/en/spec.html
;;; The grammar is here: https://www.minizinc.org/doc-2.2.0/en/spec.html#spec-grammar

;;; Footnote 1: Though I might have grabbed productions from an earlier version of MiniZinc,
;;;             I am updating to 2.2.0 wherever I find discrepancies.

;;; ============ Tokenizer ===============================================================
;;; POD All this lexer stuff could be integrated with the :builtin stuff of the parser!

(def mzn-keywords-basic
  #{"ann", "annotation", "any", "array", "bool", "case", "constraint", "else",
    "elseif", "endif", "enum", "false", "float", "function", "if", "in", "include", "int",
    "let", "list", "maximize", "minimize", "not", "of", "op", "opt", "output", ; website shows "opt"
    "par", "predicate", "record", "satisfy", "set", "solve", "string",
    "test", "then", "true", "tuple", "type", "var", "where", "trace", "trace_stdout"})

;;; builtins.arithmetic -- https://github.com/MiniZinc/libminizinc/blob/master/share/minizinc/std/builtins.mzn
;;; You get more than this if you run the function get-operators in mznp_test.clj.
(def mzn-keywords-arithmetic ; POD
  #{"abs" "acos" "acosh" "arg_max" "arg_min" "array1d" "array2set" "array_intersect" "array_union" "asin" "asinh"
    "assert" "atan" "atanh" "bool2int" "card" "ceil" "cos" "cosh" "diff" "div" "div_mt" "div_t" "element" "element_mt"
    "element_t" "enum_next" "enum_prev" "exp" "fldiv_mt" "fldiv_t" "floor" "int2float" "intersect" "lin_exp" "ln"
    "log10" "log2" "max" "max_t" "min" "min_t" "mod" "mod_mt" "mod_t" "pow" "product" "product_rec" "sin" "sinh"
    "sqrt" "sqrt_t" "subset" "sum" "superset" "symdiff" "tan" "tanh" "to_enum" "union" "xor"})

;;; global constraints  --  https://github.com/MiniZinc/libminizinc/tree/master/share/minizinc/std
(def mzn-keywords-global-constraint
  #{"all_different" "all_disjoint" "all_equal" "nvalue"
    "symmetric_all_different" "lex_greater" "lex_greatereq" "lex_less" "lex_lesseq"
    "seq_precede_chain" "strict_lex2" "value_precede" "value_precede_chain" "arg_sort" "decreasing"
    "increasing" "sort" "int_set_channel" "inverse" "inverse_set" "link_set_to_booleans"
    "among" "at_least" "at_most" "at_most1" "count" "count_eq" "count_geq" "count_gt"
    "count_leq" "count_lt" "count_neq" "distribute" "exactly" "global_cardinality"
    "global_cardinality_closed" "global_cardinality_low_up" "global_cardinality_low_up_closed"
    "bin_packing" "bin_packing_capa" "bin_packing_load" "diffn" "diffn_nonstrict"
    "diffn_nonstrict_k" "geost" "geost_bb" "geost_nonoverlap_k" "geost_smallest_bb" "knapsack"
    "alternative" "cumulative" "disjunctive" "disjunctive_strict" "span" "bounded_dpath"
    "bounded_path" "connected" "d_weighted_spanning_tree" "dag" "dconnected" "dpath"
    "dreachable" "dsteiner" "dtree" "path" "reachable" "steiner" "subgraph" "tree"
    "weighted_spanning_tree" "cost_mdd" "cost_regular" "mdd" "regular" "regular_nfa"
    "table"})

;; builtins.logic
(def mzn-keywords-logic
  #{"forall", "exists", "xorall", "clause", "iffall"})

;;; POD someday check for no overlaps.
;;; Don't put global constraints here, "tree", "knapsack" etc. make nice variables!
(def mzn-keywords
  (sets/union mzn-keywords-basic mzn-keywords-arithmetic mzn-keywords-logic))

(def ^:private mzn-long-syntactic ; chars that COULD start a multi-character syntactic elements.
  #{\., \,, \\, \/, \<, \>, \=, \!, \+, \|, \[, \], \: \-})

(def ^:private mzn-syntactic ; chars that are valid tokens in themselves.
  #{\[, \], \(, \), \{, \}, \=, \^, \,, \:, \;, \|, \*, \+, \/, \-, \<, \>}) ; not \_

;;; POD multi-line comment (e.g. /* ... */ would go in here, sort of.
(defn read-long-syntactic [st ws]
  (let [len (count st)
        c0  (nth st 0)
        c1  (and (> len 1) (nth st 1))
        c2  (and (> len 2) (nth st 2))]
    (when-let [result (cond (and (= c0 \.) (= c1 \.)) {:raw ".." :tkn :range-op}
                            (and (= c0 \-) (= c1 \>)) {:raw "->" :tkn :->-op}
                            (and (= c0 \<) (= c1 \-)) {:raw "<-" :tkn :<--op}
                            (and (= c0 \\) (= c1 \/)) {:raw "\\/" :tkn :or-op}
                            (and (= c0 \/) (= c1 \\)) {:raw "/\\" :tkn :and-op}
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
(defrecord MznEOLcomment [text line col])
(defrecord MznId [name])
(defrecord MznString [str])
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
           {:ws ws :raw num :tkn (edn/read-string num)}),                   ; number
         (when-let [[_ id] (re-matches #"(?s)('[^']*').*" s)]           ; identifer type 2 POD Need's work.
           {:ws ws :raw id :tkn (->MznId id)})
         (when-let [[_ st] (re-matches #"(?s)(\"[^\"]*\").*" s)]        ; string literal
           {:ws ws :raw st :tkn (->MznString (edn/read-string st))})
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

(defn token-vec [pstate] (mapv :tkn (:tokens pstate)))

(def balanced-map "What balances with the opening syntax?" { \{ \}, \( \), \[ \] :2d-array-open :2d-array-close})
(def balanced-inv (sets/map-invert balanced-map))

(defn find-token
  "Return position if tkn is found within the item (before semicolon)."
  [tvec tkn]
  (when (not-empty tvec)
    (let [pos-semi (util/index-of-elem tvec \;)
          pos-semi (if (pos? pos-semi) pos-semi (count tvec)) ; In testing, might not have full item; not semi.
          pos-tkn  (util/index-of-elem tvec tkn)]
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

(defn parse-dispatch [tag & _]
  tag)

(defmulti parse #'parse-dispatch)

;;;================== parse-token (parsing of some atomic things) ======================================================
;;; <ti-variable-expr-tail> ::= $[A-Za-z][A-Za-z0-9_]*
;;; <base-type> ::= "bool" | "int" | "float" | "string"
;;; <builtin-num-un-op> ::= + | -
;;; <builtin-un-op> ::= not | <builtin-num-un-op>
;;; <builtin-bin-op> ::= <-> | -> | <- | \/ | xor | /\ | < | > | <= | >= | == | = | != | in |
;;;                      subset | superset | union | diff | symdiff | .. | intersect| ++ | <builtin-num-bin-op>
;;; <builtin-num-bin-op> ::= + | - | * | / | div | mod
;;; <num-bin-op> ::= <builtin-num-bin-op> | ‘<ident>‘
(def builtin-num-un-op #{\+, \-})
(def builtin-un-op (conj builtin-num-un-op :not))
(def builtin-num-bin-op #{\+ \- \* \/ :div :mod})
(def builtin-bin-op (into #{:<->-op  :->-op  :<-op  :or-op :xor-op :and-op \< \> :<= :>= :== \= :not= :in,
                            :subset, :superset, :union, :diff, :symdiff, :range-op,  :intersect, :++-op}
                          builtin-num-bin-op))

(def parse-token-map
  "A map of parse tags and associated test for tokens of that tag type."
    {:mznp/bool-literal          #{:false :true}
     :mznp/int-literal           #(integer? %)
     :mznp/float-literal         #(float? %)
     :mznp/string-literal        #(= (type %) MznString)
     #_#_:mznp/ti-variable-expr-tail #(= (type %) MznTypeInstVar)
     :mznp/base-type             #{:bool :int :float :string}
     :mznp/builtin-num-un-op     builtin-num-un-op     ; Not yet used for parse-token; the var is used.
     :mznp/builtin-un-op         builtin-un-op         ; Not yet used for parse-token; the var is used.
     :mznp/builtin-num-bin-op    builtin-num-bin-op    ; Not yet used for parse-token; the var is used.
     :mznp/num-bin-op            #(or (= (type %) MznId) (builtin-num-bin-op %))
     :mznp/builtin-bin-op        builtin-bin-op})      ; Not yet used for parse-token; the var is used.

;;; This method is for all the parse-tags in parse-token-map above.
(defmethod parse :default
  [tag pstate]
  (if-let [test (get parse-token-map tag)]
    (-> pstate
        (assoc :result (:tkn pstate))
        (eat-token test))
    (throw (ex-info "No method for parse tag" {:tag tag}))))
;;;--------- End parse-token stuff -----------------------------------------------------------------------------------

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
  ([str] (parse-string :mznp/model str))
  ([tag str]
    (let [pstate (->> str tokenize make-pstate (parse tag))]
      (if (not= (:tkn pstate) :eof)
        (do (when @debugging? (println "\n\nPARSING ENDED PREMATURELY.\n" pstate))
            (assoc pstate :error {:reason "Parsing ended prematurely."}))
        pstate))))

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
  ([pstate char-open char-close char-sep]
   (parse-list pstate char-open char-close char-sep :mznp/expr))
  ([pstate char-open char-close char-sep parse-tag]
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
             (recur (cond-> ?ps1 (= char-sep (:tkn ?ps1)) (eat-token char-sep))))))))))

(defn parse-list-terminated
  "Does parse parametrically for '[ <item> ','... ] <terminator>'. Does not eat terminator."
  [pstate & {:keys [term-fn sep-fn parse-tag] :or {sep-fn #(= \, %)
                                                   term-fn #(= \] %)
                                                   parse-tag :mznp/expr}}]
   (as-> pstate ?ps
     (assoc-in ?ps [:local 0 :items] [])
     (loop [ps ?ps]
       (cond
         (= :eof (:tkn ps))
         (assoc ps :error {:while "parsing a list" :parse-tag parse-tag :line (:line ps)}),
         (term-fn (:tkn ps))
         (assoc ps :result (recall ps :items)),
         ;(and last-sep? (sep-fn (:tkn ps)) (term-fn
         :else
         (as-> ps ?ps
           (parse parse-tag ?ps)
           (update-in ?ps [:local 0 :items] conj (:result ?ps))
           (if (contains? ?ps :error)
             ?ps
             (recur (cond-> ?ps (sep-fn (:tkn ?ps)) (eat-token #{\,\;})))))))))

;;; See utils.cljc for the following.
;;; <num-bin-op> ::= <builtin-num-bin-op> | ‘<ident>‘
;;; <base-type> ::= "bool" | "int" | "float" | "string"
;;; <builtin-un-op> ::= not | <builtin-num-un-op>
;;; <builtin-num-un-op> ::= + | -
;;; <builtin-bin-op> ::= <-> | -> | <- | \/ | xor | /\ | < | > | <= | >= | == | = | != | in |
;;;                      subset | superset | union | diff | symdiff | .. | intersect| ++ | <builtin-num-bin-op>
;;; <builtin-num-bin-op> ::= + | - | * | / | div | mod

;;;-------------------Library Builtins: These are used 'bare', without parse-token ------
(def builtin-arithmetic-op (set (->> mzn-keywords-arithmetic (map keyword))))

;;; POD This is not complete!
(def builtin-quantifier (set (->> mzn-keywords-logic (map keyword))))

(def builtin-constraint (set (->> mzn-keywords-global-constraint (map keyword))))

(def builtin-agg-fn #{:sum :product :min :max}) ; I'm guessing see page 23 of Tutorial

(def builtin-gen-call-fn (sets/union builtin-agg-fn builtin-quantifier))

(def builtin-op (sets/union builtin-bin-op builtin-un-op builtin-arithmetic-op  builtin-quantifier builtin-constraint))
;;;--------------------End Library Builtins ------------------------------------
(s/def ::model (s/keys :req-un [::items]))

;;; <model> ::= [ <item> ; ... ]
(defrecord MznModel [items])
(defparse :mznp/model ; top-level grammar element.
  [pstate]
  (loop [ps (assoc pstate :model (->MznModel []))]
    (if (= :eof (:tkn ps))
      (-> ps
          (assoc :result (:model ps))
          (dissoc :model))
      (recur
       (as-> ps ?ps
         (parse :mznp/item ?ps)
         (update-in ?ps [:model :items] conj (:result ?ps))
         (eat-token ?ps \;))))))

(defn var-decl?
  "Returns true if head looks like it can start a var-decl"
  [pstate]
  (let [tkn (:tkn pstate)]
    (or (#{:bool :int :float :string :var :par} tkn) ; base-ti-expr
        (instance? MznTypeInstVar tkn)          ; base-ti-expr-tail
        (#{:set :array :list} tkn)                   ; set-ti-expr-tail, :array-ti-expr-tail
        (= \{ tkn)                                   ;
        (#{:ann :opt} tkn)                           ; others
        (instance? MznTypeInstVar tkn)               ; base-ti-expr-tail ... <ti-variable-expr-tail>
        (find-token (token-vec pstate) :range-op)))) ; base-ti-expr-tail ... <num-expr> ".."  <num-expr>

;;; <item> ::= <include-item> | <var-decl-item> | <assign-item> | <constraint-item> | <solve-item> |
;;;            <output-item> | <predicate-item> | <test-item> | <function-item> | <annotation-item>
(defparse :mznp/item
  [pstate]
  (let [tkn  (:tkn pstate)
        tkn2 (look pstate 1)]
    (cond (= tkn :include)                (parse :mznp/include-item pstate),
          (= tkn :constraint)             (parse :mznp/constraint-item pstate),
          (= tkn :solve)                  (parse :mznp/solve-item pstate),
          (= tkn :output)                 (parse :mznp/output-item pstate),
          (= tkn :predicate)              (parse :mznp/predicate-item pstate),
          (= tkn :test)                   (parse :mznp/test-item pstate),
          (= tkn :function)               (parse :mznp/function-item pstate),
          (= tkn :ann)                    (parse :mznp/annotation-item pstate) ; I don't think "annotation" is a keyword.
          (= tkn :enum)                   (parse :mznp/enum-item pstate)
          (and (instance? MznId tkn) (= tkn2 \=)) (parse :mznp/assign-item pstate),
          (var-decl? pstate)              (parse :mznp/var-decl-item pstate),
          :else (assoc pstate :error {:expected "a MZn item" :got (:tkn pstate) :in :item :line (:line pstate)}))))

;;; 4.1.6 Each type has one or more possible instantiations. The instantiation of a variable or value indicates
;;;       if it is fixed to a known value or not. A pairing of a type and instantiation is called a type-inst.
;;;       Reading further the "instantiation" indicates "how fixed or unfixed" its value is.
;;;       "instance-time" as opposed to "run-time" is the model as defined in .mzn.

;;; <type-inst-syn-item> ::= type <ident> <annotations>= <ti-expr>
;;;; POD I don't see where this is used

;;; <ti-expr-and-id> ::= <ti-expr> ":" <ident>
(defrecord MznIdDef [id id-type])
(defparse :mznp/ti-expr-and-id
  [pstate]
  (as-> pstate ?ps
    (parse :mznp/ti-expr ?ps)
    (store ?ps :type)
    (eat-token ?ps \:)
    (parse :mznp/ident ?ps)
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

(defparse :mznp/include-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :include)
    (parse :mznp/string-literal ?ps)
    (store ?ps :model-part)
    (assoc ?ps :result (->MznInclude (recall ?ps :model-part)))))

(defrecord MznEnum [name cases])
(defparse :mznp/enum-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :enum)
    (parse :mznp/ident ?ps)
    (store ?ps :name)
    (parse :mznp/annotations ?ps)
    (if (= (:tkn ?ps) \=)
      (as-> ?ps ?ps1
        (eat-token ?ps1)
        (parse :mznp/enum-cases ?ps1)
        (store ?ps1 :cases))
      (store ?ps :cases nil))
    (assoc ?ps :result (->MznEnum (recall ?ps :name) (recall ?ps :cases)))))

(defparse :mznp/enum-cases
  [pstate]
  (parse-list pstate \{ \} \,))

;;; <assign-item> ::= <ident> = <expr>
(defrecord MznAssignment [lhs rhs])
(defparse :mznp/assign-item
  [pstate]
  (as-> pstate ?ps
    (parse :mznp/ident ?ps)
    (store ?ps :lhs)
    (eat-token ?ps \=)
    (parse :mznp/expr ?ps)
    (assoc ?ps :result (->MznAssignment (recall ?ps :lhs) (:result ?ps)))))

(defrecord MznConstraint [expr])
;;; <constraint-item> ::= constraint <expr>
(s/def ::constraint-item #(instance? MznConstraint %))

(defparse :mznp/constraint-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :constraint)
    (parse :mznp/expr ?ps)
    (assoc ?ps :result (->MznConstraint (:result ?ps)))))

(defrecord MznSolve [action expr anns])
;;; <solve-item> ::= solve <annotations> satisfy | solve <annotations> minimize <expr> | solve <annotations> maximize <expr>
(s/def ::solve-item #(instance? MznSolve %))

(defparse :mznp/solve-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :solve)
    (parse :mznp/annotations ?ps)
    (store ?ps :anns)
    (store ?ps :action :tkn) ; Stores value of :tkn, not :result.
    (eat-token ?ps #{:satisfy :minimize :maximize})
    (if (= :satisfy (recall ?ps :action))
      (assoc ?ps :result nil)
      (parse :mznp/expr ?ps))
    (assoc ?ps :result (->MznSolve (recall ?ps :action)
                                   (:result ?ps)
                                   (recall ?ps :anns)))))

(defn search-var?
  "Search through a (potentially deep) var-decl-item.lhs looking for whether it is a var."
  [x]
  (cond (not (map? x)) nil
        (:var? x) true
        :else (some identity (map search-var? (vals x)))))

(defrecord MznVarDecl [lhs rhs anns var?])

;;; <var-decl-item> ::= <ti-expr-and-id> <annotations> [ "=" <expr> ]
(defparse :mznp/var-decl-item
  [pstate]
  (as-> pstate ?ps
    (parse :mznp/ti-expr-and-id ?ps)
    (store ?ps :lhs)
    (parse :mznp/annotations ?ps)
    (store ?ps :anns)
    (if (= (:tkn ?ps) \=)
      (as-> ?ps ?ps1
          (eat-token ?ps1)
          (parse :mznp/expr ?ps1)
          (store ?ps1 :init))
      (store ?ps :init nil))
    (assoc ?ps :result (->MznVarDecl (recall ?ps :lhs)
                                     (recall ?ps :init)
                                     (recall ?ps :anns)
                                     (-> (recall ?ps :lhs) search-var?)))))

(defrecord MznOutputItem [expr])
(s/def ::output-item #(instance? MznOutputItem %))

;;; <output-item> ::= output <expr>
(defparse :mznp/output-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :output)
    (parse :mznp/expr ?ps)
    (assoc ?ps :result (->MznOutputItem (:result ?ps)))))

(defrecord MznAnnItem [id params])
;;; <annotation-item> ::= annotation <ident> <params>
(defparse :mznp/annotation-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :annotation)
    (parse :mznp/ident ?ps)
    (store ?ps :id)
    (parse :mznp/params ?ps)
    (assoc ?ps :result (->MznAnnItem (recall ?ps :id) (:result ?ps)))))

;;; <annotations> ::= [ "::" <annotation> ]*
(defparse :mznp/annotations
  [pstate]
  (if (= (:tkn pstate) :ann-sep)
    (as-> pstate ?ps
      (assoc-in ?ps [:local 0 :anns] [])
      (loop [ps ?ps]
        (as-> ps ?ps1
          (eat-token ?ps1 :ann-sep)
          (parse :mznp/annotation ?ps1)
          (update-in ?ps1 [:local 0 :anns] conj (:result ?ps1))
          (if (not= (:tkn ?ps1) :ann-sep)
            (assoc ?ps1 :result (recall ?ps1 :anns))
            (recur ?ps1)))))
    (assoc pstate :result nil)))

(defrecord MznAnnotation [head tail])
;;; <annotation> ::= <expr-atom-head> <expr-atom-tail>
(defparse :mznp/annotation
  [pstate]
  (as-> pstate ?ps
    (parse :mznp/expr-atom-head ?ps)
    (store ?ps :head)
    (parse :mznp/expr-atom-tail ?ps)
    (assoc ?ps :result (->MznAnnotation (recall ?ps :head) (:result ?ps)))))

(defrecord MznPredItem [pred])
;;; <predicate-item> ::= predicate <operation-item-tail>
(defparse :mznp/predicate-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :predicate)
    (parse :mznp/operation-item-tail ?ps)
    (assoc ?ps :result (->MznPredItem (:result ?ps)))))

(defrecord MznTestItem [pred])
;;; <test-item> ::= test <operation-item-tail>
(defparse :mznp/test-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :test)
    (parse :mznp/operation-item-tail ?ps)
    (assoc ?ps :result (->MznTestItem (:result ?ps)))))

(defrecord MznFnItem [expr op])
;;; <function-item> ::= function <ti-expr> ":" <operation-item-tail>
(defparse :mznp/function-item
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :function)
    (parse :mznp/ti-expr ?ps)
    (store ?ps :expr (:result ?ps))
    (eat-token ?ps \:)
    (parse :mznp/operation-item-tail ?ps)
    (assoc ?ps :result (->MznFnItem (recall ?ps :expr)(:result ?ps)))))

(defrecord MznOpItemTail [id params expr ann])
;;; <operation-item-tail> ::= <ident> <params> <annotations> [ "=" <expr>]
(defparse :mznp/operation-item-tail
  [pstate]
  (as-> pstate ?ps
    (parse :mznp/ident ?ps)
    (store ?ps :id :tkn)
    (parse :mznp/params ?ps)
    (store ?ps :params)
    (parse :mznp/annotations ?ps)
    (store ?ps :anns)
    (if (= (:tkn ?ps) \=)
      (as-> ?ps ?ps1
          (eat-token ?ps1)
          (parse :mznp/expr ?ps1)
          (store ?ps1 :expr))
      (store ?ps :expr nil))
    (assoc ?ps :result (->MznOpItemTail (recall ?ps :id)
                                        (recall ?ps :params)
                                        (recall ?ps :expr)
                                        (recall ?ps :anns)))))

;;; Example: function var T: enum_next(set of T: X, var T: x);
;;; The whole thing is optional, but functions start with "function".
;;; <params> ::= [ "(" <ti-expr-and-id>, ... ")" ]
(defparse :mznp/params
  [pstate]
  (if (= \( (:tkn pstate))
    (parse-list pstate \( \) \, :mznp/ti-expr-and-id)
    (assoc pstate :result [])))

;;; ;;; B.2. Type-Inst Expressions

;;; <ti-expr> ::= <base-ti-expr>
(defparse :mznp/ti-expr
  [pstate]
  (parse :mznp/base-ti-expr pstate))

(defrecord MznTypeInstExpr [var? par? expr])
;;; <base-ti-expr> ::= <var-par> <base-ti-expr-tail>
;;; <var-par> ::= var | par | ε
(defparse :mznp/base-ti-expr
  [pstate]
  (let [var-par? (:tkn pstate)]
    (as-> pstate ?ps
      (cond-> ?ps (#{:var :par} var-par?) (eat-token))
      (parse :mznp/base-ti-expr-tail ?ps)
      (assoc ?ps :result (map->MznTypeInstExpr {:expr (:result ?ps)}))
      (cond-> ?ps (= :var var-par?) (assoc-in [:result :var?] true))
      (cond-> ?ps (= :par var-par?) (assoc-in [:result :par?] true)))))

(defrecord MznIntegerRange [from to])
(defrecord MznSetLiterals  [elems])
;;; <base-ti-expr-tail> ::= <ident> | <base-type> | <set-ti-expr-tail> |
;;;                          <array-ti-expr-tail> | ann | opt <base-ti-expr-tail> | "{" <expr>, ... "}" |
;;;                          <num-expr> .. <num-expr>
(defparse :mznp/base-ti-expr-tail
  [pstate]
  (let [tkn (:tkn pstate)]
    (cond (instance? MznId tkn)              ; <ident>
          (parse :mznp/ident pstate)
          (#{:bool :int :float :string} tkn) ; <base-type}
          (parse :mznp/base-type pstate),
          (= :set tkn)                       ; <set-ti-exp-tail>
          (parse :mznp/set-ti-expr-tail pstate),
          (instance? MznTypeInstVar tkn)     ; <ti-variable-expr-tail>
          (-> pstate
              (assoc :result tkn)
              eat-token),
          (#{:array :list} tkn)
          (parse :mznp/array-ti-expr-tail pstate),
          (= tkn :ann)                       ; ann
          (assoc pstate :error {:msg "Annotations NYI." :line (:line pstate)}),
          (#{:opt :op} tkn)                  ; opt <base-ti-expr-tail>
          (as-> pstate ?ps
              (eat-token ?ps)
              (parse :mznp/base-ti-expr-tail ?ps)
              (assoc-in ?ps [:result :optional?] true)),
          (= tkn \{)                        ; "{" <expr>, ... "}"
          (parse :mznp/set-literal pstate),
          (find-token (token-vec pstate) :range-op)        ; <num-expr> ".."  <num-expr> ; call it a range expression
          (parse :mznp/num-expr pstate)
          :else
          (assoc pstate :error {:expected :base-ti-expr-tail :tkn (:tkn pstate)
                                :line (:line pstate) :col (:col pstate)}))))

(defrecord MznSetType [base-type optional?])
(s/def ::set-ti-expr-tail (s/keys :req-un [::base-type ::optional?]))

;;; <set-ti-expr-tail> ::= set of <ti-expr>  POD It said <base-type> here, not <ti-expr> See knapsack.mzn
(defparse :mznp/set-ti-expr-tail
  [pstate]
  (as-> pstate ?ps
      (eat-token ?ps :set)
      (eat-token ?ps :of)
      (parse :mznp/ti-expr ?ps)
      (assoc ?ps :result (->MznSetType (:result ?ps) nil))))

(defrecord MznArrayType [index base-type optional?])
(defrecord MznListType [base-type])
;;; <array-ti-expr-tail> ::= array "[" <ti-expr>, ... "]" of <ti-expr> | list of <ti-expr> ; POD added " around "[".
(defparse :mznp/array-ti-expr-tail
  [pstate]
  (cond (= (:tkn pstate) :array)
        (as-> pstate ?ps
          (eat-token ?ps :array)
          (parse-list ?ps \[ \] \, :mznp/ti-expr)
          (store ?ps :ti-list)
          (eat-token ?ps :of)
          (parse :mznp/ti-expr ?ps)
          (assoc ?ps :result (->MznArrayType (recall ?ps :ti-list) (:result ?ps) nil)))
        (= (:tkn pstate) :list)
        (as-> pstate ?ps
          (eat-token ?ps :list)
          (eat-token ?ps :of)
          (parse :mznp/ti-expr ?ps)
          (assoc ?ps :result (->MznListType (:result ?ps))))
        :else
        (assoc pstate :error {:expected "array or list"
                              :got (:tkn pstate)
                              :line (:line pstate)
                              :in :array-ti-expr-tail})))

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
(defparse :mznp/expr
  [pstate]
  (as-> pstate ?ps
    (parse :mznp/expr-atom ?ps)
    (store ?ps :atom)
    (if (or (builtin-bin-op (:tkn ?ps))
            (quoted-id? pstate))
      (as-> ?ps ?ps1
        (parse :mznp/expr-binop-tail ?ps1)
        (store ?ps1 :tail))
      (assoc-in ?ps [:local 0 :tail] nil))
    (assoc ?ps :result (->MznExpr (recall ?ps :atom) (recall ?ps :tail)))))

(defrecord MznExprAtom [head tail ann])
;;; <expr-atom> ::= <expr-atom-head> <expr-atom-tail> <annotations>
(defparse :mznp/expr-atom
  [pstate]
  (as-> pstate ?ps
    (parse :mznp/expr-atom-head ?ps)
    (store ?ps :head)
    (parse :mznp/expr-atom-tail ?ps)
    (store ?ps :tail)
    (parse :mznp/annotations ?ps)
    (assoc ?ps :result (->MznExprAtom
                        (recall ?ps :head)
                        (recall ?ps :tail)
                        (:result ?ps)))))

(defrecord MznArrayAccess [exprs])
;;;  <expr-atom-tail> ::= ε | <array-access-tail> <expr-atom-tail>
(defparse :mznp/expr-atom-tail
  [pstate]
  (if (= \[ (:tkn pstate))
    (as-> pstate ?ps
      (parse :mznp/array-access-tail ?ps)
      (assoc ?ps :result (->MznArrayAccess (:result ?ps))))
    (assoc pstate :result nil)))

;;; <array-access-tail> ::= "[" <expr> "," ... "]"
(defparse :mznp/array-access-tail
  [pstate]
  (parse-list pstate \[ \] \,))

(defrecord MznExprBinopTail [bin-op expr])
;;; POD I think the grammar is botched here. The [ ] should be BNF optional, not terminals!
;;; <expr-binop-tail> ::= "[" <bin-op> <expr> "]"
(defparse :mznp/expr-binop-tail
  [pstate]
  (as-> pstate ?ps
    (parse :mznp/bin-op ?ps)
    (store ?ps :bin-op)
    (parse :mznp/expr ?ps)
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
;;;  <expr-atom-head> ::= <builtin-un-op> <expr-atom> | ( <expr> ) | <ident-or-quoted-op> |
;;;                       _ | <bool-literal> | <int-literal> | <float-literal> | <string-literal> |
;;;                       <set-literal> | <set-comp> | <array-literal> | <array-literal-2d> |
;;;                       <array-comp> | <ann-literal> | <if-then-else-expr> | <let-expr> | <call-expr> |
;;;                       <gen-call-expr>
(declare part1 part2 part3)
(defparse :mznp/expr-atom-head
  [pstate]
  (let [tkn (:tkn pstate)]
    (or (part1 pstate tkn)     ; through <string-literal>
        (part2 pstate tkn))))  ; through end

(defn part1 [pstate tkn]
  (cond (builtin-un-op tkn)                     ; <builtin-un-op> <expr-atom>
        (as-> pstate ?ps
          (eat-token ?ps)
          (parse :mznp/expr-atom ?ps)
          (assoc ?ps :result (->MznExprUnOp tkn (:result ?ps)))),
        (= \( tkn)                              ; ( <expr> )
        (as-> pstate ?ps
          (eat-token ?ps)
          (parse :mznp/expr ?ps)
          (eat-token ?ps \))
          (assoc ?ps :result (assoc (:result ?ps) :primary? true))),
        (ident-or-quoted-op? pstate)           ; <ident-or-quoted-op>
        (parse :mznp/ident-or-quoted-op pstate),
        (= \_ tkn)                             ; _
        (-> pstate
            eat-token
            (assoc :result \_)),
        (#{:false :true} tkn)                  ; bool-literal
        (parse :mznp/bool-literal pstate),
        (integer? tkn)                         ; int-literal
        (parse :mznp/int-literal pstate),
        (float? tkn)                           ; float-literal
        (parse :mznp/float-literal pstate),
        (instance? MznString tkn)              ; string-literal
        (parse :mznp/string-literal pstate)))

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
          (parse :mznp/set-literal pstate),
          (= tkn \{)                                                  ; <set-comp>
          (parse :mznp/set-comp pstate),
          (and (= tkn \[) (or (not gen-bar-pos)
                              (and close-arr-pos (< close-arr-pos gen-bar-pos))
                              balanced-arr?))                         ; <array-literal>
          (parse :mznp/array-literal pstate),
          (= tkn \[)                                                  ; <array-comp>  "[ s[i] | i in 1..n]"
          (parse :mznp/array-comp pstate),
          (= tkn :2d-array-open)                                      ; <array-literal-2d>
          (parse :mznp/array-literal-2d pstate),
          (= tkn :ann-sep) ; POD needs investigation!                 ; <ann-literal>
          (parse :mznp/ann-literal pstate),
          (= tkn :if)                                                 ; <if-then-else-expr>
          (parse :mznp/if-then-else-expr pstate),
          (= tkn :let)                                                ; <let-expr>
          (parse :mznp/let-expr pstate),
          (gen-call-expr? tvec)     ; POD I'm making this up          ; <gen-call-expr> e.g. "sum (w in Workers) (cost[w,doesTask[w]])"
          (parse :mznp/gen-call-expr pstate)                              ;                       OR forall
          (or (builtin-op tkn)                                        ; <call-expr>
              (and (instance? MznId tkn)
                   (= \( tkn2)))
          (parse :mznp/call-expr pstate)
          :else
          (assoc pstate :error {:expected "expr-atom-head (lots of stuff)"
                                :got (:tkn pstate) :line (:line pstate)}))))

(defrecord MznQuotedOp [op])
;;; 4.1.7.3. Expression Atoms suggests that they are serious about this.
;;; BTW whitespace is not allowed between the quotes. POD Currently impossible to enforce.
;;; <ident-or-quoted-op> ::= <ident> | "'" <builtin-op> "'"
(defparse :mznp/ident-or-quoted-op
  [pstate]
  (cond (instance? MznId (:tkn pstate))
        (parse :mznp/ident pstate),
        (ident-or-quoted-op? pstate)
        (as-> pstate ?ps
          (eat-token ?ps \')
          (assoc ?ps :result (->MznQuotedOp (:tkn ?ps)))
          (eat-token ?ps builtin-op)
          (eat-token ?ps \'))
        :else
        (assoc pstate :error {:expected "ident-or-quoted-op" :got (:tkn pstate) :line (:line pstate)})))

(defparse :mznp/ident
  [pstate]
  (as-> pstate ?ps
    (assoc ?ps :result (:tkn ?ps))
    (eat-token ?ps #(instance? MznId %))))

;;; <num-expr> ::= <num-expr-atom> <num-expr-binop-tail>   ; Just like <expr>
(defparse :mznp/num-expr
  [ps]
  (parse :mznp/expr ps)) ; POD Fix this.

;;; <num-expr-atom> ::= <num-expr-atom-head> <expr-atom-tail> <annotations> ; Much like <expr-atom>
(defparse :mznp/num-expr-atom
  [ps]
  (parse :mznp/expr-atom ps)) ; POD Fix this.

;;; <num-expr-binop-tail> ::= [ <num-bin-op> <num-expr>]
(defparse :mznp/num-expr-binop-tail
  [ps]
  (parse :mznp/expr-binop-tail ps)) ; POD Fix this.

;;; <num-expr-atom-head> ::= <builtin-num-un-op> <num-expr-atom> | ( <num-expr> ) | <ident-or-quoted-op> |
;;;                          <int-literal> | <float-literal> | <if-then-else-expr> | <case-expr> | <let-expr> |
;;;                          <call-expr> | <gen-call-expr>

;;; <bin-op> ::= <builtin-bin-op> | ‘<ident>‘
(defparse :mznp/bin-op
  [pstate]
  (if (builtin-bin-op (:tkn pstate))
    (as-> pstate ?ps
      (assoc ?ps :result (:tkn ?ps))
      (eat-token ?ps))
    (as-> pstate ?ps
      (eat-token ?ps \')
      (parse :mznp/ident ?ps)
      (assoc ?ps :result (->MznQuotedOp (:result ?ps)))
      (eat-token ?ps \'))))

;;; <set-literal> ::= "{" [ <expr>, ... ] "}"
(defrecord MznSetLiteral [elems])
;;; "{" <expr>, ... "}"
;;; (parse :mznp/set-literal {:tkn \{ :tokens [1 \, 2 \, 3 \}] })
(defparse :mznp/set-literal
  [pstate]
  (as-> pstate ?ps
    (parse-list ?ps \{ \} \,)
    (assoc ?ps :result (->MznSetLiteral (:result ?ps)))))

(defrecord MznSetComp [expr comp-tail])
;;; <set-comp> ::= "{" <expr> "|" <comp-tail> "}"
(defparse :mznp/set-comp
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps \{)
    (parse :mznp/expr ?ps)
    (store ?ps :expr)
    (eat-token ?ps \|)
    (parse :mznp/comp-tail ?ps)
    (eat-token ?ps \})
    (assoc ?ps :result (->MznSetComp
                        (recall ?ps :expr)
                        (:result ?ps)))))

(defrecord MznArray [elems])
;;; <array-literal> ::= "[" [ <expr>, ... ] "]"
(defparse :mznp/array-literal
  [pstate]
  (as-> pstate ?ps
    (parse-list ?ps \[ \] \,)
    (assoc ?ps :result (->MznArray (:result ?ps)))))

;;; [|10, 20, 13, |22, 11, 31, |14, 20, 18|] -- Note the extra commas!
(defrecord Mzn2dArray [sublists])
(s/def ::sublists
  (s/and (s/coll-of vector?)
         #(apply = (map count %))))
(s/def ::array-literal-2d
  (s/keys :req-un [::sublists]))

;;; <array-literal-2d> ::= "[|" [ (<expr>, ... ) "|" ... ] "|]"
(defparse :mznp/array-literal-2d
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :2d-array-open)
    (assoc-in ?ps [:local 0 :sublists] [])
    (loop [ps ?ps]
      (as-> ps ?ps1
        (parse-list-terminated ?ps1 :term-fn #(or (= % \|) (= % :2d-array-close)))
        (update-in ?ps1 [:local 0 :sublists] conj (:result ?ps1))
        (if (= (:tkn ?ps1) :2d-array-close)
          (as-> ?ps1 ?ps2
            (assoc ?ps2 :result (->Mzn2dArray (recall ?ps2 :sublists)))
            (eat-token ?ps2 :2d-array-close))
          (recur (eat-token ?ps1 \|)))))))

(defrecord MznArrayComp [expr tail])
;;; <array-comp> ::= "[" <expr> "|" <comp-tail> "]"
(defparse :mznp/array-comp
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps \[)
    (parse :mznp/expr ?ps)
    (store ?ps :expr)
    (eat-token ?ps \|)
    (parse :mznp/comp-tail ?ps)
    (assoc ?ps :result (->MznArrayComp (recall ?ps :expr) (:result ?ps)))
    (eat-token ?ps \])))

(defrecord MznAnnLiteral [ident exprs])
;;; <ann-literal> ::= <ident> [ "(" <expr> "," ... ")" ]
(defparse :mznp/ann-literal
  [pstate]
  (as-> pstate ?ps
    (parse :mznp/ident ?ps)
    (store ?ps :id)
    (if (= \( (:tkn ?ps))
      (as-> ?ps ?ps1
        (parse-list ?ps1 \( \) \,)
        (assoc ?ps1 :result (->MznAnnLiteral (recall ?ps1 :id) (:result ?ps1))))
      (assoc ?ps :result (->MznAnnLiteral (recall ?ps :id) [])))))

(defrecord MznElseIf [cond then])
(defrecord MznIfExpr [condition then elseif else])
;;; <if-then-else-expr> ::= "if" <expr> "then" <expr> ("elseif" <expr> "then" <expr>)* "else" <expr> "endif"
(defparse :mznp/if-then-else-expr
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :if)
    (parse :mznp/expr ?ps)
    (store ?ps :condition)
    (eat-token ?ps :then)
    (parse :mznp/expr ?ps)
    (assoc-in ?ps [:local 0 :elseifs] [])
    (store ?ps :then)
    (if (= :elseif (:tkn ?ps))
      (loop [x ?ps]
        (as-> x ?ps1
          (eat-token ?ps1 :elseif)
          (parse :mznp/expr ?ps1)       ; cond
          (store ?ps1 :elseif-cond)
          (eat-token ?ps1 :then)
          (parse :mznp/expr ?ps1)       ; then
          (update-in ?ps1 [:local 0 :elseifs] conj (->MznElseIf (recall ?ps1 :elseif-cond) (:result ?ps1)))
          (if (= :elseif (:tkn ?ps1)) (recur ?ps1) ?ps1)))
      ?ps)
    (eat-token ?ps :else)
    (parse :mznp/expr ?ps)
    (assoc ?ps :result (->MznIfExpr (recall ?ps :condition)
                                    (recall ?ps :then)
                                    (recall ?ps :elseifs)
                                    (:result ?ps)))
    (eat-token ?ps :endif)))

(s/def ::call-expr
  (s/keys :req-un [::op ::args]))

;;; This isn't picked up for e.g. noattack(i, j, queens[i], queens[j]). I suppose noattack is a
(defrecord MznCallExpr [op args])
;;; <call-expr> ::= <ident-or-quoted-op> [ "(" <expr>, ... ")" ]
;;; This is called for e.g. constraint all_different(foo); :all_different is a builtin-op
;;; POD So I'm doing it different!
(defparse :mznp/call-expr
  [pstate]
  (as-> pstate ?ps
    (store ?ps :op :tkn)
    (eat-token ?ps #(or (builtin-op %) (instance? MznId %)))
    (if (= \( (:tkn ?ps))
      (as-> ?ps ?ps1
        (parse-list ?ps1 \( \) \,)
        (assoc ?ps1 :result (->MznCallExpr (recall ?ps1 :op) (:result ?ps1))))
      (assoc ?ps :result (->MznCallExpr (recall ?ps :op) nil)))))

;;(parse-list-terminated ?ps #(= :in %) :mznp/let-item)
(defrecord MznLetExpr [items expr])
;;; <let-expr> ::= "let" { <let-item>; ... } "in" <expr>
(defparse :mznp/let-expr
  [pstate]
  (as-> pstate ?ps
    (eat-token ?ps :let)
    (parse-list ?ps \{ \} \; :mznp/let-item)
    (store ?ps :items)
    (eat-token ?ps :in)
    (parse :mznp/expr ?ps)
    (assoc ?ps :result (->MznLetExpr (recall ?ps :items) (:result ?ps)))))

;;; <let-item> ::= <var-decl-item> | <constraint-item>
(defparse :mznp/let-item
  [pstate]
  (cond (= :constraint (:tkn pstate)) (parse :mznp/constraint-item pstate)
        (var-decl? pstate) (parse :mznp/var-decl-item pstate)
        :else (assoc pstate :error {:expected "let-item" :got (:tkn pstate) :line (:line pstate)})))

(defrecord MznCompTail [generators where-expr])
;;; <comp-tail> ::= <generator>, ... [ "where" <expr>]
(defparse :mznp/comp-tail
  [pstate]
  (as-> pstate ?ps
    (parse-list-terminated ?ps :term-fn #(#{:where \) \]} %) :parse-tag :mznp/generator)
    (store ?ps :generators)
    (if (= :where (:tkn ?ps))
      (as-> ?ps ?ps1
        (eat-token ?ps1 :where)
        (parse :mznp/expr ?ps1))
      (assoc ?ps :result nil))
    (assoc ?ps :result (->MznCompTail (recall ?ps :generators) (:result ?ps)))))

(defrecord MznGenerator [ids expr])
;;; <generator> ::= <ident> "," ... "in" <expr>
(s/def ::generator
  #(instance? MznGenerator %))

(defparse :mznp/generator
  [pstate]
  (as-> pstate ?ps
    (parse-list-terminated ?ps  :term-fn #(= :in %) :parse-tag :mznp/ident)
    (store ?ps :ids)
    (eat-token ?ps :in)
    (parse :mznp/expr ?ps)
    (assoc ?ps :result (->MznGenerator (recall ?ps :ids) (:result ?ps)))))

;;; <gen-call-expr> ::= <ident-or-quoted-op> "(" <comp-tail> ")" "(" <expr> ")"

;;; See https://www.minizinc.org/doc-2.2.0/en/spec.html#spec-generator-call-expressions
;;; I am improvizing since I don't understand the spec at this point!
;;;
;;; forall(i,j in Domain where i<j) (noattack(i, j, queens[i], queens[j]));
;;; https://www.minizinc.org/doc-2.2.0/en/spec.html#spec-generator-call-expressions

;;; 2019-01-21: gen-call-expr must also include things like:
;;; "sum (w in Workers) (cost[w,doesTask[w]])"  (See pg 23 of the tutorial).
;;; 2020-02006: It also must handle
;;;;   sum (r in Route)
;;;    (let { var int: ActualEffort = (LastWeekOfRoute[r] - FirstWeekOfRoute[r] + 1)*TeamsOnRoute[r]*160; } in
;;;    if (ActualEffort >= RouteEffort[r]) then 0 else (RouteEffort[r] - ActualEffort)*RoutePenalty[r] endif)"
(defrecord MznGenCallExpr [gen-call-op generators where body])

(s/def ::gen-call-expr
  #(instance? MznGenCallExpr %))

(defparse :mznp/gen-call-expr
  [pstate]
  (as-> pstate ?ps
    (store ?ps :gen-call-op :tkn)
    (eat-token ?ps builtin-gen-call-fn) ; 'sum', 'forall' etc.
    (eat-token ?ps \()
    (parse :mznp/comp-tail ?ps)             ; '(w in worker), '(i,j in Domain where i<j)', etc.
    (store ?ps :comp-tail)
    (eat-token ?ps \))
    (eat-token ?ps \()
    (parse :mznp/expr ?ps)                  ; any expr.
    (store ?ps :body)
    (eat-token ?ps \))
    (assoc ?ps :result
           (->MznGenCallExpr
            (recall ?ps :gen-call-op)
            (-> (recall ?ps :comp-tail) :generators)
            (-> (recall ?ps :comp-tail) :where-expr)
            (recall ?ps :body)))))
