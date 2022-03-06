# minizinc-parser

A Clojure library to parse MiniZinc to ASTs (populating a metamodel) or Clojure code.

## Usage

The grammar implemented is 2.2.0. See https://www.minizinc.org/doc-2.2.0/en/spec.html

The library is still young, though its basic features have been tested.

[![Clojars Project](https://img.shields.io/clojars/v/com.github.pdenno/minizinc-parser.svg)](https://clojars.org/com.github.pdenno/minizinc-parser)

See the files in the test directory for various usage scenarios, but just to get started, 
the `rewrite*` function in the `namespace pdenno.mznp.rewrite` provides capabilities to parse and rewrite MiniZinc code.

```
   With no keys it does all steps without debug output.

      tag - a grammar element. For whole MiniZinc programs use :mznp/model; for expressions :mznp/expr.
      str - a string to parse, or if :file? true, a file to slurp and process. (:file? true cannot yet be used from JS).

    Optional Boolean keywords:
      :simplify?    - Return a nested map of the parse with ::type specified for each node in the AST. Default is true.
      :rewrite?     - Return clojure translation of the input. Default is false.
      :debug?       - Display diagnostics of the rewriting (when :rewrite? is true).
      :debug-parse? - Display diagnostics of the parse.
```


```clojure
(require '[pdenno.mznp.rewrite :as rw])

(rewrite*  :mznp/gen-call-expr "sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif)")
  :==> 
    (mznf/sum  [[j Jobs]]   true
        (if  (= (mznf/aref LineOfJob j) lin) (mznf/aref WorkersOnJob j w1) 0))
```		
In the example above
   1. `mznp/gen-call-expr` is a syntax keyword for parsing structures such as MiniZinc `sum`. (See the MiniZinc grammar.)
   2. `mznf/sum` and `mznf/aref` are functions in the namespece pdenno.mznp.mzn-fns namespace, and

## License

Copyright © 2022 Peter Denno

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
