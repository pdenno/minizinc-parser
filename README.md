# minizinc-parser

A Clojure/ClojureScript library to parse MiniZinc to either ASTs populating a metamodel or Clojure code.

## Usage

The grammar implemented is 2.2.0. See https://www.minizinc.org/doc-2.2.0/en/spec.html

The library is still young, though its basic features have been tested.

[![Clojars Project](https://img.shields.io/clojars/v/com.github.pdenno/mznp.svg)](https://clojars.org/com.github.pdenno/mznp)

See the files in the test directory for various usage scenarios, but just to get started,
the `rewrite*` function in the namespace `mznp.rewrite` provides capabilities to parse and rewrite MiniZinc code.

`(rw/rewrite <parse-tag> <string>)`

With no keys it does all steps without debug output.

   -   `<parse-tag>` - a grammar element. For whole MiniZinc programs use :mznp/model; for expressions :mznp/expr.
   -   `<string>` - a string to parse, or if :file? true, a file to slurp and process. (:file? true cannot yet be used from ClojureScript).

  Optional Boolean keywords:

   -   `file?`         - Interpret `<string>` as naming a file.
   -   `:simplify?`    - Return a nested map of the parse with ::type specified for each node in the AST. Default is true.
   -   `:rewrite?`     - Return clojure translation of the input. Default is false.
   -   `:debug?`       - Display diagnostics of the rewriting (when :rewrite? is true).
   -   `:debug-parse?` - Display diagnostics of the parse.

```clojure
(require '[mznp.rewrite :as rw])

(rw/rewrite*
   :mznp/gen-call-expr
   "sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif)")

  :==>

	(mznf/sum  [[j Jobs]]   true
		(if (= (mznf/aref LineOfJob j) lin)
			(mznf/aref WorkersOnJob j w1)
			0))
```
In the example above
   1. `:mznp/gen-call-expr` is a MiniZinc syntax keyword for parsing structures such as MiniZinc `sum`. (See the MiniZinc grammar.), and
   2. `mznf/sum` and `mznf/aref` are functions in the `mznp.mzn-fns` namespace.
   
## Sidebar note

In addition to the MiniZinc functionality, this library contains some Clojure Spec-like features that were part of an experimental version of spec.
This code provides programmatic creation of specs, it is found in src/spec-sha, and corresponds to spec-alpha2 SHA c087ded910b3532a938b37e853df79fc3b9c48c1.

## License

Copyright © 2024 Peter Denno

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
