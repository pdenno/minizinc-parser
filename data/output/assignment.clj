{:constraints
 [{:type :MznCallExpr,
   :op :alldifferent,
   :args
   [{:type :MznExpr,
     :atom
     {:type :MznExprAtom,
      :head {:type :MznId, :name "doesTask"}}}]}],
 :var-decls
 {"n" {:name "n", :vartype :int, :init 3},
  "Workers"
  {:name "Workers",
   :vartype {:datatype :mzn-set, :base-type :int},
   :init (range-op 1 n)},
  "Tasks"
  {:name "Tasks",
   :vartype {:datatype :mzn-set, :base-type :int},
   :init (range-op 1 n)},
  "cost"
  {:name "cost",
   :vartype
   {:datatype :mzn-array, :index [Workers Tasks], :base-type :int},
   :init [[10 20 13] [22 11 31] [14 20 18]]},
  "doesTask"
  {:name "doesTask",
   :vartype
   {:datatype :mzn-array, :index [Workers], :base-type Tasks},
   :init nil,
   :var? true}},
 :solve
 {:action :minimize,
  :expr
  (sum [[w Workers]] true (aacc-op cost w (aacc-op doesTask w)))}}
