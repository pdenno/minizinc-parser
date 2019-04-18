{
 :constraints [(mznf/alldifferent DoesTask)],
 :var-decls
 {:n {:name "n", :vartype {:datatype :int}, :mval 3},
  :Workers
  {:name "Workers",
   :vartype {:datatype :mzn-set, :base-type :int},
   :mval (mznf/range 1 n)},
  :Tasks
  {:name "Tasks",
   :vartype {:datatype :mzn-set, :base-type :int},
   :mval (mznf/range 1 n)},
  :Cost
  {:name "Cost",
   :vartype
   {:datatype :mzn-2d-array, :index [Workers Tasks], :base-type :int},
   :mval [[10 20 13] [22 11 31] [14 20 18]]},
  :DoesTask
  {:name "DoesTask",
   :vartype
   {:datatype :mzn-array, :index [Workers], :base-type Tasks},
   :mval nil,
   :var? true}},
 :solve
 {:action :minimize,
  :expr
  (mznf/sum [[w Workers]] true (mznf/aref Cost w (mznf/aref DoesTask w)))}}
