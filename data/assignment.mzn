include "alldifferent.mzn";
% Each worker has a cost for doing each task. Every worker does one task.
% This is the linear assignment problem -- # of tasks = # of workers.
int: n = 3;
set of int: Workers = 1..n;
set of int: Tasks   = 1..n;
array[Workers, Tasks] of int: cost = [|10, 20, 13,
			 	      |22, 11, 31,
				      |14, 20, 18|];

array[Workers] of var Tasks: doesTask; 

constraint alldifferent(doesTask); % everyone does a task.

solve minimize sum (w in Workers) (cost[w,doesTask[w]]);

output [show(doesTask),"\n"];


