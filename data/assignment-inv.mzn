include "alldifferent.mzn";
% Each worker has a cost for doing each task. Every worker does one task.
% This is the linear assignment problem -- # of tasks = # of workers.
int: n = 3;
set of int: Workers = 1..n;
set of int: Tasks   = 1..n;
array[Workers, Tasks] of int: cost = [|10, 20, 13,
			 	      |22, 11, 31,
				      |14, 20, 18|];

array[Tasks] of var 1..n: whoDoes; 

constraint alldifferent(whoDoes); % All the tasks get done by someone.

solve minimize sum (t in Tasks) (cost[whoDoes[t],t]);

output [show(whoDoes),"\n"];
