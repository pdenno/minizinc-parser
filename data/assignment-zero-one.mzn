% Each worker has a cost for doing each task. Every worker does one task.
% This is the linear assignment problem -- # of tasks = # of workers.
int: n = 3;
set of int: Workers = 1..n;
set of int: Tasks   = 1..n;
array[Workers, Tasks] of int: cost = [|10, 20, 13,
			 	      |22, 11, 31,
				      |14, 20, 18|];

array[Workers, Tasks] of var 0..1: x;

% Every worker does one task:
constraint forall (i in Workers) (sum (j in Tasks)   (x[i,j]) = 1);
% Every task gets done: 
constraint forall (j in Tasks  ) (sum (i in Workers) (x[i,j]) = 1); 

solve minimize sum (i in Workers) (sum (j in Tasks) (cost[i,j]*x[i,j]));

output [show(x),"\n"];
