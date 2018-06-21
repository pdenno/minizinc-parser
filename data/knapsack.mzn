int: n;
set of int: Items = 1..n;
int: capacity;

% There is no 'item type' here, just an enumeration.
array[Items] of int: profits;
array[Items] of int: weights;

% setvar
var set of Items: knapsack;
% capacity
constraint sum (i in Items) (bool2int(i in knapsack)*weights[i]) <= capacity;

%The following is not allowed!
% Hence these are rather procedural: run through all items, only tally for ones in knapsack
%constraint sum (i in knapsack) (weights[i]) <= capacity;

solve maximize sum (i in Items) (bool2int(i in knapsack)*profits[i]) ;

% In the tutorial it looks like this:
% solve maximize sum (i in knapsack) (profits[i]);

output [show(knapsack),"\n"];
