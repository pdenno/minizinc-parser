% Consider how much clearer this is, owing to change of variable names, than what is in the tutorial!
int: n;

set of int: Men = 1..n;
set of int: Women = 1..n; 

array[Women, Men] of int: wRanksm;
array[Men, Women] of int: mRanksw;

array[Men]   of var Women: hasWife;
array[Women] of var Men  : hasHusband;

% assignment
constraint forall (m in Men)   (hasHusband[hasWife[m]]=m);
constraint forall (w in Women) (hasWife[hasHusband[w]]=w);

% ranking: If a man m ranks o (other) higher (smaller) than his hasWife, that woman ranks her hasHusband higher than the man.
constraint forall (m in Men, o in Women) (
     mRanksw[m,o] < mRanksw[m,hasWife[m]] -> 
         wRanksm[o,hasHusband[o]] < wRanksm[o,m] );

constraint forall (w in Women, o in Men) (
     wRanksm[w,o] < wRanksm[w,hasHusband[w]] -> 
         mRanksw[o,hasWife[o]] < mRanksw[o,w] );

solve satisfy;

output ["hasWife= ", show(hasWife),"\n", "hasHusband= ", show(hasHusband), "\n"];

% wRanksm =
%  [| 1, 2, 4, 3, 5,
%   | 3, 5, 1, 2, 4,
%   | 5, 4, 2, 1, 3,
%   | 1, 3, 5, 4, 2,
%   | 4, 2, 3, 5, 1 |];

% mRanksw =
%  [| 5, 1, 2, 4, 3,
%   | 4, 1, 3, 2, 5,
%   | 5, 3, 2, 4, 1,
%   | 1, 5, 4, 3, 2,
%   | 4, 3, 2, 1, 5 |];

% hasWive   = [2, 1, 5, 3, 4] ; It is not necessary to specify both of these
% hasHusband= [2, 1, 4, 5, 3]  

% married [man,woman] : [m1,w2] [m2,w1] [m3,w5] [m4,w3] [m5,w4]
% married [woman,man] : [w1,m2] [w2,m1] [w3,m4] [w4,m5] [w5,m3]
