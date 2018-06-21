% Number of different products
int: nproducts; 
set of int: Products = 1..nproducts;  
% profit per unit for each product
array[Products] of int: profit;
array[Products] of string: pname; 
% Number of resources
int: nresources; 
set of int: Resources = 1..nresources; 
% amount of each resource available
array[Resources] of int: capacity; % Could say "of par int" but that's the default.
array[Resources] of string: rname;

% units of each resource required to produce 1 unit of product
array[Products, Resources] of int: consumption;
% more than one of these gets tedious
constraint assert(forall (r in Resources, p in Products) 
                         (consumption[p,r] >= 0), "Error: negative consumption");

% bound on number of Products 
int: mproducts = max (p in Products)                                  % for each product, 
                     (min (r in Resources where consumption[p,r] > 0) % what are you going to run out of first?
                          (capacity[r] div consumption[p,r]));

% Variables: how much should we make of each product? <==========================
array[Products]  of var 0..mproducts:     produce;
array[Resources] of var 0..max(capacity): used; % That's almost a constraint!

% Production cannot use more than the available Resources:
constraint forall (r in Resources) (     
         used[r] = sum (p in Products)(consumption[p, r] * produce[p])
      /\ used[r] <= capacity[r] %  /\ is AND, but used here like a LET.
);    

% Maximize profit
solve maximize sum (p in Products) (profit[p]*produce[p]);

output [ show(pname[p]) ++ " = " ++ show(produce[p]) ++ ";\n" |
         p in Products ] ++
       [ show(rname[r]) ++ " = " ++ show(used[r]) ++ ";\n" |
         r in Resources ];
