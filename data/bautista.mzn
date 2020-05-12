
enum ProductType = {A, B, C};                  %# Product types
int : cycletime  = 4;                          %# cycle time
int : windowSize = 6;                          %# window size

int : NumWorkstations   = 3;                   %# Number of workstations
set of int: Workstation = 1..NumWorkstations;   

array[ProductType] of int: demand   = [3,1,2];   %# Demand by model. 
array[Workstation] of int: nWorkers = [1,2,1];   %# Number of homogeneous workers, b_k. 
array[ProductType, Workstation] of int : ptime;  %# processing time by product / workstation

%# =================================== Design Vars ==================================================================
int : TotalToProduce  = 6;                         %# total number of units to produce (total demand)
set of int: LaunchSequence = 1..TotalToProduce;    %# Index on launch sequence
int : maxtime = 3000; %# It is less than TotalToProduce * max(p) * NumWorkstations.
%# work overload (w) is a measurement, in units of time, of work that cannot be completed at the standard
%# rhythm of an established activity, within the time granted to the workstations (WINDOW).
array[LaunchSequence, Workstation] of var int        :   overload;  %# work overload
array[LaunchSequence, Workstation] of var int        :        rho;  %# processing time required by workstation/launch, rho_{k,t}
array[LaunchSequence, Workstation] of var 0..maxtime : start_time;  %# s[t,k] start time of t at workstation k

array[LaunchSequence] of var ProductType :  sequence;  %# Product type is at this position in launch.
%#array[LaunchSequence] of ProductType :  sequence = [C, B, A, A, C, A];

%# ----------- I added the following, not described in the paper. ----------
%# Can't start before the start of the window, and there is a workstation offset.
constraint  forall (t in LaunchSequence, k in Workstation)
   (start_time[t,k] >= cycletime*(t-1) + cycletime*(k-1));

%# Like Eqn 6 but for s: Start after previous instance finished.
constraint forall (t in LaunchSequence, k in Workstation where t>1)
   (start_time[t,k] >= start_time[t-1,k] + rho[t-1,k] - overload[t-1,k]);

%# Like Eqn 7 but for s: start on workstation after done at previous workstation. 
constraint forall (t in LaunchSequence, k in Workstation where k>1)
   (start_time[t,k] >= start_time[t,k-1] + rho[t,k-1] - overload[t,k-1]);

%# Work overload is the amount that the instance extends beyond the window.
constraint forall (t in LaunchSequence, k in Workstation)
   (overload[t,k] == max(0, start_time[t,k] + rho[t,k] - cycletime*(t-1) - cycletime*(k-1) - windowSize));

constraint start_time[1,1] == 0;
%#---------------------------------------------------------------------

%# Eqn 2: Demand by model is met. (Eqn 1 is solve stmt. )
constraint forall (i in ProductType)
   (demand[i] == sum (t in LaunchSequence)
                     (if sequence[t] == i then 1 else 0 endif));

%# Eqn 4: Processing time at workstation is processing time of that model at workstation. 
constraint forall (t in LaunchSequence, k in Workstation)
   (rho[t,k] == ptime[sequence[t],k]);

%# Eqn 5 overload can't be larger than work required. 
constraint forall (t in LaunchSequence, k in Workstation)
   (rho[t,k] - overload[t,k] >= 0);

%# Eqn 10: Work overload is non-negative. 
constraint forall (t in LaunchSequence, k in Workstation)
   (overload[t,k] >= 0);

%# Eqn 11: {sequence \in {0,1}} is part of the var definition.

%# Eqn 1
solve minimize
   sum (t in LaunchSequence, k in Workstation)
      (nWorkers[k]* overload[t,k]);
