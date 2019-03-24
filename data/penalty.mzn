% This model lets jobs be late but penalizes them for it.
% FEATURES: 
%    1) Two skill levels.
%    2) WorkdaysPerWeek and WorkhoursPerDay (for whole weeks). 
%    3) Keeps number of LOWER SKILLED WORKERS on the line constant (higher skilled added as needed).
%    4) Balances line by ratio of high-skilled to total. (e.g. 1/4 means need 1 out of 4 workers to be high skilled.) 
%    5) Penalizes late jobs with CUSTOMER-PRIORITY WEIGHTED PENALTIES. 
%   PROS/CONS: Should always give an answer / I don't know the cons yet!
int: workforce_size;    
int: numLines;
set of int: Lines = 1..numLines;
array[Lines] of int: LinePenalty; 

int: numJobs;
set of int: Jobs = 1..numJobs;
array[Jobs] of Lines: LineOfJob; 

int: numWeeksScheduled;
set of int: Weeks = 1..numWeeksScheduled;
array[Weeks] of int: WorkdaysPerWeek;
array[Weeks] of int: WorkhoursPerDay;

% RequiredWorkEffort is the number of sewer/days needed to complete the job.
% It is lhpg times quantity. 
% I made the first such that it has to be started now with a big workforce.
% I made the last two such that they can "stretch out" over many weeks.
array[Jobs] of int: WeeksTillDue;
array[Jobs] of int: RequiredWorkEffort;

% These three 'var' things are what we are trying to assign best values to. 
% Number of workers on the line in any week. 
array [Jobs,Weeks] of var 0..workforce_size: WorkersOnJob;

% There are no weeks for which we have assigned more workers than we have.
constraint forall (w in Weeks) 
  (sum (j in Jobs) (WorkersOnJob[j,w]) <= workforce_size);
  
% w2 last week any job runs on line. w1 is any week < w2 that a job runs on line. 
% Work on a line is constant across all weeks line operates. 
constraint forall (lin in Lines, w1, w2 in Weeks
		   where w1 < w2                                                           /\
		   forall (j in Jobs) ((LineOfJob[j] = lin) /\ (WorkersOnJob[j,w1] != 0))  /\
		   (w2 == max (j in Jobs, w in (w1+1)..numWeeksScheduled)
		               (if ((LineOfJob[j] == lin) /\ (WorkersOnJob[j,w] != 0)) then w else 0 endif)))
              ((sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif))
               ==
               (sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w2] else 0 endif)));

% Effort over weeks worked is sufficient to finish the job. 
constraint forall (j in Jobs) 
    (RequiredWorkEffort[j] <= (sum (w in Weeks) (WorkhoursPerDay[w]*WorkdaysPerWeek[w]*WorkersOnJob[j,w])));
                       
% Minimize the line-weighted lateness penalty. 
solve minimize sum (j in Jobs, w in Weeks
      		         where (w == max (w in Weeks) (if (WorkersOnJob[j,w] != 0) then w else 0 endif)))
                   (if (w <= WeeksTillDue[j]) then 0 else (w - WeeksTillDue[j])*LinePenalty[LineOfJob[j]] endif);
