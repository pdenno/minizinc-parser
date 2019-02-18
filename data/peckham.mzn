include "globals.mzn";

int: workforce_size = 570;    
% Next one useful because Level 3&4 can do Level 2&3 work. 

int: numLines;
set of int: Lines = 1..numLines;
int: numJobs;
set of int: Jobs = 1..numJobs;
array[Jobs] of Lines: LineOfJob; % 1 = "NB T" etc. See LineNames above. 
array[Jobs] of int: JobRoutes;

int: numWeeksScheduled;
set of int: Weeks = 1..numWeeksScheduled;
array[Weeks] of int: Workdays;


% RequiredWorkEffort is the number of sewer/days needed to complete the job.
% It is lhpg times quantity. 
% I made the first such that it has to be started now with a big workforce.
% I made the last two such that they can "stretch out" over many weeks.
array[Jobs] of int: WeeksTillDue;
array[Jobs] of int: RequiredWorkEffort;

% These three 'var' things are what we are trying to assign best values to. 
% Number of workerss on the line in any week. 
array [Jobs,Weeks] of var 0..workforce_size: WorkersOnJob;  
% The week at which the work on the line starts.
array [Jobs] of var 1..numWeeksScheduled: startWeek;
% The week at which the work on the line ends.
array [Jobs] of var 1..numWeeksScheduled: endWeek;              

% There are no weeks for which we have assigned more people of a certain skill than we have.
% Note that since Skills 3&4 workers can do 1&2 work, the first of these constraints 
% allows that. 
% (Imagine that all the work was classified as 1&2 work; we wouldn't want the 3&4 workers idle.) 
constraint forall (w in Weeks) 
  (sum (j in Jobs) (WorkersOnJob[j,w]) <= workforce_size);
  
% Work on a job starts before it ends!
constraint forall (j in Jobs) (startWeek[j] <= endWeek[j]);

% Work on a job ends by its due date.
constraint forall (j in Jobs) (endWeek[j] <= WeeksTillDue[j]);

constraint forall (lin in Lines, w1, w2 in Weeks 
                   where w1<w2                                                          /\ 
                   exists(j in Jobs) ((LineOfJob[j] = lin) /\ (w2 <= WeeksTillDue[j]))  /\
                   exists(j in Jobs) ((LineOfJob[j] = lin) /\ (w1 >= startWeek[j])))
              ((sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif))
               ==
               (sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w2] else 0 endif)));

% Nobody assigned when it isn't being worked. 
constraint forall (j in Jobs, w in Weeks where w < startWeek[j])
  (WorkersOnJob[j,w] == 0);
  
constraint forall (j in Jobs, w in Weeks where w > endWeek[j]) 
   (WorkersOnJob[j,w] == 0);

% Effort over weeks worked is sufficient to finish the job but not
% so much as to be more workers than necessary. (The 'but' part NYI. It might over-constrain the problem.)
constraint forall (j in Jobs) 
    (RequiredWorkEffort[j]
     <=
     (sum (w in Weeks) (8*Workdays[w]*WorkersOnJob[j,w])));
                       
% Find an assignment that meets due dates etc. (constraints above) and maximizes run length. 
solve maximize sum (j in Jobs) (endWeek[j] - startWeek[j]);
