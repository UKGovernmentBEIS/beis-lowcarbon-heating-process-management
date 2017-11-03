# beis-process-management
This is for BEIS process management using BPM component Activiti.
This Business process flow contains 2 stages.
1. Review Application 
2. Approve Application

There are 3 approval steps in each stage
1. Review/Approve 2.Reject 3. RequestForInfo

Reviewer and Approver can keep a task of the Process Instance in ‘Waiting’ state.
This is a Receive Task and receive a Signal/reply from BEIS forms 
and relaunch task in the Reviewer’s work list.