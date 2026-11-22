--  Hosted stand-in body: the heartbeat writes no lines in unit tests (its
--  rate bound is tested in log_grant_tests over Intel_GPU_Wait_Heartbeat).
package body Intel_GPU_Wait_Report is
   procedure Observe (Offset, Value : Interfaces.Unsigned_32) is null;
   procedure Pulse (S : Site) is null;
end Intel_GPU_Wait_Report;
