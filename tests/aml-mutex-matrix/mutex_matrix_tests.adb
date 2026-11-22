with AML_Execute;
with Mutex_Matrix_Replay;
procedure Mutex_Matrix_Tests is
   procedure Replay is new Mutex_Matrix_Replay
     (Not_Acquired_Status => AML_Execute.Mutex_Not_Acquired);
begin
   Replay;
end Mutex_Matrix_Tests;
