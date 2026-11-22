with AML_Execute;
generic
   -- Required exact category; the concrete main binds Mutex_Not_Acquired.
   -- No default and no fallback to Unsupported.
   Not_Acquired_Status : AML_Execute.Execution_Status;
procedure Mutex_Matrix_Replay;
