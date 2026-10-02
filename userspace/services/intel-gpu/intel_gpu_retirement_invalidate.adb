package body Intel_GPU_Retirement_Invalidate is
   procedure Execute (Object : in out Attempt; Status : out Result;
                      Poll_Limit : Positive := 4096) is
      Engine_Status : Engine.Result;
      GuC_Status : GuC.Result;
      use type Engine.Result;
      use type GuC.Result;
   begin
      Status := Rejected;
      if Object.Used then return; end if;
      Object.Used := True;
      if not Gate then return; end if;
      Engine.Execute (Object.Engine_Attempt, Engine_Status, Poll_Limit);
      if Engine_Status /= Engine.Complete then Status := Engine_Failed; return; end if;
      if not Gate then Status := Ownership_Lost; return; end if;
      GuC.Execute (Object.GuC_Attempt, GuC_Status, Poll_Limit);
      if GuC_Status /= GuC.Complete then Status := GuC_Failed; return; end if;
      if not Gate then Status := Ownership_Lost; return; end if;
      Status := Complete;
   end Execute;
end Intel_GPU_Retirement_Invalidate;
