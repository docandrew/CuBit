with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Update;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
procedure VM_Update_Dispatch_Tests is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   package Events renames Intel_GPU_GuC_Context_Event;
   use type Life.Phase;
   type Injection is (Unrelated, Late_Failure, Malformed, Retention_Overflow);
   procedure Test (Mode : Injection; At_Stage : Positive) is
      Calls, Retained : Natural := 0;
      function Device_Ready return Boolean is (True);
      procedure Queue (Payload : Events.Words;
                       Result : out Life.Send_Result) is
      begin
         pragma Assert (Payload'Length > 0);
         Result := Life.Queued;
      end Queue;
      procedure Retain (Payload : Events.Words; Fence : Unsigned_16;
                        Success : out Boolean) is
      begin
         pragma Assert (Payload'Length = 1 and Fence = 99);
         Retained := Retained + 1;
         Success := Mode /= Retention_Overflow;
      end Retain;
      package Driver is new Intel_GPU_GuC_Context_Session
        (Device_Ready, Queue, Retain);
      Context : Driver.Session;
      use type Driver.Result;
      function Owner return Boolean is
        (Device_Ready and then Driver.State (Context) /= Life.Quarantined);
      procedure Drain (OK : out Boolean);
      procedure Publish (OK : out Boolean);
      procedure Invalidate (OK : out Boolean);
      procedure Resume (OK : out Boolean);
      package Update is new Intel_GPU_VM_Update
        (Owner, Drain, Publish, Invalidate, Resume);
      Object : Update.State;
      Status : Update.Result;
      use type Update.Result;
      procedure Submit (Action : Life.Operation) is
         Result : Driver.Result;
      begin
         Driver.Submit (Context, Action, Result);
         pragma Assert (Result = Driver.Queued);
      end Submit;
      procedure Acknowledge (Runnable : Unsigned_32) is
         Result : Driver.Result;
      begin
         Driver.Dispatch (Context, [16#90001002#, 7, Runnable], 0, Result);
         pragma Assert (Result = Driver.Handled);
      end Acknowledge;
      procedure Pump is
         Result : Driver.Result;
      begin
         Calls := Calls + 1;
         pragma Assert (not Update.Can_Submit (Object));
         if Calls /= At_Stage then return; end if;
         case Mode is
            when Unrelated | Retention_Overflow =>
               Driver.Dispatch (Context, [0 => 16#90000042#], 99, Result);
            when Late_Failure =>
               Driver.Dispatch (Context, [0 => 16#E0000001#], 16#8000#, Result);
            when Malformed =>
               Driver.Dispatch (Context, [16#90001002#, 7, 2], 0, Result);
         end case;
         pragma Assert (Result =
           (if Mode = Unrelated then Driver.Retained else Driver.Faulted));
      end Pump;
      procedure Drain (OK : out Boolean) is
      begin
         -- GPU flush/completion is assumed here; scheduling ACK alone does
         -- not establish it. Transport framing is also outside this fixture.
         Submit (Life.Disable);
         Acknowledge (0);
         pragma Assert (Driver.State (Context) = Life.Disabled);
         Pump; OK := True;
      end Drain;
      procedure Publish (OK : out Boolean) is
      begin
         pragma Assert (Driver.State (Context) = Life.Disabled);
         Pump; OK := True;
      end Publish;
      procedure Invalidate (OK : out Boolean) is
      begin
         pragma Assert (Driver.State (Context) = Life.Disabled);
         Pump; OK := True;
      end Invalidate;
      procedure Resume (OK : out Boolean) is
      begin
         Submit (Life.Enable); Acknowledge (1);
         pragma Assert (Driver.State (Context) = Life.Enabled);
         Pump; OK := True;
      end Resume;
   begin
      Driver.Initialize (Context, 7, 16#200000#, 4096,
                         1000, 500000, False);
      Submit (Life.Register_Context); Submit (Life.Set_Policy);
      Submit (Life.Enable); Acknowledge (1);
      Update.Execute (Object, 0, Status);
      if Mode = Unrelated then
         pragma Assert (Status = Update.Complete and Calls = 4 and Retained = 1);
         pragma Assert (Update.Can_Submit (Object) and Update.Generation (Object) = 1);
      else
         pragma Assert (Status = Update.Ownership_Lost and Calls = At_Stage);
         pragma Assert (not Update.Can_Submit (Object) and Update.Generation (Object) = 0);
         Update.Execute (Object, 0, Status);
         pragma Assert (Status = Update.Rejected and Calls = At_Stage);
      end if;
   end Test;
begin
   for Mode in Injection loop
      for Stage in 1 .. 4 loop Test (Mode, Stage); end loop;
   end loop;
   Ada.Text_IO.Put_Line
     ("VM/session dispatch PASS: late failures, malformed events, retention overflow at all stages; unrelated events retained (mock transport/GPU)");
end VM_Update_Dispatch_Tests;
