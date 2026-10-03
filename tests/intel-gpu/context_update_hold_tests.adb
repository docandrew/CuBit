with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Context_Table;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
procedure Context_Update_Hold_Tests is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   package Events renames Intel_GPU_GuC_Context_Event;
   Owner : Boolean := True;
   Calls : Natural := 0;
   function Ready return Boolean is (Owner);
   procedure Queue (Payload : Events.Words;
                    Status : out Life.Send_Result) is
   begin
      pragma Assert (Payload'Length > 0);
      Calls := Calls + 1; Status := Life.Queued;
   end Queue;
   procedure Retain (Payload : Events.Words; Fence : Unsigned_16;
                     Success : out Boolean) is
      pragma Unreferenced (Payload, Fence);
   begin Success := True; end Retain;
   package Driver is new Intel_GPU_GuC_Context_Session (Ready, Queue, Retain);
   package Pool is new Intel_GPU_Context_Table (2, Driver, Ready, Retain);
   Object : Pool.Table;
   A, B, ID : Unsigned_32;
   Accepted : Boolean;
   Status : Driver.Result;
   use type Driver.Result;
   use type Pool.Dispatch_Result;
   procedure Submit (Target : Unsigned_32; Action : Life.Operation) is
   begin
      Pool.Submit (Object, Target, Action, Status);
      pragma Assert (Status = Driver.Queued);
   end Submit;
   procedure Ack (Target, Runnable : Unsigned_32) is
      Result : Pool.Dispatch_Result;
      Routed : Unsigned_32;
   begin
      Pool.Dispatch (Object, [16#90001002#, Target, Runnable], 0, Routed, Result);
      pragma Assert (Result = Pool.Delivered and Routed = Target);
   end Ack;
   procedure Start (Session : Unsigned_64; Target : out Unsigned_32) is
   begin
      Pool.Open (Object, 16#200000# + Session * 16#10000#, 4096,
                 1000, 500000, False, Target, Accepted, Session);
      pragma Assert (Accepted and not Pool.Work_Allowed (Object, Target));
      Pool.Hold_Work (Object, Target, Accepted); pragma Assert (not Accepted);
      Submit (Target, Life.Register_Context); Submit (Target, Life.Set_Policy);
      Submit (Target, Life.Enable); Ack (Target, 1);
      pragma Assert (Pool.Work_Allowed (Object, Target));
   end Start;
begin
   Pool.Hold_Work (Object, Pool.No_Context, Accepted); pragma Assert (not Accepted);
   pragma Assert (not Pool.Work_Allowed (Object, 0));
   Start (1, A); Start (2, B);
   for Cycle in 1 .. 8 loop
      Pool.Hold_Work (Object, A, Accepted); pragma Assert (Accepted);
      pragma Assert (not Pool.Work_Allowed (Object, A) and Pool.Work_Allowed (Object, B));
      Pool.Hold_Work (Object, A, Accepted); pragma Assert (not Accepted);
      declare Before : constant Natural := Calls; begin
         Pool.Notify_Work (Object, A, True, Status);
         pragma Assert (Status = Driver.Rejected and Calls = Before);
      end;
      -- Controls and acknowledgments still flow while ordinary work is held.
      Submit (A, Life.Disable);
      Pool.Release_Work (Object, A, Accepted); pragma Assert (not Accepted);
      Ack (A, 0);
      Pool.Release_Work (Object, A, Accepted); pragma Assert (not Accepted);
      -- Mapping publication/invalidation are assumed here, not emulated.
      Submit (A, Life.Enable);
      Pool.Release_Work (Object, A, Accepted); pragma Assert (not Accepted);
      Ack (A, 1);
      pragma Assert (not Pool.Work_Allowed (Object, A));
      Pool.Release_Work (Object, A, Accepted); pragma Assert (Accepted);
      Pool.Release_Work (Object, A, Accepted); pragma Assert (not Accepted);
      Pool.Notify_Work (Object, A, True, Status); pragma Assert (Status = Driver.Queued);
   end loop;
   for Cycle in 1 .. 8 loop
      Pool.Hold_Work (Object, A, Accepted); pragma Assert (Accepted);
      Pool.Release_Work (Object, A, Accepted, Keep_Disabled => True);
      pragma Assert (not Accepted); -- enabled is not the requested idle state
      Submit (A, Life.Disable);
      Pool.Release_Work (Object, A, Accepted, Keep_Disabled => True);
      pragma Assert (not Accepted); -- an outstanding disable is not an ack
      Ack (A, 0);
      declare Before : constant Natural := Calls; begin
         Pool.Release_Work (Object, A, Accepted, Keep_Disabled => True);
         pragma Assert (Accepted and Calls = Before and
           not Pool.Work_Allowed (Object, A));
         Pool.Notify_Work (Object, A, True, Status);
         pragma Assert (Status = Driver.Rejected and Calls = Before);
         Pool.Release_Work (Object, A, Accepted, Keep_Disabled => True);
         pragma Assert (not Accepted);
      end;
      -- Only a later explicit scheduling enable makes new work admissible.
      Submit (A, Life.Enable); Ack (A, 1);
      pragma Assert (Pool.Work_Allowed (Object, A));
   end loop;
   Submit (A, Life.Disable); Ack (A, 0);
   Pool.Hold_Work (Object, A, Accepted); pragma Assert (Accepted);
   Pool.Retire_Session (Object, 1, ID); pragma Assert (ID = A);
   Pool.Release_Work (Object, A, Accepted); pragma Assert (not Accepted);
   Pool.Release_Work (Object, A, Accepted, Keep_Disabled => True);
   pragma Assert (not Accepted);
   pragma Assert (not Pool.Work_Allowed (Object, A) and Pool.Work_Allowed (Object, B));
   Submit (B, Life.Disable); Ack (B, 0);
   Pool.Hold_Work (Object, B, Accepted); pragma Assert (Accepted);
   Owner := False;
   Pool.Release_Work (Object, B, Accepted, Keep_Disabled => True);
   pragma Assert (not Accepted);
   Owner := True;
   pragma Assert (Pool.Failed (Object) and not Pool.Work_Allowed (Object, B));
   Ada.Text_IO.Put_Line ("Context VM-update hold PASS: repeated pause/resume, isolation, retirement, ownership loss");
end Context_Update_Hold_Tests;
