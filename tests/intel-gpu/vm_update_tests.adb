with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Update;
with Intel_GPU_GuC_Context_Lifecycle;
procedure VM_Update_Tests is
   -- Owner checks are callbacks too: they may dispatch retirement or attempt
   -- a nested update before returning an otherwise successful snapshot.
   procedure Predicate_Reentry (Retire_On_Check : Natural) is
      Checks, Calls : Natural := 0;
      Inside : Boolean := False;
      function Owner return Boolean;
      procedure Step (OK : out Boolean);
      package Update is new Intel_GPU_VM_Update (Owner, Step, Step, Step, Step);
      Object : Update.State;
      Status : Update.Result;
      use type Update.Result;
      use type Update.Phase;
      function Owner return Boolean is
         Nested : Update.Result;
      begin
         if not Inside then
            Inside := True;
            Checks := Checks + 1;
            Update.Execute (Object, Update.Generation (Object), Nested);
            pragma Assert (Nested = Update.Rejected);
            if Checks = Retire_On_Check then Update.Fail (Object); end if;
            Inside := False;
         end if;
         return True;
      end Owner;
      procedure Step (OK : out Boolean) is
      begin Calls := Calls + 1; OK := True; end Step;
   begin
      Update.Execute (Object, 0, Status);
      pragma Assert (Status = Update.Ownership_Lost);
      pragma Assert (Update.Generation (Object) = 0);
      pragma Assert (Update.Current_Phase (Object) = Update.Quarantined);
      pragma Assert (Calls = (Retire_On_Check - 1) / 2);
      pragma Assert (not Update.Can_Submit (Object));
   end Predicate_Reentry;
   procedure Submission_Predicate_Retirement is
      function Owner return Boolean;
      procedure Step (OK : out Boolean) is begin OK := True; end Step;
      package Update is new Intel_GPU_VM_Update (Owner, Step, Step, Step, Step);
      Object : Update.State;
      function Owner return Boolean is
      begin
         Update.Fail (Object);
         return True;
      end Owner;
      use type Update.Phase;
   begin
      pragma Assert (not Update.Can_Submit (Object));
      pragma Assert (Update.Current_Phase (Object) = Update.Quarantined);
   end Submission_Predicate_Retirement;
   procedure Test (Failure, Loss, Retirement : Natural := 0;
                   Initially_Owned : Boolean := True) is
      Alive : Boolean := Initially_Owned;
      Calls : Natural := 0;
      function Owner return Boolean is (Alive);
      procedure Drain (OK : out Boolean);
      procedure Publish (OK : out Boolean);
      procedure Invalidate (OK : out Boolean);
      procedure Resume (OK : out Boolean);
      package Update is new Intel_GPU_VM_Update (Owner, Drain, Publish, Invalidate, Resume);
      use Update;
      Object : State;
      Status : Result;
      procedure Step (Stage : Phase; OK : out Boolean) is
         Nested : Result;
      begin
         Calls := Calls + 1;
         pragma Assert (not Can_Submit (Object) and Current_Phase (Object) = Stage);
         pragma Assert (Phase'Pos (Stage) = (Calls - 1) mod 4 + 1);
         Execute (Object, Generation (Object), Nested);
         pragma Assert (Nested = Rejected and Current_Phase (Object) = Stage);
         if Calls = Loss then Alive := False; end if;
         if Calls = Retirement then Fail (Object); end if;
         OK := Calls /= Failure;
      end Step;
      procedure Drain (OK : out Boolean) is begin Step (Draining, OK); end Drain;
      procedure Publish (OK : out Boolean) is begin Step (Publishing, OK); end Publish;
      procedure Invalidate (OK : out Boolean) is begin Step (Invalidating, OK); end Invalidate;
      procedure Resume (OK : out Boolean) is begin Step (Resuming, OK); end Resume;
      Saved : Natural;
   begin
      pragma Assert (Can_Submit (Object) = Initially_Owned);
      Execute (Object, 1, Status); pragma Assert (Status = Rejected and Calls = 0);
      Execute (Object, 0, Status);
      if Failure = 0 and Loss = 0 and Retirement = 0 and Initially_Owned then
         pragma Assert (Status = Complete and Calls = 4 and Generation (Object) = 1);
         pragma Assert (Can_Submit (Object));
         Execute (Object, 0, Status); pragma Assert (Status = Rejected and Calls = 4);
         Execute (Object, 1, Status);
         pragma Assert (Status = Complete and Calls = 8 and Generation (Object) = 2);
         Fail (Object);
      else
         pragma Assert (Generation (Object) = 0 and Current_Phase (Object) = Quarantined);
         if Failure /= 0 then
            pragma Assert (Result'Pos (Status) = Failure + 2 and Calls = Failure);
         else
            pragma Assert (Status = Ownership_Lost);
            pragma Assert (Calls = (if not Initially_Owned then 0 else Loss + Retirement));
         end if;
      end if;
      Saved := Calls;
      Alive := True; -- recovery of predicate must not revive a failed state
      Execute (Object, Generation (Object), Status);
      pragma Assert (Status = Rejected and Calls = Saved and not Can_Submit (Object));
end Test;
   type Scenario is (Normal, Disable_Queued_Only, Disable_Wrong_ID,
                     Disable_Uncertain, Resume_Queued_Only,
                     Resume_Wrong_State, Late_Disable_Error);
   procedure Composed (Mode : Scenario) is
      package Life renames Intel_GPU_GuC_Context_Lifecycle;
      use type Life.Phase;
      Context : Life.Context;
      Fence, Disable_Fence : Unsigned_16 := 0;
      Accepted : Boolean;
      Publications, Invalidations, Resumes : Natural := 0;
      function Owner return Boolean is (Life.State (Context) /= Life.Quarantined);
      procedure Drain (OK : out Boolean);
      procedure Publish (OK : out Boolean);
      procedure Invalidate (OK : out Boolean);
      procedure Resume (OK : out Boolean);
      package Update is new Intel_GPU_VM_Update (Owner, Drain, Publish, Invalidate, Resume);
      Object : Update.State;
      Status : Update.Result;
      use type Update.Result;
      procedure Queue (Action : Life.Operation) is
      begin
         Life.Prepare (Context, Action, Fence, Accepted); pragma Assert (Accepted);
         Life.Sent (Context, Life.Queued);
      end Queue;
      procedure Drain (OK : out Boolean) is
      begin
         pragma Assert (not Update.Can_Submit (Object) and Life.State (Context) = Life.Enabled);
         -- GPU flush completion remains a fixture assumption, not implied by
         -- this real scheduling lifecycle or its scheduling-done message.
         Life.Prepare (Context, Life.Disable, Fence, Accepted);
         pragma Assert (Accepted);
         Disable_Fence := Fence;
         Life.Sent (Context, Life.Backpressure);
         pragma Assert (Life.State (Context) = Life.Enabled);
         Life.Prepare (Context, Life.Disable, Fence, Accepted);
         pragma Assert (Accepted and Fence = Disable_Fence);
         Life.Sent (Context, (if Mode = Disable_Uncertain then Life.Uncertain else Life.Queued));
         pragma Assert (Life.State (Context) =
           (if Mode = Disable_Uncertain then Life.Quarantined else Life.Disable_Pending));
         pragma Assert (not Update.Can_Submit (Object));
         if Mode = Disable_Uncertain then
            null;
         elsif Mode = Disable_Queued_Only then
            null;
         else
            Life.Scheduling_Done (Context, (if Mode = Disable_Wrong_ID then 8 else 7), 0, Accepted);
         end if;
         OK := Life.State (Context) = Life.Disabled;
      end Drain;
      procedure Publish (OK : out Boolean) is
      begin
         pragma Assert (Life.State (Context) = Life.Disabled and not Update.Can_Submit (Object));
         Publications := Publications + 1; OK := True;
      end Publish;
      procedure Invalidate (OK : out Boolean) is
      begin
         pragma Assert (Life.State (Context) = Life.Disabled and Publications = Invalidations + 1);
         Invalidations := Invalidations + 1;
         if Mode = Late_Disable_Error then
            Life.Failed_Request (Context, Disable_Fence, Accepted); pragma Assert (Accepted);
         end if;
         OK := True; -- owner check must catch the asynchronously failed context
      end Invalidate;
      procedure Resume (OK : out Boolean) is
      begin
         Resumes := Resumes + 1;
         pragma Assert (Life.State (Context) = Life.Disabled and Publications = Invalidations);
         Queue (Life.Enable);
         pragma Assert (Life.State (Context) = Life.Enable_Pending and not Update.Can_Submit (Object));
         if Mode /= Resume_Queued_Only then
            Life.Scheduling_Done (Context, 7, (if Mode = Resume_Wrong_State then 0 else 1), Accepted);
         end if;
         OK := Life.State (Context) = Life.Enabled;
      end Resume;
   begin
      Life.Initialize (Context, 7, 100, 120, True);
      Queue (Life.Register_Context); Queue (Life.Set_Policy); Queue (Life.Enable);
      Life.Scheduling_Done (Context, 7, 1, Accepted); pragma Assert (Accepted);
      Update.Execute (Object, 0, Status);
      if Mode = Normal then
         pragma Assert (Status = Update.Complete and Life.State (Context) = Life.Enabled);
         Update.Execute (Object, 1, Status);
         pragma Assert (Status = Update.Complete and Publications = 2 and Resumes = 2);
         pragma Assert (Update.Generation (Object) = 2 and Update.Can_Submit (Object));
      else
         pragma Assert (Status /= Update.Complete and not Update.Can_Submit (Object));
         pragma Assert (Update.Generation (Object) = 0);
         if Mode in Disable_Queued_Only | Disable_Wrong_ID | Disable_Uncertain then
            pragma Assert (Publications = 0 and Invalidations = 0 and Resumes = 0);
         elsif Mode = Late_Disable_Error then
            pragma Assert (Publications = 1 and Invalidations = 1 and Resumes = 0);
         else pragma Assert (Publications = 1 and Invalidations = 1 and Resumes = 1);
         end if;
         -- A delayed valid scheduling event must not undo VM quarantine.
         if Life.State (Context) = Life.Disable_Pending then
            Life.Scheduling_Done (Context, 7, 0, Accepted); pragma Assert (Accepted);
         elsif Life.State (Context) = Life.Enable_Pending then
            Life.Scheduling_Done (Context, 7, 1, Accepted); pragma Assert (Accepted);
         end if;
         pragma Assert (not Update.Can_Submit (Object));
         Update.Execute (Object, 0, Status); pragma Assert (Status = Update.Rejected);
      end if;
   end Composed;
begin
   for Check in 1 .. 9 loop Predicate_Reentry (Check); end loop;
   Submission_Predicate_Retirement;
   Test;
   Test (Initially_Owned => False);
   for Stage in 1 .. 4 loop
      Test (Failure => Stage);
      Test (Loss => Stage);
      Test (Retirement => Stage);
   end loop;
   for Mode in Scenario loop Composed (Mode); end loop;
   Ada.Text_IO.Put_Line ("VM/GuC lifecycle PASS: disable/enable acknowledgments, backpressure fence reuse, wrong/missing events, late failure, repeated update cycles (no transport or GPU)");
   Ada.Text_IO.Put_Line ("VM update PASS: exclusion through all stages, ordered callbacks, stale/nested denial, retirement/failure quarantine, repeated generations (mock hardware)");
end VM_Update_Tests;
