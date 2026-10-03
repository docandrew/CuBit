with Intel_GPU_GuC_Context_Request;
package body Intel_GPU_GuC_Context_Session is
   use Interfaces;
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   package Events renames Intel_GPU_GuC_Context_Event;
   package Requests renames Intel_GPU_GuC_Context_Request;
   use type Life.Phase;
   use type Life.Operation;
   use type Life.Send_Result;
   use type Events.Kind;
   function State (Object : Session) return Life.Phase is (Life.State (Object.Life));
   function Can_Run_And_Retire (Object : Session) return Boolean is (Life.Can_Run_And_Retire (Object.Life));
   procedure Fail (Object : in out Session) is
   begin Life.Fail (Object.Life); end Fail;
   procedure Initialize
     (Object : in out Session; ID : Unsigned_32;
      GPU_Start, Pin_Bias : Unsigned_64;
      Quantum_Us, Preemption_Us : Unsigned_32; Preempt_To_Idle : Boolean) is
   begin
      if State (Object) /= Life.Fresh then return; end if;
      Life.Initialize (Object.Life, ID,
        Owner_Ready and then Requests.Admissible (ID, GPU_Start, Pin_Bias)
        and then Quantum_Us /= 0 and then Preemption_Us /= 0);
      if State (Object) /= Life.Ready then return; end if;
      Object.ID := ID; Object.GPU := GPU_Start; Object.Bias := Pin_Bias;
      Object.Quantum := Quantum_Us; Object.Preemption := Preemption_Us;
      Object.Forced := Preempt_To_Idle;
   end Initialize;
   procedure Submit (Object : in out Session; Action : Life.Operation;
                     Status : out Result) is
      Payload : Events.Words (0 .. 11) := [others => 0];
      Length : Natural := 0;
      Accepted : Boolean;
      Outcome : Life.Send_Result;
   begin
      Status := Rejected;
      if State (Object) in Life.Fresh | Life.Quarantined then return; end if;
      if not Owner_Ready then Fail (Object); Status := Faulted; return; end if;
      case Action is
         when Life.Register_Context =>
            declare Request : constant Requests.Request :=
              Requests.Build (Object.ID, Object.GPU, Object.Bias); begin
               if not Request.Valid then Fail (Object); Status := Faulted; return; end if;
               Payload := Events.Words (Request.Words); Length := 12;
            end;
         when Life.Set_Policy =>
            declare Request : constant Requests.Policy_Request :=
              Requests.Policy (Object.ID, Object.Quantum, Object.Preemption, Object.Forced); begin
               if Request.Length = 0 then Fail (Object); Status := Faulted; return; end if;
               Payload := Events.Words (Request.Words); Length := Request.Length;
            end;
         when Life.Enable | Life.Disable =>
            Payload (0 .. 2) := Events.Words
              (Requests.Scheduling_Mode (Object.ID, Action = Life.Enable));
            Length := 3;
      end case;
      Life.Prepare (Object.Life, Action, Accepted);
      if not Accepted then return; end if;
      Queue (Payload (0 .. Length - 1), Outcome);
      Life.Sent (Object.Life, Outcome);
      if not Owner_Ready then Fail (Object); end if;
      if State (Object) = Life.Quarantined then Status := Faulted;
      elsif Outcome = Life.Backpressure then Status := Backpressure;
      else Status := Queued; end if;
   end Submit;
   procedure Notify_Work (Object : in out Session; Tail_Published : Boolean;
                          Status : out Result) is
      Accepted : Boolean;
      Outcome : Life.Send_Result;
   begin
      Status := Rejected;
      if State (Object) /= Life.Enabled then return; end if;
      if not Owner_Ready then Fail (Object); Status := Faulted; return; end if;
      if not Tail_Published then return; end if;
      Life.Prepare_Notification (Object.Life, Accepted);
      if not Accepted then return; end if;
      Queue (Events.Words (Requests.Schedule (Object.ID)), Outcome);
      Life.Notification_Sent (Object.Life, Outcome);
      if not Owner_Ready then Fail (Object); end if;
      if State (Object) = Life.Quarantined then Status := Faulted;
      elsif Outcome = Life.Backpressure then Status := Backpressure;
      else Status := Queued; end if;
   end Notify_Work;
   procedure Deregister
     (Object : in out Session; Admission_Closed, Work_Drained : Boolean;
      Status : out Result) is
      Accepted : Boolean;
      Outcome : Life.Send_Result;
   begin
      Status := Rejected;
      if State (Object) in Life.Fresh | Life.Quarantined then return; end if;
      if not Owner_Ready then Fail (Object); Status := Faulted; return; end if;
      if not Admission_Closed or else not Work_Drained then return; end if;
      Life.Prepare_Deregister (Object.Life, Accepted);
      if not Accepted then return; end if;
      Queue (Events.Words (Requests.Deregister (Object.ID)), Outcome);
      Life.Deregister_Sent (Object.Life, Outcome);
      if not Owner_Ready then Fail (Object); end if;
      if State (Object) = Life.Quarantined then Status := Faulted;
      elsif Outcome = Life.Backpressure then Status := Backpressure;
      else Status := Queued; end if;
   end Deregister;
   procedure Dispatch (Object : in out Session; Payload : Events.Words;
                       Fence : Unsigned_16; Status : out Result) is
      Item : constant Events.Event := Events.Decode (Payload, Fence);
      Matched : Boolean;
   begin
      Status := Rejected;
      if State (Object) in Life.Fresh | Life.Quarantined then return; end if;
      if not Owner_Ready or Item.Tag = Events.Malformed then
         Fail (Object); Status := Faulted; return;
      end if;
      case Item.Tag is
         when Events.Request_Failure =>
            -- The table must broadcast transport failures to every session.
            -- A directly delivered failure cannot be attributed by wire ID.
            Fail (Object); Status := Faulted; return;
         when Events.Scheduling_Done =>
            if Item.ID = Object.ID then
               Life.Scheduling_Done (Object.Life, Item.ID, Item.Runnable, Matched);
               Status := (if Matched then Handled else Faulted); return;
            end if;
         when Events.Deregister_Done =>
            if Item.ID = Object.ID then
               Life.Deregistration_Done (Object.Life, Item.ID, Matched);
               Status := (if Matched then Handled else Faulted); return;
            end if;
         when Events.Other_Message => null;
         when Events.Malformed => null; -- rejected above
      end case;
      Retain (Payload, Fence, Matched);
      if not Matched or else not Owner_Ready then
         Fail (Object); Status := Faulted;
      else Status := Retained; end if;
   end Dispatch;
end Intel_GPU_GuC_Context_Session;
