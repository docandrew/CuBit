with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Lifecycle;
procedure GuC_Context_Lifecycle_Tests is
   use Intel_GPU_GuC_Context_Lifecycle;
   procedure Queue (Object : in out Context; Action : Operation) is
      OK : Boolean;
   begin
      Prepare (Object, Action, OK); pragma Assert (OK);
      if Action in Enable | Disable then
         pragma Assert (Credits_Held (Object) = 4);
      end if;
      Sent (Object, Backpressure);
      pragma Assert (Credits_Held (Object) = 0);
      Prepare (Object, Action, OK); pragma Assert (OK);
      Sent (Object, Queued);
   end Queue;
begin
   -- Wire-ID wrap and publication are transport tests, not lifecycle state.
   -- Replace the old 83-cycle exhaustion expectation with sustained operation.
   declare
      Object : Context;
      OK : Boolean;
   begin
      pragma Assert (not Can_Run_And_Retire (Object));
      Initialize (Object, 7, True);
      pragma Assert (not Can_Run_And_Retire (Object));
      Queue (Object, Register_Context); Queue (Object, Set_Policy);
      Queue (Object, Enable); Scheduling_Done (Object, 7, 1, OK);
      pragma Assert (OK);
      Queue (Object, Disable); Scheduling_Done (Object, 7, 0, OK);
      pragma Assert (OK);
      for Cycle in 1 .. 131_072 loop
         pragma Assert (Can_Run_And_Retire (Object));
         Prepare (Object, Enable, OK); pragma Assert (OK);
         pragma Assert (not Can_Run_And_Retire (Object));
         Sent (Object, Backpressure);
         pragma Assert (Can_Run_And_Retire (Object));
         Queue (Object, Enable); Scheduling_Done (Object, 7, 1, OK);
         pragma Assert (OK and State (Object) = Enabled);
         Prepare_Notification (Object, OK); pragma Assert (OK);
         Notification_Sent (Object, Backpressure);
         Prepare_Notification (Object, OK); pragma Assert (OK);
         Notification_Sent (Object, Queued);
         Queue (Object, Disable); Scheduling_Done (Object, 7, 0, OK);
         pragma Assert (OK and State (Object) = Disabled and
                        Credits_Held (Object) = 0);
      end loop;
      pragma Assert (Can_Run_And_Retire (Object));
      Prepare_Deregister (Object, OK); pragma Assert (OK);
      Deregister_Sent (Object, Queued);
      Deregistration_Done (Object, 7, OK);
      pragma Assert (OK and State (Object) = Deregistered);
   end;
   for Invalid in 0 .. 2 loop
      declare Object : Context; begin
         Initialize (Object,
           (if Invalid = 0 then 65535 elsif Invalid = 1 then Unsigned_32'Last else 7),
           Invalid /= 2);
         pragma Assert (State (Object) = Quarantined);
      end;
   end loop;
   for Failure_At in 0 .. 4 loop
      declare Object : Context; OK : Boolean; begin
         Initialize (Object, 7, True);
         Prepare (Object, Enable, OK);
         pragma Assert (not OK and State (Object) = Ready);
         Queue (Object, Register_Context); Queue (Object, Set_Policy);
         Prepare (Object, Enable, OK);
         pragma Assert (OK and State (Object) = Enable_Pending and
                        Credits_Held (Object) = 4);
         Sent (Object, Backpressure);
         pragma Assert (State (Object) = Policy_Queued and Credits_Held (Object) = 0);
         Queue (Object, Enable);
         Scheduling_Done (Object, 8, 1, OK);
         pragma Assert (not OK and State (Object) = Enable_Pending);
         if Failure_At < 3 then
            -- CT failure classification is separately tested in table/transport.
            Fail (Object);
            pragma Assert (State (Object) = Quarantined and Credits_Held (Object) = 4);
         else
            Scheduling_Done (Object, 7, 1, OK);
            pragma Assert (OK and State (Object) = Enabled and Credits_Held (Object) = 0);
            Queue (Object, Disable);
            if Failure_At = 3 then
               Fail (Object);
               pragma Assert (State (Object) = Quarantined);
            else
               Scheduling_Done (Object, 7, 0, OK);
               pragma Assert (OK and State (Object) = Disabled);
               Prepare (Object, Enable, OK); pragma Assert (OK);
               Sent (Object, Queued);
               Scheduling_Done (Object, 7, 0, OK);
               pragma Assert (not OK and State (Object) = Quarantined);
            end if;
         end if;
         Initialize (Object, 9, True);
         pragma Assert (State (Object) = Quarantined);
      end;
   end loop;
   for Bad_Mode in 0 .. 1 loop
      declare Object : Context; OK : Boolean; begin
         Initialize (Object, 7, True);
         Queue (Object, Register_Context); Queue (Object, Set_Policy);
         Queue (Object, Enable);
         Scheduling_Done (Object, 7, (if Bad_Mode = 0 then 0 else 2), OK);
         pragma Assert (not OK and State (Object) = Quarantined and Credits_Held (Object) = 4);
      end;
   end loop;
   declare Object : Context; OK : Boolean; begin
      Initialize (Object, 7, True);
      Prepare (Object, Register_Context, OK); pragma Assert (OK);
      Sent (Object, Uncertain);
      pragma Assert (State (Object) = Quarantined);
      Prepare (Object, Register_Context, OK); pragma Assert (not OK);
   end;
end GuC_Context_Lifecycle_Tests;
