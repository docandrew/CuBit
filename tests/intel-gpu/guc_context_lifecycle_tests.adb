with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Lifecycle;
procedure GuC_Context_Lifecycle_Tests is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   use Life;
   procedure Queue (Object : in out Context; Action : Operation) is
      Fence : Unsigned_16; OK : Boolean;
   begin
      Prepare (Object, Action, Fence, OK);
      pragma Assert (OK and Fence = 100 + Unsigned_16 (Operation'Pos (Action)));
      if Action in Enable | Disable then pragma Assert (Credits_Held (Object) = 4); end if;
      Sent (Object, Backpressure);
      pragma Assert (Credits_Held (Object) = 0);
      Failed_Request (Object, Fence, OK);
      pragma Assert (not OK);
      Prepare (Object, Action, Fence, OK);
      pragma Assert (OK);
      Sent (Object, Queued);
   end Queue;
begin
   declare
      Object : Context; Fence : Unsigned_16; OK : Boolean;
   begin
      Initialize (Object, 7, 100, 109, True);
      Queue (Object, Register_Context); Queue (Object, Set_Policy);
      Queue (Object, Enable); Scheduling_Done (Object, 7, 1, OK);
      Queue (Object, Disable); Scheduling_Done (Object, 7, 0, OK);
      for Cycle in 0 .. 2 loop
         Prepare (Object, Enable, Fence, OK);
         pragma Assert (OK and Fence = 104 + Unsigned_16 (Cycle) * 2);
         Sent (Object, Backpressure);
         pragma Assert (State (Object) = Disabled and Credits_Held (Object) = 0);
         Failed_Request (Object, Fence, OK); pragma Assert (not OK);
         Prepare (Object, Enable, Fence, OK); pragma Assert (OK);
         Sent (Object, Queued); Scheduling_Done (Object, 7, 1, OK);
         pragma Assert (OK and State (Object) = Enabled);
         if Cycle = 2 then
            Prepare_Notification (Object, Fence, OK);
            pragma Assert (not OK); -- last fence reserved for disable
         end if;
         Prepare (Object, Disable, Fence, OK);
         pragma Assert (OK and Fence = 105 + Unsigned_16 (Cycle) * 2);
         Sent (Object, Queued); Scheduling_Done (Object, 7, 0, OK);
         pragma Assert (OK and State (Object) = Disabled);
      end loop;
      Prepare (Object, Enable, Fence, OK);
      pragma Assert (not OK and Fence = 0 and State (Object) = Disabled);
      Failed_Request (Object, 104, OK);
      pragma Assert (OK and State (Object) = Quarantined);
   end;
   -- Fences beyond the caller-reserved interval are neither emitted nor
   -- matched. Exhaustion never wraps into the next interval.
   for Last in Unsigned_16 range 100 .. 110 loop
      declare
         Object : Context; Fence : Unsigned_16; OK : Boolean;
      begin
         Initialize (Object, 7, 100, Last, True);
         if Last < 103 then
            pragma Assert (State (Object) = Quarantined);
         else
            Queue (Object, Register_Context); Queue (Object, Set_Policy);
            Queue (Object, Enable);
            Scheduling_Done (Object, 7, 1, OK);
            pragma Assert (OK);
            for Expected in Unsigned_16 range 104 .. Last loop
               Prepare_Notification (Object, Fence, OK);
               pragma Assert (OK and Fence = Expected);
               Notification_Sent (Object, Backpressure);
               Prepare_Notification (Object, Fence, OK);
               pragma Assert (OK and Fence = Expected);
               Notification_Sent (Object, Queued);
            end loop;
            Prepare_Notification (Object, Fence, OK);
            pragma Assert (not OK and Fence = 0 and State (Object) = Enabled);
            Failed_Request (Object, Last + 1, OK);
            pragma Assert (not OK and State (Object) = Enabled);
            Queue (Object, Disable);
            Scheduling_Done (Object, 7, 0, OK);
            pragma Assert (OK and State (Object) = Disabled);
         end if;
      end;
   end loop;
   for Invalid in 0 .. 3 loop
      declare Object : Context; begin
         Initialize (Object, (if Invalid = 0 then 65535 else 7),
           (if Invalid = 1 then 0 elsif Invalid = 2 then 65533 else 100), 65535, Invalid /= 3);
         pragma Assert (State (Object) = Quarantined);
      end;
   end loop;
   for Failure_At in 0 .. 4 loop
      declare
         Object : Context; Fence : Unsigned_16; OK : Boolean;
      begin
         Initialize (Object, 7, 100, 65535, True);
         Prepare (Object, Enable, Fence, OK);
         pragma Assert (not OK and State (Object) = Ready);
         Queue (Object, Register_Context);
         Queue (Object, Set_Policy);
         Prepare (Object, Enable, Fence, OK);
         pragma Assert (OK and State (Object) = Enable_Pending and Credits_Held (Object) = 4);
         Sent (Object, Backpressure);
         pragma Assert (State (Object) = Policy_Queued and Credits_Held (Object) = 0);
         Queue (Object, Enable);
         Scheduling_Done (Object, 8, 1, OK);
         pragma Assert (not OK and State (Object) = Enable_Pending);
         if Failure_At < 3 then
            Failed_Request (Object, 100 + Unsigned_16 (Failure_At), OK);
            pragma Assert (OK and State (Object) = Quarantined and Credits_Held (Object) = 4);
         else
            Scheduling_Done (Object, 7, 1, OK);
            pragma Assert (OK and State (Object) = Enabled and Credits_Held (Object) = 0);
            Queue (Object, Disable);
            if Failure_At = 3 then
               Failed_Request (Object, 103, OK);
               pragma Assert (OK and State (Object) = Quarantined);
            else
               Scheduling_Done (Object, 7, 0, OK);
               pragma Assert (OK and State (Object) = Disabled);
               Prepare (Object, Enable, Fence, OK);
               pragma Assert (OK and Fence = 104);
               Sent (Object, Queued);
               Scheduling_Done (Object, 7, 0, OK);
               pragma Assert (not OK and State (Object) = Quarantined);
            end if;
         end if;
         Initialize (Object, 9, 200, 65535, True);
         pragma Assert (State (Object) = Quarantined);
      end;
   end loop;
   for Bad_Mode in 0 .. 1 loop
      declare Object : Context; OK : Boolean; begin
         Initialize (Object, 7, 100, 65535, True);
         Queue (Object, Register_Context); Queue (Object, Set_Policy);
         Queue (Object, Enable);
         Scheduling_Done (Object, 7, (if Bad_Mode = 0 then 0 else 2), OK);
         pragma Assert (not OK and State (Object) = Quarantined and Credits_Held (Object) = 4);
      end;
   end loop;
   declare
      Object : Context; Fence : Unsigned_16; OK : Boolean;
   begin
      Initialize (Object, 7, 100, 65535, True);
      Prepare (Object, Register_Context, Fence, OK);
      pragma Assert (OK);
      Sent (Object, Uncertain);
      pragma Assert (State (Object) = Quarantined);
      Prepare (Object, Register_Context, Fence, OK);
      pragma Assert (not OK);
   end;
end GuC_Context_Lifecycle_Tests;
