with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Request;
procedure GuC_Notification_Tests is
   use Intel_GPU_GuC_Context_Lifecycle;
   procedure Enable_Context (Object : in out Context) is
      OK : Boolean;
   begin
      Initialize (Object, 7, True);
      for Op in Register_Context .. Enable loop
         Prepare (Object, Op, OK); pragma Assert (OK);
         Sent (Object, Queued);
      end loop;
      Scheduling_Done (Object, 7, 1, OK);
      pragma Assert (OK and State (Object) = Enabled);
   end Enable_Context;
begin
   declare
      use Intel_GPU_GuC_Context_Request;
   begin
      pragma Assert (Schedule (7) = [16#20001000#, 7]);
      pragma Assert (Schedule (65534) = [16#20001000#, 65534]);
      pragma Assert (Schedule (65535) = [0, 0]);
      pragma Assert (Schedule (Unsigned_32'Last) = [0, 0]);
   end;
   for Scenario in 0 .. 4 loop
      declare
         Object : Context;
         OK : Boolean;
      begin
         Prepare_Notification (Object, OK);
         pragma Assert (not OK);
         Enable_Context (Object);
         Prepare_Notification (Object, OK);
         pragma Assert (OK and Credits_Held (Object) = 0);
         Prepare_Notification (Object, OK);
         pragma Assert (not OK);
         Prepare (Object, Disable, OK);
         pragma Assert (not OK); -- no overlapping send callbacks
         if Scenario = 0 then
            Notification_Sent (Object, Backpressure);
            pragma Assert (State (Object) = Enabled);
            Prepare_Notification (Object, OK);
            pragma Assert (OK);
         elsif Scenario = 1 then
            Notification_Sent (Object, Uncertain);
            pragma Assert (State (Object) = Quarantined);
         elsif Scenario = 2 then
            -- Wire failure classification belongs to the transport/table.
            Fail (Object);
            pragma Assert (State (Object) = Quarantined);
         end if;
         Notification_Sent (Object, Queued);
         if Scenario in 1 .. 2 then
            pragma Assert (State (Object) = Quarantined);
         else
            pragma Assert (State (Object) = Enabled);
            Prepare_Notification (Object, OK);
            pragma Assert (OK);
            Notification_Sent (Object, Queued);
            if Scenario = 4 then
               Prepare (Object, Disable, OK); pragma Assert (OK);
               Sent (Object, Queued);
               Scheduling_Done (Object, 7, 0, OK); pragma Assert (OK);
            end if;
            Fail (Object);
            pragma Assert (State (Object) = Quarantined);
         end if;
      end;
   end loop;
   declare
      Object : Context; OK : Boolean;
   begin
      Enable_Context (Object);
      -- Repeated notifications must not consume a lifetime control budget.
      for Iteration in 1 .. 131_072 loop
         Prepare_Notification (Object, OK);
         pragma Assert (OK);
         Notification_Sent (Object, Queued);
      end loop;
      pragma Assert (State (Object) = Enabled);
      Prepare (Object, Disable, OK);
      pragma Assert (OK);
      Sent (Object, Queued);
      Scheduling_Done (Object, 7, 0, OK);
      pragma Assert (OK and State (Object) = Disabled);
   end;
end GuC_Notification_Tests;
