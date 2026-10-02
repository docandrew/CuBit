with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Request;
procedure GuC_Notification_Tests is
   use Intel_GPU_GuC_Context_Lifecycle;
   procedure Enable_Context (Object : in out Context; Base : Unsigned_16) is
      F : Unsigned_16;
      OK : Boolean;
   begin
      Initialize (Object, 7, Base, 65535, True);
      for Op in Register_Context .. Enable loop
         Prepare (Object, Op, F, OK); pragma Assert (OK);
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
         Fence, Other : Unsigned_16;
         OK : Boolean;
      begin
         Prepare_Notification (Object, Fence, OK);
         pragma Assert (not OK and Fence = 0);
         Enable_Context (Object, 100);
         Prepare_Notification (Object, Fence, OK);
         pragma Assert (OK and Fence = 104 and Credits_Held (Object) = 0);
         Prepare_Notification (Object, Other, OK);
         pragma Assert (not OK and Other = 0);
         Prepare (Object, Disable, Other, OK);
         pragma Assert (not OK); -- no overlapping send callbacks
         if Scenario = 0 then
            Notification_Sent (Object, Backpressure);
            Failed_Request (Object, Fence, OK); pragma Assert (not OK);
            Prepare_Notification (Object, Fence, OK);
            pragma Assert (OK and Fence = 104);
         elsif Scenario = 1 then
            Notification_Sent (Object, Uncertain);
            pragma Assert (State (Object) = Quarantined);
         elsif Scenario = 2 then
            Failed_Request (Object, Fence, OK);
            pragma Assert (OK and State (Object) = Quarantined);
         end if;
         Notification_Sent (Object, Queued);
         if Scenario in 1 .. 2 then
            pragma Assert (State (Object) = Quarantined);
         else
            pragma Assert (State (Object) = Enabled);
            Prepare_Notification (Object, Fence, OK);
            pragma Assert (OK and Fence = 105);
            Notification_Sent (Object, Queued);
            if Scenario = 4 then
               Prepare (Object, Disable, Other, OK); pragma Assert (OK);
               Sent (Object, Queued);
               Scheduling_Done (Object, 7, 0, OK); pragma Assert (OK);
            end if;
            Failed_Request (Object, 104, OK);
            pragma Assert (OK and State (Object) = Quarantined);
         end if;
      end;
   end loop;
   declare
      Object : Context; F : Unsigned_16; OK : Boolean;
   begin
      Enable_Context (Object, 65530);
      for Expected in Unsigned_16 range 65534 .. 65535 loop
         Prepare_Notification (Object, F, OK);
         pragma Assert (OK and F = Expected);
         Notification_Sent (Object, Queued);
      end loop;
      Prepare_Notification (Object, F, OK);
      pragma Assert (not OK and F = 0 and State (Object) = Enabled);
      Prepare (Object, Disable, F, OK);
      pragma Assert (OK and F = 65533); -- control fence remains available
   end;
end GuC_Notification_Tests;
