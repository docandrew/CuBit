with Interfaces; use Interfaces;
with Ada.Text_IO;
with Intel_GPU_GuC_Context_Lifecycle;
procedure GuC_Deregister_Lifecycle_Tests is
   use Intel_GPU_GuC_Context_Lifecycle;
   procedure Disabled_Context (Object : in out Context; Last : Unsigned_16 := 120) is
      Fence : Unsigned_16;
      OK : Boolean;
   begin
      Initialize (Object, 7, 100, Last, True);
      for Action in Operation loop
         Prepare (Object, Action, Fence, OK); pragma Assert (OK);
         Sent (Object, Queued);
         if Action in Enable | Disable then
            Scheduling_Done (Object, 7, (if Action = Enable then 1 else 0), OK);
            pragma Assert (OK);
         end if;
      end loop;
      pragma Assert (State (Object) = Disabled);
      pragma Assert (Scheduling_Stopped (State (Object)));
   end Disabled_Context;
begin
   for Value in Phase loop
      pragma Assert
        (Scheduling_Stopped (Value) = (Value = Disabled or Value = Deregistered));
   end loop;
   for Scenario in 0 .. 6 loop
      declare
         Object : Context;
         Fence, Again : Unsigned_16;
         OK : Boolean;
      begin
         Disabled_Context (Object);
         Prepare_Deregister (Object, Fence, OK);
         pragma Assert (OK and Fence = 104 and Credits_Held (Object) = 3);
         pragma Assert (not Scheduling_Stopped (State (Object)));
         Prepare_Deregister (Object, Again, OK); pragma Assert (not OK and Again = 0);
         case Scenario is
            when 0 =>
               Deregister_Sent (Object, Backpressure);
               pragma Assert (State (Object) = Disabled and Credits_Held (Object) = 0);
               Prepare_Deregister (Object, Again, OK); pragma Assert (OK and Again = Fence);
               Deregister_Sent (Object, Queued);
            when 1 => Deregister_Sent (Object, Uncertain);
            when 2 => Deregistration_Done (Object, 7, OK); pragma Assert (not OK);
            when others => Deregister_Sent (Object, Queued);
         end case;
         if Scenario in 1 .. 2 then
            pragma Assert (State (Object) = Quarantined);
         else
            Deregistration_Done (Object, 8, OK);
            pragma Assert (not OK and State (Object) = Deregister_Pending);
            if Scenario = 3 then
               Failed_Request (Object, Fence, OK); pragma Assert (OK);
            elsif Scenario = 4 then
               Scheduling_Done (Object, 7, 0, OK); pragma Assert (not OK);
            elsif Scenario = 5 then Fail (Object);
            else
               Deregistration_Done (Object, 7, OK);
               pragma Assert (OK and State (Object) = Deregistered and Credits_Held (Object) = 0);
               pragma Assert (Scheduling_Stopped (State (Object)));
               Prepare (Object, Enable, Again, OK); pragma Assert (not OK);
               Deregistration_Done (Object, 7, OK); pragma Assert (not OK);
            end if;
            pragma Assert (State (Object) = Quarantined);
         end if;
         Deregistration_Done (Object, 7, OK); pragma Assert (not OK);
         pragma Assert (not Scheduling_Stopped (State (Object)));
         Prepare_Deregister (Object, Again, OK); pragma Assert (not OK);
      end;
   end loop;
   declare Object : Context; Fence : Unsigned_16; OK : Boolean; begin
      Disabled_Context (Object, 103);
      Prepare_Deregister (Object, Fence, OK);
      pragma Assert (not OK and Fence = 0 and State (Object) = Disabled);
   end;
   Ada.Text_IO.Put_Line ("Deregister lifecycle PASS: pending, credits, backpressure, stale/error rejection; no firmware");
end GuC_Deregister_Lifecycle_Tests;
