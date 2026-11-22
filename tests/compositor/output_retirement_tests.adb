with Ada.Text_IO;
with Output_Retirement_Check;
procedure Output_Retirement_Tests is
   package R renames Output_Retirement_Check.R;
   use R;
   S, Before : State;
   Grants : Grant_Set;
   Other : State;
begin
   for Leased in Boolean loop
      for Mask in 0 .. 7 loop
         for I in Target_Index loop Grants (I) := (Mask / 2 ** (I - 1)) mod 2 = 1; end loop;
         S := Start (Leased, Grants);
         Before := S;
         for Poll in 1 .. 1000 loop
            Observe_Renderer (S, Busy);
            pragma Assert (S = Before and Status (S) = Renderer_Pending);
         end loop;
         Other := S; Observe_Renderer (Other, Uncertain);
         pragma Assert (Status (Other) = Quarantined and Lease_Held (Other) = Leased);
         Observe_Renderer (S, Retired);
         if Leased then
            pragma Assert (not Can_Release_Lease (S, False));
            pragma Assert (Can_Release_Lease (S, True));
            Other := S; Observe_Lease (Other, False);
            pragma Assert (Status (Other) = Quarantined and Lease_Held (Other));
            Observe_Lease (S, True);
         end if;
         -- Reverse completion order and arbitrary absent targets exercise
         -- partial setup cleanup as well as a fully published output.
         for I in reverse Target_Index loop
            if Grants (I) then
               Other := S; Observe_Revoke (Other, I, False);
               pragma Assert (Status (Other) = Quarantined and Grant_Status (Other, I) = Revoke_Required);
               Observe_Revoke (S, I, True);
               Before := S;
               for Poll in 1 .. 1000 loop
                  Observe_Grant (S, I, False);
                  pragma Assert (S = Before and Status (S) = Grants_Pending);
               end loop;
               Observe_Grant (S, I, True);
            end if;
         end loop;
         pragma Assert (Status (S) = Storage_Ready and not Lease_Held (S) and not Grants_Held (S));
         Other := S; Observe_Storage (Other, False);
         pragma Assert (Status (Other) = Quarantined);
         Observe_Storage (S, True);
         pragma Assert (Status (S) = Released and Valid (S));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("OUTPUT-RETIREMENT: PASS 16 lease/grant layouts, 16000 renderer-busy and 24000 pending-confirmation observations, ordered cleanup and uncertainty");
end Output_Retirement_Tests;
