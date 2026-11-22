with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Render_Startup;
procedure Startup_Tests is
   use CuBit.Render_Startup;
   Next : Decision;
   Cases : Natural := 0;
begin
   declare
      -- The first three are invalid identities; the last four are valid.
      -- Entries 4 and 6 intentionally reuse a PID with another generation.
      Identities : constant array (Positive range 1 .. 7) of Unsigned_64 :=
        [0, 1, 2 ** 32, 2 ** 32 + 1, 2 ** 32 + 2, 2 ** 33 + 1,
         Unsigned_64'Last];
   begin
      for Old in Identities'Range loop
         for New_Child in Identities'Range loop
            pragma Assert (Fresh_Retry (Identities (Old), Identities (New_Child)) =
              (Old >= 4 and New_Child >= 4 and Old /= New_Child));
         end loop;
      end loop;
      Ada.Text_IO.Put_Line ("RENDER-STARTUP: PASS 49 fresh-incarnation cases");
   end;
   pragma Assert (Initial_Attempt (Optional, False) = Software_Only);
   pragma Assert (Initial_Attempt (Optional, True) = With_Render);
   pragma Assert (Initial_Attempt (Required, False) = With_Render);
   for Demand in Requirement loop
      for Mode in Attempt loop
         for Approved in Boolean loop
            for Result in Admission loop
               for Slot in Capability loop
                  Next := Decide (Demand, Mode, Approved, Result, Slot);
                  if not Approved then pragma Assert (Next /= Resume_Render); end if;
                  if Result in Pending | Rejected | Uncertain then
                     pragma Assert (Next not in Resume_Render | Resume_Software);
                  end if;
                  if Demand = Required then pragma Assert (Next not in Resume_Software | Discard_Then_Software); end if;
                  if Mode = Software_Only then pragma Assert (Next /= Discard_Then_Software); end if;
                  if Next = Resume_Software then
                     pragma Assert (Demand = Optional and Mode = Software_Only and Result = Not_Requested and Slot = Empty);
                  end if;
                  if Mode = With_Render and Approved and Result = Admitted and Slot = Render_Endpoint then
                     pragma Assert (Next = Resume_Render);
                  end if;
                  if Mode = Software_Only and Demand = Optional and Result = Not_Requested and Slot = Empty then
                     pragma Assert (Next = Resume_Software);
                  end if;
                  for Stopped in Boolean loop
                     pragma Assert (Retry_Software (Next, Stopped) = (Next = Discard_Then_Software and Stopped));
                     if Retry_Software (Next, Stopped) then
                        pragma Assert (Decide (Demand, Software_Only, Approved, Not_Requested, Empty) = Resume_Software);
                     end if;
                     Cases := Cases + 1;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("RENDER-STARTUP: PASS" & Natural'Image (Cases) &
     " cases; mandatory denial, pending/uncertain stop, empty-slot software gate and one retry bound");
end Startup_Tests;
