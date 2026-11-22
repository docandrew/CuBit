with Ada.Text_IO;
with Intel_GPU_Table_Preflight;
procedure Table_Preflight_Tests is
begin
   for Fault in 0 .. 6 loop
      declare
         Live : Boolean := True;
         Calls, Turns : Natural := 0;
         OK : Boolean;
         function Current return Boolean is (Live);
         function Valid_Table (Ordinal : Positive) return Boolean;
         package Check is new Intel_GPU_Table_Preflight (Current, Valid_Table);
         use type Check.Phase;
         State : Check.Controller;
         function Valid_Table (Ordinal : Positive) return Boolean is
            Nested : Boolean;
         begin
            Calls := Calls + 1;
            pragma Assert (Calls = Ordinal and Calls <= 65);
            if Calls = 33 then
               case Fault is
                  when 1 => return False;
                  when 2 => Live := False;
                  when 3 => Check.Start (State, 1, Nested); pragma Assert (not Nested);
                  when 4 => Check.Step (State);
                  when others => null;
               end case;
            end if;
            return True;
         end Valid_Table;
      begin
         Check.Start (State, 0, OK); pragma Assert (not OK and Calls = 0);
         Check.Start (State, 65, OK); pragma Assert (OK and Calls = 0);
         Check.Start (State, 1, OK); pragma Assert (not OK);
         while Check.Status (State) = Check.Running loop
            Turns := Turns + 1; pragma Assert (Turns <= 3);
            if Turns = 2 then
               if Fault = 5 then Live := False; end if;
               if Fault = 6 then Check.Cancel (State); end if;
            end if;
            Check.Step (State);
            if Turns = 1 then pragma Assert (Calls = 32); end if;
         end loop;
         if Fault = 0 then
            pragma Assert (Check.Status (State) = Check.Complete and Turns = 3 and Calls = 65);
         else
            pragma Assert (Check.Status (State) = Check.Failed and Turns = 2);
            pragma Assert (Calls = (if Fault in 5 .. 6 then 32 else 33));
         end if;
         Check.Step (State); -- terminal state cannot resume callbacks
         pragma Assert (Calls = (if Fault = 0 then 65 elsif Fault in 5 .. 6 then 32 else 33));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("table preflight PASS7: 32/32/1 callbacks, duplicate/zero start, failed mapping, owner loss, start/step reentry and cancellation (hosted)");
end Table_Preflight_Tests;
