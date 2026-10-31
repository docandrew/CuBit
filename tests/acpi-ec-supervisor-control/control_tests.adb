pragma Ada_2022;
with Ada.Text_IO;
with ACPI_EC_Supervisor_Control;
with ACPI_EC_Event_Types;
procedure Control_Tests is
   package P renames ACPI_EC_Supervisor_Control;
   package E renames ACPI_EC_Event_Types;
   use type P.Phase, P.Stop_Result, P.Snapshot, E.Controller_ID, E.Epoch_ID, E.Token_ID;
   Event : constant E.Notice := (16#FEDC_BA98_0000_0001#, 16#ABCD_9876_0000_0001#, E.Token_ID'Last, 1);
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with "control check" & Checks'Image; end if; end Check;
begin
   for Ceiling in Natural range 0 .. 3 loop
      for Charge in Natural range 0 .. 4 loop
         declare
            S : P.State;
            Before : P.Snapshot;
            Accepted : Boolean;
            Result : P.Stop_Result;
         begin
            Check (P.Valid (S) and then P.View (S).Stage = P.Fresh and then not P.Entry_Closed (S)
              and then not P.Report_Complete (S) and then not P.Cleanup_Allowed (S));
            Before := P.View (S); P.Enter_Cleanup (S, Accepted); Check (not Accepted and then P.View (S) = Before);
            P.Retain (S, Event, Charge, Ceiling, Accepted); Check (not Accepted and then P.View (S) = Before);
            P.Enter_Advance (S, Accepted); Check (Accepted and then P.Entry_Closed (S) and then not P.Report_Complete (S));
            Before := P.View (S); P.Enter_Advance (S, Accepted); Check (not Accepted and then P.View (S) = Before);
            P.Request_Stop (S, Result); Check (Result = P.Stop_Incomplete and then P.View (S) = Before);
            for Invalid_Part in 1 .. 3 loop
               declare Bad : E.Notice := Event; begin
                  case Invalid_Part is
                     when 1 => Bad.Controller := 0;
                     when 2 => Bad.Epoch := 0;
                     when others => Bad.Token := 0;
                  end case;
                  P.Retain (S, Bad, Charge, Ceiling, Accepted); Check (not Accepted and then P.View (S) = Before);
               end;
            end loop;
            P.Retain (S, Event, Charge, Ceiling, Accepted); Check (Accepted = (Charge <= Ceiling) and then P.Valid (S));
            if Accepted then
               Check (P.View (S).Pending.Present and then P.View (S).Charge = Charge and then P.View (S).Ceiling = Ceiling);
               Before := P.View (S); P.Retain (S, Event, 0, Ceiling, Accepted); Check (not Accepted and then P.View (S) = Before);
               for Wrong_Part in 1 .. 4 loop
                  declare Other : E.Notice := Event; begin
                     case Wrong_Part is
                        when 1 => Other.Controller := Other.Controller + 1;
                        when 2 => Other.Epoch := Other.Epoch + 1;
                        when 3 => Other.Token := Other.Token - 1;
                        when others => Other.Number := 2;
                     end case;
                     P.Confirm (S, Other, Accepted); Check (not Accepted and then P.View (S) = Before);
                  end;
               end loop;
               declare T : P.State := S; begin
                  P.Confirm (T, Event, Accepted); Check (Accepted and then not P.View (T).Pending.Present
                    and then P.View (T).Charge = 0 and then P.View (T).Ceiling = 0);
                  Before := P.View (T); P.Confirm (T, Event, Accepted); Check (not Accepted and then P.View (T) = Before);
                  P.Finish_Advance (T, True); Check (P.Entry_Closed (T) and then P.Report_Complete (T));
               end;
            else Check (P.View (S) = Before);
            end if;
            Before := P.View (S); P.Finish_Advance (S, False);
            Check (P.View (S) = (Before with delta Stage => P.Ready) and then not P.Entry_Closed (S) and then P.Report_Complete (S));
            Before := P.View (S); P.Request_Stop (S, Result);
            Check (Result = P.Stop_Accepted and then P.View (S) = (Before with delta Stage => P.Stopped_State));
            Before := P.View (S); P.Request_Stop (S, Result); Check (Result = P.Stop_Already_Closed and then P.View (S) = Before);
            P.Enter_Advance (S, Accepted); Check (not Accepted and then P.View (S) = Before);
            P.Confirm (S, Event, Accepted); Check (not Accepted and then P.View (S) = Before);
            P.Enter_Cleanup (S, Accepted); Check (Accepted and then not P.Report_Complete (S) and then P.Entry_Closed (S));
            Before := P.View (S); P.Request_Stop (S, Result); Check (Result = P.Stop_Incomplete and then P.View (S) = Before);
            P.Retain (S, Event, 0, Ceiling, Accepted); Check (not Accepted and then P.View (S) = Before);
            P.Confirm (S, Event, Accepted); Check (not Accepted and then P.View (S) = Before);
            P.Finish_Cleanup (S); Check (P.View (S) = (Before with delta Stage => P.Stopped_State) and then P.Valid (S));
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Control checks" & Checks'Image);
end Control_Tests;
