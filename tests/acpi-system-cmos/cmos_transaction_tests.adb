pragma Ada_2022;
with Ada.Text_IO;
with ACPI_CMOS_Transactions;

procedure CMOS_Transaction_Tests is
   package T renames ACPI_CMOS_Transactions;
   package P renames T.P;
   package I renames T.I;
   use type T.Phase, T.Initialization, T.Start_Result, T.Finish_Result;
   use type I.Word, I.Byte, P.Device_Kind, P.Port_Pair;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      if not Good then raise Program_Error with "CMOS transaction regression"; end if;
      Checks := Checks + 1;
   end Check;
   type Centuries is array (Positive range <>) of I.Byte;
   Samples : constant Centuries := [0, 13, 14, 63, 127, 128, 255];
begin
   for Kind in P.Device_Kind loop
      for Extended in Boolean loop
         for Century of Samples loop
            for Known in Boolean loop
               for NMI in Boolean loop
                  for Standard_Grant in Boolean loop
                     for Extended_Grant in Boolean loop
                        declare
                           Owner : T.State;
                           Profile : constant P.Profile := (Kind, Extended);
                           Maximum : constant Natural :=
                             (if Kind = P.PC_AT_64 then 63 elsif Extended then 255 else 127);
                           Initialized : T.Initialization;
                           Plan, Reentry : T.Transfer;
                           Started, Reentered : T.Start_Result;
                           Finished : T.Finish_Result;
                           Stopped : Boolean;
                           Previous : I.Word := 0;
                           Should_Start : constant Boolean := Known
                             and (Kind in P.PC_AT_64 | P.Intel_Compatible)
                             and Natural (Century) <= Maximum and Standard_Grant
                             and (Kind /= P.Intel_Compatible or not Extended or Extended_Grant);
                        begin
                           Check (T.Valid (Owner) and T.Current (Owner) = T.Fresh);
                           T.Initialize (Owner, Profile, Century, Known, NMI,
                             Standard_Grant, Extended_Grant, Initialized);
                           Check ((Initialized = T.Initialized) = Should_Start);
                           Check (T.Valid (Owner));
                           if Should_Start then
                              Check (T.NMI_Bit (Owner) = NMI and T.Serial (Owner) = 0);
                              -- Repeated initialization must not erase even a
                              -- ready owner's identity or known selector bit.
                              T.Initialize (Owner, (P.Unknown_Device, False), 0,
                                False, not NMI, False, False, Initialized);
                              Check (Initialized = T.Already_Attempted);
                              Check (T.Current (Owner) = T.Ready and T.NMI_Bit (Owner) = NMI);
                              for Cell in I.Byte loop
                                 for Writing in Boolean loop
                                    Previous := T.Serial (Owner);
                                    T.Begin_Access (Owner, I.Word (Cell), 1, 0,
                                      Writing, 16#A5#, Plan, Started);
                                    if Natural (Cell) > Maximum then
                                       Check (Started = T.Invalid_Extent and Plan.Ticket = 0);
                                    elsif Writing and then
                                      (Natural (Cell) in 0 .. 13 or (Century /= 0 and Cell = Century))
                                    then
                                       Check (Started = T.Read_Only and Plan.Ticket = 0);
                                    else
                                       Check (Started = T.Started and T.Current (Owner) = T.In_Flight);
                                       Check (Plan.Ticket = Previous + 1 and T.Serial (Owner) = Plan.Ticket);
                                       Check (Plan.For_Write = Writing and Plan.Value = 16#A5#);
                                       Check (((Plan.Selector and 16#80#) /= 0) = NMI);
                                       Check (Natural (Plan.Selector and 16#7F#) +
                                         (if Plan.Pair = P.Extended_Pair then 128 else 0) = Natural (Cell));
                                       -- Reentry, a forged completion and stop
                                       -- cannot interrupt the select/data pair.
                                       T.Begin_Access (Owner, 14, 1, 0, False, 0, Reentry, Reentered);
                                       Check (Reentered = T.Not_Ready and Reentry.Ticket = 0);
                                       T.Finish_Access (Owner, 0, T.Completed, Finished);
                                       Check (Finished = T.Wrong_Operation and T.Current (Owner) = T.In_Flight);
                                       T.Stop (Owner, Stopped);
                                       Check (not Stopped and T.Current (Owner) = T.In_Flight);
                                       T.Finish_Access (Owner, Plan.Ticket,
                                         (if Writing then T.Refused else T.Completed), Finished);
                                       Check (Finished = T.Finished and T.Current (Owner) = T.Ready);
                                       T.Finish_Access (Owner, Plan.Ticket, T.Unknown, Finished);
                                       Check (Finished = T.Wrong_Operation and T.Current (Owner) = T.Ready);
                                    end if;
                                    Check (T.Valid (Owner));
                                    if Started /= T.Started then Check (T.Serial (Owner) = Previous); end if;
                                 end loop;
                              end loop;
                              -- An unknown effect permanently retains this
                              -- operation; neither retry, reinitialize, stop nor
                              -- a late fabricated completion may discard it.
                              T.Begin_Access (Owner, 14, 1, 0, False, 0, Plan, Started);
                              Check (Started = T.Started);
                              T.Finish_Access (Owner, Plan.Ticket, T.Unknown, Finished);
                              Check (Finished = T.Finished and T.Current (Owner) = T.Uncertain);
                              T.Begin_Access (Owner, 14, 1, 0, False, 0, Reentry, Reentered);
                              Check (Reentered = T.Not_Ready and Reentry.Ticket = 0);
                              T.Initialize (Owner, Profile, Century, True, NMI, True, True, Initialized);
                              Check (Initialized = T.Already_Attempted and T.Current (Owner) = T.Uncertain);
                              T.Stop (Owner, Stopped);
                              Check (not Stopped and T.Current (Owner) = T.Uncertain);
                              T.Finish_Access (Owner, Plan.Ticket, T.Completed, Finished);
                              Check (Finished = T.Wrong_Operation and T.Current (Owner) = T.Uncertain);
                              Check (T.Valid (Owner));
                           else
                              Check (T.Current (Owner) = T.Failed);
                              T.Begin_Access (Owner, 14, 1, 0, False, 0, Plan, Started);
                              Check (Started = T.Not_Ready and Plan.Ticket = 0);
                              T.Initialize (Owner, (P.Intel_Compatible, True), 0,
                                True, NMI, True, True, Initialized);
                              Check (Initialized = T.Already_Attempted and T.Current (Owner) = T.Failed);
                              T.Stop (Owner, Stopped);
                              Check (Stopped and T.Current (Owner) = T.Stopped);
                           end if;
                        end;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("CMOS transaction checks " & Checks'Image & " PASS");
end CMOS_Transaction_Tests;
