pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with Intel_LPSS_I2C_Profile;
with Intel_LPSS_I2C_Reset;
procedure LPSS_Tests is
   package P renames Intel_LPSS_I2C_Profile;
   package R renames Intel_LPSS_I2C_Reset;
   use type P.Outcome;
   use type P.Hertz;
   use type P.Counter;
   use type R.Word;
   use type R.Physical_Address;
   use type R.T.Tick;
   use type R.T.Count;
   use type R.Stage;
   use type R.Failure;
   use type R.Access_Status;
   use type R.Admission;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   Values : array (R.Register_ID) of R.Word := [others => 0];
   Reads, Writes, Accesses, Clocks, Fail_At, Lose_At, Expire_At, Reverse_At : Natural := 0;
   Failure_Status : R.Access_Status := R.Access_OK;
   Hold_Reset, Hold_Release, Corrupt_Low, Corrupt_High : Boolean := False;
   procedure Reset_Model is
   begin
      Values := [others => 0]; Values (R.Reset_Control) := R.Release_Resets;
      Reads := 0; Writes := 0; Accesses := 0; Clocks := 0;
      Fail_At := 0; Lose_At := 0; Expire_At := 0; Reverse_At := 0;
      Failure_Status := R.Access_OK;
      Hold_Reset := False; Hold_Release := False; Corrupt_Low := False; Corrupt_High := False;
   end Reset_Model;
   procedure Clock (Now : out R.T.Tick; Available : out Boolean) is
   begin
      Clocks := Clocks + 1; Available := Clocks /= Lose_At;
      Now := (if Clocks = Expire_At then 100 elsif Clocks = Reverse_At then 0 else 1);
   end Clock;
   procedure Read (ID : R.Register_ID; Value : out R.Word; Status : out R.Access_Status) is
   begin
      Accesses := Accesses + 1; Reads := Reads + 1; Value := Values (ID);
      case ID is
         when R.Remap_Low => if Corrupt_Low then Value := Value xor 1; end if;
         when R.Remap_High => if Corrupt_High then Value := Value xor 1; end if;
         when others => null;
      end case;
      Status := (if Accesses = Fail_At then Failure_Status else R.Access_OK);
   end Read;
   procedure Write (ID : R.Register_ID; Value : R.Word; Status : out R.Access_Status) is
   begin
      Accesses := Accesses + 1; Writes := Writes + 1;
      Status := (if Accesses = Fail_At then Failure_Status else R.Access_OK);
      if Status in R.Unavailable | R.Denied then return; end if;
      case ID is
         when R.Capabilities => raise Program_Error with "capability register is read only";
         when R.Reset_Control =>
            if Value = 0 then
               if not Hold_Reset then Values (ID) := Value; end if;
            else
               Check (Value = R.Release_Resets, "named reset release mask");
               if not Hold_Release then
                  Values (ID) := (if (Values (R.Capabilities) and 16#100#) /= 0
                                  then Value and R.Function_Reset_Mask else Value);
               end if;
            end if;
         when R.Remap_Low =>
            Check (Writes = 3, "remap low after both reset writes"); Values (ID) := Value;
         when R.Remap_High =>
            Check (Writes = 4, "remap high follows low"); Values (ID) := Value;
      end case;
   end Write;
   procedure Advance is new R.Step (Clock, Read, Write);
   procedure Run (S : in out R.State) is
      Calls : Natural := 0;
   begin
      while R.Observe (S).At_Stage not in R.Ready | R.Failed loop
         Calls := Calls + 1; Check (Calls <= 30, "bounded reset test");
         declare Before : constant Natural := Accesses;
         begin
            Advance (S); Check (Accesses <= Before + 1, "one access per reset step");
         end;
      end loop;
   end Run;
   procedure Reset_Case (Expected : R.Failure) is
      S : R.State;
      Admission : R.Admission;
   begin
      R.Start (S, P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#,
               4096, 1, 100, 20, Admission);
      Check (Admission = R.Accepted, "reset admission"); Run (S);
      if Expected = R.None then
         Check (R.Observe (S).At_Stage = R.Ready and then R.Observe (S).Clock_Hz = P.Root_Clock,
                "reset sequence ready with fixed I2C profile clock");
         Check (Values (R.Remap_Low) = 16#5678_9000# and then Values (R.Remap_High) = 16#1234#,
                "64-bit admitted BAR remap split preserved");
         Check (R.Observe (S).Writes_Completed = 4 and then Accesses = 9, "reset complete access counts");
      else
         Check (R.Observe (S).At_Stage = R.Failed and then R.Observe (S).Reason = Expected,
                "typed reset failure");
      end if;
      if Lose_At = 8 or else Expire_At = 8 or else Reverse_At = 8
        or else (Fail_At = 4 and then Failure_Status = R.Unknown_Effect)
      then Check (R.Observe (S).Reset_May_Be_Released, "release side effect remains visible");
      end if;
      declare Before : constant Natural := Accesses;
      begin Advance (S); Check (Accesses = Before, "terminal reset never replays"); end;
      R.Start (S, P.Intel_Vendor, P.Alder_Lake_N_I2C1, 4096, 4096, 1, 100, 20, Admission);
      Check (Admission = R.Busy, "reset owner cannot restart");
   end Reset_Case;
   subtype Wide is Interfaces.Unsigned_64;
begin
   for Speed in P.Hertz range 1 .. P.Fast_Plus_Maximum loop
      declare
         Timing : constant P.Timing := P.Derive (P.Intel_Vendor, P.Alder_Lake_N_I2C1, Speed);
      begin
         if Timing.Status = P.Calculated then
            Check (Timing.High_Count > 0 and then Timing.Low_Count > 0, "nonzero timing counters");
            Check (Wide (P.Root_Clock) <= Wide (Speed) *
                     (Wide (Timing.High_Count) + Wide (Timing.Low_Count) + 4),
                   "modeled clock never exceeds advertised ceiling");
            Check (Timing.Frequency_Upper_Bound <= Speed, "reported ceiling fits resource");
            Check (Timing.TX_Hold = 6 and then Timing.RX_Hold = 1, "profile hold fields");
         else Check (Timing.Status = P.Counter_Overflow, "slow speed refuses counter overflow");
         end if;
      end;
   end loop;
   declare
      Standard : constant P.Timing := P.Derive (P.Intel_Vendor, P.Alder_Lake_N_I2C0, 100_000);
      Fast : constant P.Timing := P.Derive (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 400_000);
      Plus : constant P.Timing := P.Derive (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 1_000_000);
   begin
      Check (Standard.High_Count = 552 and then Standard.Low_Count = 774, "100kHz golden counters");
      Check (Fast.High_Count = 100 and then Fast.Low_Count = 229, "400kHz golden counters");
      Check (Plus.High_Count = 55 and then Plus.Low_Count = 94, "1MHz golden counters");
      Check (P.Derive (0, P.Alder_Lake_N_I2C1, 400_000).Status = P.Unknown_Controller, "vendor gate");
      Check (P.Derive (P.Intel_Vendor, 0, 400_000).Status = P.Unknown_Controller, "device gate");
      Check (P.Derive (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 0).Status = P.Invalid_Speed, "zero speed");
      Check (P.Derive (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 1_000_001).Status = P.Invalid_Speed, "unsupported speed");
   end;
   Reset_Model; Reset_Case (R.None);
   Reset_Model; Values (R.Capabilities) := 16#100#; Reset_Case (R.None);
   for Position in 1 .. 9 loop
      for Status in R.Access_Status range R.Unavailable .. R.Unknown_Effect loop
         Reset_Model; Fail_At := Position; Failure_Status := Status;
         Reset_Case ((if Status = R.Unknown_Effect then R.Register_Indeterminate else R.Register_Refused));
         Check (Accesses = Position, "failure stops at access position");
      end loop;
   end loop;
   for Position in 1 .. 18 loop
      Reset_Model; Lose_At := Position; Reset_Case (R.Clock_Unavailable);
      Reset_Model; Expire_At := Position; Reset_Case (R.Deadline_Expired);
      Reset_Model; Reverse_At := Position; Reset_Case (R.Clock_Reversed);
   end loop;
   Reset_Model; Hold_Reset := True; Reset_Case (R.Budget_Exhausted);
   Reset_Model; Hold_Release := True; Reset_Case (R.Budget_Exhausted);
   Reset_Model; Corrupt_Low := True; Reset_Case (R.Remap_Mismatch);
   Reset_Model; Corrupt_High := True; Reset_Case (R.Remap_Mismatch);
   Reset_Model; Values (R.Capabilities) := 16#10#; Reset_Case (R.Wrong_Controller_Type);
   declare S : R.State; A : R.Admission;
   begin
      Reset_Model;
      R.Start (S, 0, P.Alder_Lake_N_I2C1, 4096, 4096, 1, 100, 20, A);
      Check (A = R.Unsupported_Device, "unsupported reset profile has no access");
      R.Start (S, P.Intel_Vendor, P.Alder_Lake_N_I2C1, 0, 4096, 1, 100, 20, A);
      Check (A = R.Invalid_Window, "zero BAR refused");
      R.Start (S, P.Intel_Vendor, P.Alder_Lake_N_I2C1, 4097, 4096, 1, 100, 20, A);
      Check (A = R.Invalid_Window, "unaligned BAR refused");
      R.Start (S, P.Intel_Vendor, P.Alder_Lake_N_I2C1, 4096, 767, 1, 100, 20, A);
      Check (A = R.Invalid_Window, "short BAR refused");
      R.Start (S, P.Intel_Vendor, P.Alder_Lake_N_I2C1, R.Physical_Address'Last - 3, 4096, 1, 100, 20, A);
      Check (A = R.Invalid_Window, "wrapping BAR refused");
      R.Start (S, P.Intel_Vendor, P.Alder_Lake_N_I2C1, 4096, 4096, 100, 100, 20, A);
      Check (A = R.Expired and then Accesses = 0, "expired admission has no access");
   end;
   Ada.Text_IO.Put_Line ("LPSS I2C checks" & Checks'Image);
end LPSS_Tests;
