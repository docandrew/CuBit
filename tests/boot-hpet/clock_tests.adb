with Interfaces; use Interfaces;
with HPET_Clock;
with Ada.Text_IO;
procedure Clock_Tests is
   type Fault_Kind is (Bad_Revision, All_Ones_ID, Narrow_Counter,
     Zero_Period, Excess_Period, Bad_Config, Bad_Timer, Bad_Timer_Readback, Bad_First,
     Bad_Progress, Backward_Progress, Runtime_Invalid, Runtime_Before_Epoch);
   procedure Fault_Run (Fault : Fault_Kind; Bad_Timer_Index : Natural := 0) is
      Config : Unsigned_32 := 3;
      Timers : array (0 .. 31) of Unsigned_32 := [others => 16#4004#];
      Reads, Writes, Pauses : Natural := 0;
      function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      begin
         case Offset is
            when 0 =>
               return (case Fault is
                 when Bad_Revision => 16#3F00#,
                 when All_Ones_ID => Unsigned_32'Last,
                 when Narrow_Counter => 16#1F01#,
                 when others => 16#3F01#);
            when 4 =>
               return (case Fault is
                 when Zero_Period => 0,
                 when Excess_Period => 100_000_001,
                 when others => 100_000_000);
            when 16#10# =>
               return (if Fault = Bad_Config then Unsigned_32'Last else Config);
            when others =>
               declare
                  Index : constant Natural := Natural ((Offset - 16#100#) / 16#20#);
               begin
                  return (if Fault = Bad_Timer and Index = Bad_Timer_Index
                          then Unsigned_32'Last else Timers (Index));
               end;
         end case;
      end Read32;
      function Read64 (Offset : Unsigned_32) return Unsigned_64 is
      begin
         pragma Assert (Offset = 16#F0#);
         Reads := Reads + 1;
         if Reads = 1 then
            return (if Fault = Bad_First then Unsigned_64'Last else 100);
         elsif Reads = 2 then
            return (if Fault = Bad_Progress then Unsigned_64'Last
                    elsif Fault = Backward_Progress then 99 else 110);
         else
            return (if Fault = Runtime_Invalid then Unsigned_64'Last else 99);
         end if;
      end Read64;
      procedure Write32 (Offset, Value : Unsigned_32) is
      begin
         Writes := Writes + 1;
         if Offset = 16#10# then Config := Value;
         else
            declare
               Index : constant Natural := Natural ((Offset - 16#100#) / 16#20#);
            begin
               Timers (Index) :=
                 (if Fault = Bad_Timer_Readback and Index = Bad_Timer_Index
                  then Unsigned_32'Last else Value);
            end;
         end if;
      end Write32;
      procedure Pause is
      begin Pauses := Pauses + 1; end Pause;
      package Clock is new HPET_Clock (Read32, Read64, Write32, Pause);
      use type Clock.Startup_Status;
      OK : Boolean;
      Before : Natural;
   begin
      Clock.Initialize (4, OK);
      pragma Assert (Pauses = 0);
      pragma Assert (OK = (Fault in Runtime_Invalid | Runtime_Before_Epoch));
      pragma Assert (Clock.Status =
        (case Fault is
           when Bad_Revision | All_Ones_ID | Narrow_Counter |
                Zero_Period | Excess_Period => Clock.Identity_Rejected,
           when Bad_Config => Clock.Configuration_Unreadable,
           when Bad_Timer => Clock.Timer_Unreadable,
           when Bad_Timer_Readback => Clock.Timer_Mask_Not_Confirmed,
           when Bad_First => Clock.Counter_Unreadable,
           when Bad_Progress | Backward_Progress => Clock.Counter_Invalid,
           when others => Clock.Running));
      if Fault in Bad_Revision .. Bad_Config then
         pragma Assert (Writes = 0 and Reads = 0);
      elsif Fault = Bad_Timer then
         pragma Assert (Writes = Bad_Timer_Index + 1 and Reads = 0 and Config = 0);
      elsif Fault = Bad_Timer_Readback then
         pragma Assert (Writes = Bad_Timer_Index + 2 and Reads = 0 and Config = 0);
         pragma Assert (Clock.Timer_Offset = 16#100# + Unsigned_32 (Bad_Timer_Index) * 16#20#);
         pragma Assert (Clock.Timer_Before = 16#4004# and Clock.Timer_After = Unsigned_32'Last);
      elsif Fault = Bad_First then
         pragma Assert (Writes = 33 and Reads = 1 and Config = 0);
      elsif not OK then
         pragma Assert (Writes = 35 and Reads = 2 and Config = 0);
      end if;
      Before := Writes;
      pragma Assert (Clock.Microseconds = Unsigned_64'Last);
      pragma Assert (Writes = Before);
      -- Startup evidence is immutable even if a later read is unavailable.
      pragma Assert (Clock.Available = OK);
      Clock.Initialize (4, OK);
      pragma Assert (not OK and Writes = Before);
   end Fault_Run;
   procedure Run (Count : Positive; Ignore_Write : Natural; Moving : Boolean;
                  Change_Unrelated : Boolean := False;
                  Stuck_Enable : Unsigned_32 := 0;
                  Initial_Timer : Unsigned_32 := 16#4004#) is
      Config : Unsigned_32 := 3;
      Timers : array (0 .. 31) of Unsigned_32 := [others => Initial_Timer];
      Counter : Unsigned_64 := 100;
      Writes : Natural := 0;
      function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      begin
         case Offset is
            when 0 => return 16#2001# or Shift_Left (Unsigned_32 (Count - 1), 8);
            when 4 => return 100_000_000;
            when 16#10# => return Config;
            when others => return Timers (Natural ((Offset - 16#100#) / 16#20#));
         end case;
      end;
      function Read64 (Offset : Unsigned_32) return Unsigned_64 is
      begin
         pragma Assert (Offset = 16#F0#);
         if Moving and (Config and 1) /= 0 then Counter := Counter + 10; end if;
         return Counter;
      end;
      procedure Write32 (Offset, Value : Unsigned_32) is
      begin
         Writes := Writes + 1;
         if Writes = Ignore_Write then return; end if;
         if Offset = 16#10# then
            if (Value and 1) /= 0 then
               for T in 0 .. Count - 1 loop pragma Assert ((Timers (T) and 4) = 0); end loop;
               pragma Assert ((Value and 2) = 0);
            end if;
            Config := Value;
         else
            pragma Assert ((Config and 3) = 0);
            Timers (Natural ((Offset - 16#100#) / 16#20#)) := Value or Stuck_Enable;
            if Change_Unrelated then
               Timers (Natural ((Offset - 16#100#) / 16#20#)) := Value xor 16#40#;
            end if;
         end if;
      end;
      procedure Pause is null;
      package Clock is new HPET_Clock (Read32, Read64, Write32, Pause);
      use type Clock.Startup_Status;
      OK : Boolean;
      Before : Natural;
      Saved_Status : Clock.Startup_Status;
      Saved_Offset, Saved_Before, Saved_After : Unsigned_32;
   begin
      pragma Assert (Clock.Status = Clock.Not_Attempted);
      pragma Assert (Clock.Microseconds = Unsigned_64'Last);
      Clock.Initialize (4, OK);
      pragma Assert (OK = (Ignore_Write = 0 and Moving and (Stuck_Enable and 4) = 0));
      pragma Assert (Clock.Available = OK);
      pragma Assert (Clock.Status =
        (if Ignore_Write = 1 then Clock.Disable_Not_Confirmed
         elsif (Stuck_Enable and 4) /= 0 then Clock.Timer_Mask_Not_Confirmed
         elsif Ignore_Write in 2 .. Count + 1 then Clock.Timer_Mask_Not_Confirmed
         elsif Ignore_Write = Count + 2 then Clock.Enable_Not_Confirmed
         elsif Moving then Clock.Running else Clock.Counter_Stalled));
      if OK then pragma Assert (Clock.Microseconds = 2);
      else pragma Assert (Clock.Microseconds = Unsigned_64'Last); end if;
      if not OK and Ignore_Write /= 1 then pragma Assert ((Config and 1) = 0); end if;
      Before := Writes;
      Saved_Status := Clock.Status;
      Saved_Offset := Clock.Timer_Offset;
      Saved_Before := Clock.Timer_Before;
      Saved_After := Clock.Timer_After;
      if Ignore_Write = 1 then
         pragma Assert (Saved_Offset = 0 and Saved_Before = 0 and Saved_After = 0);
      else
         pragma Assert (Saved_Before = Initial_Timer);
         pragma Assert (Saved_Offset = 16#100# + 16#20# *
           Unsigned_32 (if (Stuck_Enable and 4) /= 0 then 0
                        elsif Ignore_Write in 2 .. Count + 1 then Ignore_Write - 2
                        else Count - 1));
         pragma Assert (Saved_After =
           (if Ignore_Write in 2 .. Count + 1 then 16#4004#
            elsif Change_Unrelated then 16#40#
            else (Initial_Timer and not Unsigned_32'(16#4004#)) or Stuck_Enable));
      end if;
      Clock.Initialize (4, OK);
      pragma Assert (not OK and Writes = Before);
      pragma Assert (Clock.Status = Saved_Status);
      pragma Assert (Clock.Timer_Offset = Saved_Offset and
        Clock.Timer_Before = Saved_Before and Clock.Timer_After = Saved_After);
   end Run;
begin
   for Fault in Fault_Kind loop
      if Fault in Bad_Timer | Bad_Timer_Readback then
         for Index in 0 .. 31 loop Fault_Run (Fault, Index); end loop;
      else Fault_Run (Fault); end if;
   end loop;
   for Count in 1 .. 32 loop
      Run (Count, 0, True);
      Run (Count, 0, True, True);
      Run (Count, 0, True, Stuck_Enable => 4);
      Run (Count, 0, True, Stuck_Enable => 16#4000#);
      Run (Count, 0, True, Stuck_Enable => 16#4004#);
      Run (Count, 0, True, Stuck_Enable => 16#4000#,
           Initial_Timer => 16#C000#);
      Run (Count, 0, False);
      for Write in 1 .. Count + 2 loop Run (Count, Write, True); end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS: HPET startup stages, invalid reads, regressions, all comparator counts and ignored writes");
end Clock_Tests;
