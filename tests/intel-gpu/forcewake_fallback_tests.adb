with Interfaces; use Interfaces;
with Intel_GPU_Forcewake_Fallback;
with Intel_GPU_Forcewake;
with Ada.Text_IO;
procedure Forcewake_Fallback_Tests is
   procedure Run (Expected : Unsigned_32; Fault : Natural) is
      Clock : Unsigned_64 := 10;
      Ack : Unsigned_32 := 1 - Expected;
      Sets, Clears : Natural := 0;
      function Now return Unsigned_64 is
        (if Fault = 8 then Unsigned_64'Last else Clock);
      function Read (Offset : Unsigned_32) return Unsigned_32 is
      begin
         pragma Assert (Offset = 16#130044#);
         if Fault = 7 then Clock := Clock + 100_000; end if;
         return (if Fault = 4 then Unsigned_32'Last else Ack);
      end Read;
      procedure Write (Offset, Value : Unsigned_32) is
      begin
         pragma Assert (Offset = 16#A188#);
         if Value = 16#80008000# then
            Sets := Sets + 1;
            if Fault /= 2 then Ack := Ack or 16#8000#; end if;
            if Fault /= 1 then Ack := (Ack and not 1) or Expected; end if;
         else
            pragma Assert (Value = 16#80000000#);
            Clears := Clears + 1;
            if Fault /= 3 then Ack := Ack and not 16#8000#; end if;
            if Fault = 9 then Ack := 1 - Expected; end if;
         end if;
      end Write;
      procedure Pause is
      begin
         if Fault = 6 then Clock := 0;
         elsif Fault /= 5 then Clock := Clock + 1;
         end if;
      end Pause;
      package Recovery is new Intel_GPU_Forcewake_Fallback (Read, Write, Now, Pause);
      Status : Recovery.Result;
      use type Recovery.Result;
   begin
      Recovery.Recover (16#A188#, 16#130044#, Expected, 150, Status);
      pragma Assert (Sets = Clears);
      pragma Assert ((Status = Recovery.Recovered) = (Fault = 0));
      if Fault = 0 then
         pragma Assert (Clock >= 10 and Sets = 1 and (Ack and 16#8001#) = Expected);
      elsif Fault in 1 | 9 then
         pragma Assert (Status = Recovery.Ack_Unchanged and Sets = 10 and Clock >= 550);
      elsif Fault in 2 | 3 | 5 then
         pragma Assert (Status = Recovery.Poll_Exhausted);
      elsif Fault = 4 then
         pragma Assert (Status = Recovery.Invalid_MMIO and Sets = 0);
      elsif Fault = 7 then
         pragma Assert (Status = Recovery.Timed_Out);
      else
         pragma Assert (Status = Recovery.Invalid_Clock);
         if Fault = 8 then pragma Assert (Sets = 0); end if;
      end if;
   end Run;
   procedure Composed is
      Clock : Unsigned_64 := 0;
      Ack, Target : Unsigned_32 := 0;
      Calls : Natural := 0;
      function Now return Unsigned_64 is (Clock);
      function Milliseconds return Unsigned_64 is (Clock / 1000);
      function Read (Offset : Unsigned_32) return Unsigned_32 is
      begin pragma Assert (Offset = 2); return Ack; end Read;
      procedure Write (Offset, Value : Unsigned_32) is
      begin
         pragma Assert (Offset = 1);
         case Value is
            when 16#10001# => Target := 1; -- Suppressed original ACK.
            when 16#10000# => Target := 0;
            when 16#80008000# => Ack := 16#8000# or Target;
            when 16#80000000# => Ack := Ack and 1;
            when others => pragma Assert (False);
         end case;
      end Write;
      procedure Pause is
      begin Clock := Clock + 1; end Pause;
      package Recovery is new Intel_GPU_Forcewake_Fallback (Read, Write, Now, Pause);
      procedure Recover (Expected : Unsigned_32; Recovered : in out Boolean) is
         Status : Recovery.Result;
         use type Recovery.Result;
      begin
         Calls := Calls + 1;
         Recovery.Recover (1, 2, Expected, 100, Status);
         Recovered := Status = Recovery.Recovered;
      end Recover;
      package FW is new Intel_GPU_Forcewake (Read, Write, Pause, Milliseconds, 1, 2, Recover);
      Object : FW.Lease;
      Status : FW.Result;
      use type FW.Result;
      use type FW.Ownership_State;
   begin
      FW.Acquire (Object, 2, Status);
      pragma Assert (Status = FW.Ready and Calls = 1 and FW.State (Object) = FW.Held);
      FW.Release (Object, 2, Status);
      pragma Assert (Status = FW.Ready and Calls = 2 and FW.State (Object) = FW.Idle);
   end Composed;
   procedure Guard (Fault : Natural) is
      Clock : Unsigned_64 := 10;
      Calls, Writes : Natural := 0;
      function Now return Unsigned_64 is (Clock);
      function Read (Offset : Unsigned_32) return Unsigned_32 is
      begin
         pragma Assert (Offset = 2);
         if Fault = 1 then Clock := 9; end if;
         return (if Fault = 0 then Unsigned_32'Last else 1);
      end Read;
      procedure Write (Offset, Value : Unsigned_32) is
      begin
         pragma Assert (Offset = 1 and Value = 16#10000#);
         Writes := Writes + 1;
      end Write;
      procedure Pause is null;
      procedure Recover (Expected : Unsigned_32; Recovered : in out Boolean) is
      begin
         pragma Assert (Expected = 0 and not Recovered);
         Calls := Calls + 1;
         -- Deliberately offer success for invalid-MMIO/clock cases: the
         -- normal lease must never invoke this callback in those cases.
         Recovered := Fault /= 2;
      end Recover;
      package FW is new Intel_GPU_Forcewake (Read, Write, Pause, Now, 1, 2, Recover);
      Object : FW.Lease;
      Status : FW.Result;
      use type FW.Result;
      use type FW.Ownership_State;
   begin
      FW.Acquire (Object, 1, Status);
      pragma Assert (Status = (case Fault is
        when 0 => FW.Invalid_MMIO, when 1 => FW.Invalid_Clock,
        when others => FW.Poll_Exhausted));
      pragma Assert (Calls = (if Fault = 2 then 1 else 0));
      pragma Assert (FW.State (Object) = FW.Faulted and Writes = 0);
      FW.Acquire (Object, 1, Status);
      pragma Assert (Status = FW.Invalid_State);
      pragma Assert (Calls = (if Fault = 2 then 1 else 0));
   end Guard;
begin
   for Expected in Unsigned_32 range 0 .. 1 loop
      for Fault in 0 .. 9 loop Run (Expected, Fault); end loop;
   end loop;
   Composed;
   for Fault in 0 .. 2 loop Guard (Fault); end loop;
   Ada.Text_IO.Put_Line ("PASS: 20 fallback cases, composed acquire/release, 3 recovery guards");
end Forcewake_Fallback_Tests;
