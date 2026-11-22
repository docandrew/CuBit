with HPET_Counter;
package body HPET_Clock is
   use Interfaces;
   Attempted, Ready : Boolean := False;
   State : Startup_Status := Not_Attempted;
   function Status return Startup_Status is (State);
   Saved_Offset, Saved_Before, Saved_After : Unsigned_32 := 0;
   function Timer_Offset return Unsigned_32 is (Saved_Offset);
   function Timer_Before return Unsigned_32 is (Saved_Before);
   function Timer_After return Unsigned_32 is (Saved_After);
   Period : HPET_Counter.Tick_Period := 1;
   Epoch : Unsigned_64 := 0;
   function Available return Boolean is (Ready);
   procedure Initialize (Poll_Limit : Positive; Success : out Boolean) is
      ID, Rate, Config, Quiet, Value, Offset : Unsigned_32;
      First, Stamp : Unsigned_64;
      procedure Disable is
      begin Write32 (16#10#, Quiet); end Disable;
   begin
      Success := False;
      if Attempted then return; end if;
      Attempted := True;
      State := Identity_Rejected;
      ID := Read32 (0); Rate := Read32 (4);
      if not HPET_Counter.Admitted (ID, Rate) then return; end if;
      State := Configuration_Unreadable;
      Config := Read32 (16#10#);
      if Config = Unsigned_32'Last then return; end if;
      Quiet := Config and not Unsigned_32'(3);
      State := Disable_Not_Confirmed;
      Disable;
      if Read32 (16#10#) /= Quiet then return; end if;
      for Timer in 0 .. HPET_Counter.Timer_Count (ID) - 1 loop
         Offset := 16#100# + Unsigned_32 (Timer) * 16#20#;
         State := Timer_Unreadable;
         Value := Read32 (Offset);
         if Value = Unsigned_32'Last then return; end if;
         State := Timer_Mask_Not_Confirmed;
         Saved_Offset := Offset;
         Saved_Before := Value;
         Write32 (Offset, HPET_Counter.Quiet_Timer (Value));
         Saved_After := Read32 (Offset);
         -- INT_ENB (bit 2) gates interrupts for either routing choice.
         -- FSB_EN (bit 14) selects routing, and is read-only 1 on Intel
         -- timers 4..7. Requiring it clear rejects valid counter-only use.
         -- Keep all-ones rejection and require the actual interrupt gate off.
         if Saved_After = Unsigned_32'Last or else
           (Saved_After and 4) /= 0 then return; end if;
      end loop;
      State := Counter_Unreadable;
      First := Read64 (16#F0#);
      if First = Unsigned_64'Last then return; end if;
      State := Enable_Not_Confirmed;
      Write32 (16#10#, HPET_Counter.Counter_Only (Quiet));
      if Read32 (16#10#) /= HPET_Counter.Counter_Only (Quiet) then Disable; return; end if;
      for Poll in 1 .. Poll_Limit loop
         State := Counter_Invalid;
         Stamp := Read64 (16#F0#);
         if Stamp = Unsigned_64'Last or Stamp < First then Disable; return; end if;
         if Stamp > First then
            Period := HPET_Counter.Tick_Period (Rate);
            Epoch := First;
            Ready := True;
            State := Running;
            Success := True;
            return;
         end if;
         if Poll < Poll_Limit then Pause; end if;
      end loop;
      State := Counter_Stalled;
      Disable;
   end Initialize;
   function Microseconds return Unsigned_64 is
      Stamp : Unsigned_64;
   begin
      if not Ready then return Unsigned_64'Last; end if;
      Stamp := Read64 (16#F0#);
      if Stamp = Unsigned_64'Last or Stamp < Epoch then return Unsigned_64'Last; end if;
      return HPET_Counter.Microseconds (Stamp - Epoch, Period);
   end Microseconds;
end HPET_Clock;
