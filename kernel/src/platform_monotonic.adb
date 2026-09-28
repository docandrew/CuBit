with HPET_Clock;
with System.Storage_Elements;
with System.Machine_Code;
package body Platform_Monotonic is
   use Interfaces;
   use System;
   use System.Storage_Elements;
   Registers : Address := Null_Address;
   Attempted : Boolean := False;
   Saved_ID, Saved_Period : Unsigned_32 := 0;
   function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => Registers + Storage_Offset (Offset);
   begin return Value; end Read32;
   function Read64 (Offset : Unsigned_32) return Unsigned_64 is
      Value : Unsigned_64 with Import, Volatile_Full_Access,
        Address => Registers + Storage_Offset (Offset);
   begin return Value; end Read64;
   procedure Write32 (Offset, Value : Unsigned_32) is
      Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => Registers + Storage_Offset (Offset);
   begin Register_Value := Value; end Write32;
   procedure Pause is
   begin System.Machine_Code.Asm ("pause", Volatile => True); end Pause;
   package Counter is new HPET_Clock (Read32, Read64, Write32, Pause);
   function Startup_Diagnostic return String is
     (if not Attempted then "not-initialized"
      elsif Registers = Null_Address then "invalid-register-base"
      else Counter.Status'Image);
   function Diagnostic (Detail : Unsigned_64) return Unsigned_64 is
   begin
      case Detail is
         when 0 =>
            return (if not Attempted then 0
                    elsif Registers = Null_Address then 1
                    else 2 + Counter.Startup_Status'Pos (Counter.Status));
         when 1 => return Unsigned_64 (Saved_ID);
         when 2 => return Unsigned_64 (Saved_Period);
         when 3 => return Unsigned_64 (Counter.Timer_Offset);
         when 4 => return Unsigned_64 (Counter.Timer_Before);
         when 5 => return Unsigned_64 (Counter.Timer_After);
         when others => return Unsigned_64'Last;
      end case;
   end Diagnostic;
   procedure Initialize_HPET (Base : Address; Success : out Boolean) is
   begin
      Success := False;
      if Attempted then return; end if;
      Attempted := True;
      if Base = Null_Address or else To_Integer (Base) mod 8 /= 0 then return; end if;
      Registers := Base;
      Saved_ID := Read32 (0);
      Saved_Period := Read32 (4);
      Counter.Initialize (100_000, Success);
   end Initialize_HPET;
   procedure Read (Microseconds : out Unsigned_64; Success : out Boolean) is
   begin
      Microseconds := Counter.Microseconds;
      Success := Microseconds /= Unsigned_64'Last;
      if not Success then Microseconds := 0; end if;
   end Read;
end Platform_Monotonic;
