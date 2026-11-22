pragma Ada_2022;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with TextIO; use TextIO;
with x86;
with Debug_Exceptions;
with User_Access_Ranges;
with Process.User_Memory;
with PerCPUData;
with Config;

package body Debug_Register_Probe is

   Armed_Reported : Boolean := False with Atomic;

   -- Which CPUs hold an armed breakpoint: only those write DR7 at a switch.
   -- (Writing DR7 at every switch hung a one-CPU KVM guest during boot.)
   Armed : array (0 .. Config.MAX_CPUS - 1) of Boolean := [others => False]
     with Volatile_Components;

   -- Text is Prefix and Hex_Digits lowercase hex digits: Value.
   procedure Parse
     (Text, Prefix : String; Value : out Unsigned_64; Valid : out Boolean)
   is
      First : constant Natural := Text'First + Prefix'Length;
      Nibble : Unsigned_64;
   begin
      Value := 0;
      Valid := False;
      if Text'Length /= Prefix'Length + Hex_Digits
        or else Text (Text'First .. First - 1) /= Prefix
      then
         return;
      end if;
      for C of Text (First .. Text'Last) loop
         case C is
            when '0' .. '9' => Nibble := Character'Pos (C) - Character'Pos ('0');
            when 'a' .. 'f' => Nibble := Character'Pos (C) - Character'Pos ('a') + 10;
            when others => return;
         end case;
         Value := Shift_Left (Value, 4) or Nibble;
      end loop;
      Valid := True;
   end Parse;

   procedure Disarm_Local is
      CPU : constant Natural := PerCPUData.getCPUNumber;
   begin
      if CPU in Armed'Range and then Armed (CPU) then
         Armed (CPU) := False;
         x86.setDR7 (0);
      end if;
   end Disarm_Local;

   procedure Handle_Request
     (Caller : Process.ProcessID; Text : String; Handled : out Boolean)
   is
      Value : Unsigned_64;
   begin
      Parse (Text, Arm_Prefix, Value, Handled);
      if Handled then
         -- A user address only: a breakpoint never names kernel memory.
         if User_Access_Ranges.Within_User (Value, 1) then
            declare
               CPU : constant Natural := PerCPUData.getCPUNumber;
            begin
               if CPU in Armed'Range then
                  Armed (CPU) := True;
                  x86.setDR6 (x86.DR6_CLEAR);
                  x86.setDR0 (Value);
                  x86.setDR7 (Data_Watch_DR7);
               end if;
            end;
            if not Armed_Reported then
               Armed_Reported := True;
               println ("DBGK-PROBE: armed");
            end if;
         end if;
         return;
      end if;
      Parse (Text, Count_Prefix, Value, Handled);
      if Handled then
         declare
            Count : aliased constant Unsigned_64 := Debug_Exceptions.Kernel_Count;
            Stored : Boolean;
         begin
            Process.User_Memory.Copy_To_User
              (Caller, Value, Count'Address, Count'Size / 8, Stored);
            if not Stored then
               println ("DBGK-PROBE: count not stored");
            end if;
         end;
      end if;
   end Handle_Request;
end Debug_Register_Probe;
