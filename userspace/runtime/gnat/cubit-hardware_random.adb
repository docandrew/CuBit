------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;

package body CuBit.Hardware_Random is

   Word_Bytes : constant := 8;
   Byte_Bits : constant := 8;

   procedure Word (Value : out Unsigned_64; OK : out Boolean) is
      Carry : Unsigned_8;
   begin
      Value := 0;
      OK := False;
      for Attempt in 1 .. Attempts loop
         Asm ("rdrand %0" & ASCII.LF & "setc %1",
              Outputs => [Unsigned_64'Asm_Output ("=r", Value),
                          Unsigned_8'Asm_Output ("=qm", Carry)],
              Volatile => True);
         if Carry /= 0 then
            OK := True;
            return;
         end if;
      end loop;
   end Word;

   procedure Fill (Data : out Byte_Array; OK : out Boolean) is
      Value : Unsigned_64 := 0;
      Left : Natural := 0;
   begin
      Data := [others => 0];
      OK := True;
      for I in Data'Range loop
         if Left = 0 then
            Word (Value, OK);
            if not OK then
               Data := [others => 0];
               return;
            end if;
            Left := Word_Bytes;
         end if;
         Data (I) := Unsigned_8 (Value and 16#FF#);
         Value := Shift_Right (Value, Byte_Bits);
         Left := Left - 1;
      end loop;
   end Fill;

end CuBit.Hardware_Random;
