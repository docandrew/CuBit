------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Process_IDs with SPARK_Mode is

   procedure Image (Process : Process_ID; Text : out Image_Text; Last : out Positive) is
      Buffer : Image_Text := [others => ' '];
      Position : Positive := Buffer'Last;
      Value : Process_ID := Process;
   begin
      loop
         Buffer (Position) := Character'Val (Character'Pos ('0') + Natural (Value mod 10));
         Value := Value / 10;
         exit when Value = 0 or else Position = Buffer'First;
         Position := Position - 1;
      end loop;
      --  Left-aligned.
      Last := Buffer'Last - Position + 1;
      Text := [others => ' '];
      Text (1 .. Last) := Buffer (Position .. Buffer'Last);
   end Image;

   procedure Parse (Text : String; Process : out Process_ID; Valid : out Boolean) is
      Value : Process_ID := 0;
      Digit : Process_ID;
   begin
      Process := No_Process;
      Valid := False;
      if Text'Length = 0 then
         return;
      end if;
      for C of Text loop
         if C not in '0' .. '9' then
            return;
         end if;
         Digit := Process_ID (Character'Pos (C) - Character'Pos ('0'));
         if Value > (Process_ID'Last - Digit) / 10 then
            return;
         end if;
         Value := Value * 10 + Digit;
      end loop;
      Process := Value;
      Valid := True;
   end Parse;

end CuBit.Process_IDs;
