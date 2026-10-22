package body Metadata_Owner_Support is
   use AML_Decode;
   use type Firmware_Tables.Byte;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Result;
   end Enc;
   function Block (Opcode, Payload : Bytes) return Bytes is
      Length : Natural := Payload'Length + 1;
   begin
      if Length <= 63 then return Opcode & [Byte (Length)] & Payload; end if;
      Length := Payload'Length + 2;
      if Length > 4095 then raise Program_Error with "fixture package bound"; end if;
      return Opcode & [Byte (16#40# + Length mod 16), Byte (Length / 16)] & Payload;
   end Block;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Code));
   function Pressure return Bytes is
      Buffer_Size : constant := AML_Decode.Max_Buffer_Length;
      Repetitions : constant Positive := 24;
      Buffer_Code : constant Bytes := Block ([16#11#],
        [16#0B#, Byte (Buffer_Size mod 256), Byte (Buffer_Size / 256), 16#41#]);
      Statement : constant Bytes := Bytes'[16#70#] & Buffer_Code & [16#61#];
      Result : Bytes (1 .. Repetitions * Statement'Length);
   begin
      for I in 0 .. Repetitions - 1 loop
         Result (I * Statement'Length + 1 .. (I + 1) * Statement'Length) := Statement;
      end loop;
      return Result;
   end Pressure;
   function Fixture return Bytes is
     (Block ([16#5B#, 16#82#], Enc ("DEV0")) &
      Method ("CAPT", Bytes'[16#9D#] & Enc ("DEV0") & [16#60#, 16#A4#, 16#60#]) &
      Method ("PRES", Pressure & [16#A4#, 0]) &
      Method ("LOCL", Bytes'[16#9D#] & Enc ("DEV0") & [16#60#] & Enc ("PRES") &
        [16#A4#, 16#8E#, 16#60#]) &
      Method ("REFR", Bytes'[16#A4#, 16#71#] & Enc ("DEV0")) &
      Method ("REPL", Bytes'[16#9D#, 16#0A#, 42] & Enc ("DEV0") & [16#A4#, 1]));
   function Table (Code : Bytes; Revision : Positive) return Firmware_Tables.Bytes is
      Result : Firmware_Tables.Bytes (1 .. 36 + Code'Length) := [others => 0];
      Sum : Firmware_Tables.Byte := 0;
   begin
      if Revision not in 1 .. 2 or else Result'Length > 65_535 then
         raise Program_Error with "fixture table bound";
      end if;
      Result (1 .. 4) := [Character'Pos ('D'), Character'Pos ('S'), Character'Pos ('D'), Character'Pos ('T')];
      Result (5) := Firmware_Tables.Byte (Result'Length mod 256);
      Result (6) := Firmware_Tables.Byte (Result'Length / 256);
      Result (9) := Firmware_Tables.Byte (Revision);
      for I in Code'Range loop Result (36 + I - Code'First + 1) := Firmware_Tables.Byte (Code (I)); end loop;
      for Item of Result loop Sum := Sum + Item; end loop;
      Result (10) := 0 - Sum;
      return Result;
   end Table;
end Metadata_Owner_Support;
