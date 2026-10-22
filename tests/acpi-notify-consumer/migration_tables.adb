package body Migration_Tables is
   use type Firmware_Tables.Byte;
   function Make (Payload : Firmware_Tables.Bytes; Secondary : Boolean := False)
     return Firmware_Tables.Bytes is
      Data : Firmware_Tables.Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Length : Natural := Data'Length;
      Sum : Firmware_Tables.Byte := 0;
   begin
      Data (1 .. 4) := (if Secondary then [83, 83, 68, 84] else [68, 83, 68, 84]);
      for I in 5 .. 8 loop Data (I) := Firmware_Tables.Byte (Length mod 256); Length := Length / 256; end loop;
      Data (9) := 2; Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Make;
end Migration_Tables;
