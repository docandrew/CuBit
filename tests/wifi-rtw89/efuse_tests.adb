--  Efuse logical-map decoding (RTW89_Efuse) against synthetic physical maps.
pragma Ada_2022;
with Interfaces; use Interfaces;
with RTW89_Efuse; use RTW89_Efuse;
with Test_Support; use Test_Support;

procedure Efuse_Tests is
   Physical : Physical_Map;
   Logical  : Logical_Map;
   Records  : Record_Count;
   Status   : Decode_Status;
   Station  : constant MAC_Address := [16#00#, 16#E0#, 16#4C#, 16#88#, 16#52#, 16#B0#];

   --  A record header for Block with the given word-enable nibble.
   procedure Put_Header (At_Index : Physical_Index; Block : Natural; Absent : Unsigned_8) is
   begin
      Physical (At_Index) := Unsigned_8 (Block / 16) or 16#30#;
      Physical (At_Index + 1) := Shift_Left (Unsigned_8 (Block mod 16), 4) or Absent;
   end Put_Header;
begin
   --  Blank efuse: nothing decoded, no usable address.
   Physical := [others => Unprogrammed];
   Decode (Physical, Logical, Records, Status);
   Check (Status = Decoded and then Records = 0, "blank: decoded, no records");
   Check ((for all B of Logical => B = Unprogrammed), "blank: logical all 0xFF");
   Check (not Usable (MAC (Logical)), "blank: MAC unusable");

   --  MAC at logical 0x400 = block 0x80, words 0..2 present, word 3 absent.
   Physical := [others => Unprogrammed];
   Put_Header (Security_Control_Size, PCIe_MAC_Offset / Block_Bytes, 2#1000#);
   for I in MAC_Index loop
      Physical (Security_Control_Size + Header_Bytes + I) := Station (I);
   end loop;
   --  Second record: block 0x59 (0x2C8..0x2CF), only word 1 (0x2CA, 0x2CB)
   --  present: RFE type 0x41 and the first country letter.
   Put_Header (Security_Control_Size + Header_Bytes + MAC_Bytes, 16#59#, 2#1101#);
   Physical (Security_Control_Size + 2 * Header_Bytes + MAC_Bytes) := 16#41#;
   Physical (Security_Control_Size + 2 * Header_Bytes + MAC_Bytes + 1) := Character'Pos ('U');
   Decode (Physical, Logical, Records, Status);
   Check (Status = Decoded and then Records = 2, "mac: two records");
   Check (MAC (Logical) = Station, "mac: address at 0x400");
   Check (Usable (MAC (Logical)), "mac: usable");
   Check (Logical (PCIe_MAC_Offset + MAC_Bytes) = Unprogrammed, "mac: absent word stays 0xFF");
   Check (Logical (RFE_Type_Offset) = 16#41#, "mac: RFE type");
   Check (Logical (Country_Offset) = Character'Pos ('U'), "mac: country first byte");
   Check (Logical (Country_Offset + 1) = Unprogrammed, "mac: country second byte absent");

   --  Group or zero addresses are refused.
   Check (not Usable ([16#01#, 0, 0, 0, 0, 1]), "group address refused");
   Check (not Usable ([others => 0]), "zero address refused");

   --  Records of block 0 with all four words, back to back, run into the
   --  end of the record area: the last record's word 3 would pass it.
   Physical := [others => 0];
   Decode (Physical, Logical, Records, Status);
   Check (Status = Word_Past_Physical, "overrun: refused at the record area's end");
   Check (Records = (Physical_Size - 2 * Security_Control_Size) / (Header_Bytes + Block_Bytes) + 1,
          "overrun: every record counted");

   --  Highest block (0xFF): its last word ends at logical 2047, accepted.
   Physical := [others => Unprogrammed];
   Physical (Security_Control_Size) := 16#3F#;
   Physical (Security_Control_Size + 1) := 16#F7#;  --  block 0xFF, only word 3
   Physical (Security_Control_Size + 2) := 16#AB#;
   Physical (Security_Control_Size + 3) := 16#CD#;
   Decode (Physical, Logical, Records, Status);
   Check (Status = Decoded and then Records = 1, "top block: decoded");
   Check (Logical (Logical_Size - 2) = 16#AB# and then Logical (Logical_Size - 1) = 16#CD#,
          "top block: last word lands at 2046..2047");
end Efuse_Tests;
