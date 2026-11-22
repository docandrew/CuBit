--  Byte-exact SCSI fixtures (SPC-4 / SBC-3 tables) for every command block
--  and decoder in package SCSI.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with SCSI; use SCSI;

procedure Main is
   Item : Command_Block;
   Inquiry_Result : Inquiry_Data;
   Capacity : Capacity_Data;
   Sense : Sense_Data;
   Status : Status_Code;
   Valid, Needs_16 : Boolean;
   Empty : constant Bytes (1 .. 0) := [];
   Checks : Natural := 0;

   procedure Expect (Condition : Boolean) is
   begin
      pragma Assert (Condition);
      Checks := Checks + 1;
   end Expect;
begin
   Item := Test_Unit_Ready;
   Expect (Wire (Item) = [0, 0, 0, 0, 0, 0] and Item.Direction = No_Data);
   Item := Inquiry (36);
   Expect (Wire (Item) = [16#12#, 0, 0, 0, 36, 0] and Item.Expected = 36);
   Item := Inquiry (16#0102#);
   Expect (Wire (Item) = [16#12#, 0, 0, 1, 2, 0]);
   Item := Request_Sense (18);
   Expect (Wire (Item) = [16#03#, 0, 0, 0, 18, 0] and Item.Expected = 18);
   Item := Read_Capacity_10;
   Expect (Wire (Item) = [16#25#, 0, 0, 0, 0, 0, 0, 0, 0, 0]
           and Item.Expected = 8 and Item.Direction = From_Device);
   Item := Read_Capacity_16;
   Expect (Wire (Item) =
             [16#9E#, 16#10#, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 0, 0]
           and Item.Expected = 32);
   Item := Read_10 (16#1234_5678#, 16, 512);
   Expect (Wire (Item) =
             [16#28#, 0, 16#12#, 16#34#, 16#56#, 16#78#, 0, 0, 16, 0]
           and Item.Expected = 8192 and Item.Data (11 .. 16) = [0, 0, 0, 0, 0, 0]);
   Item := Write_10 (1, 16#0203#, 4096, Force_Unit_Access => True);
   Expect (Wire (Item) = [16#2A#, 16#08#, 0, 0, 0, 1, 0, 2, 3, 0]
           and Item.Direction = To_Device and Item.Expected = 16#0203# * 4096);
   Item := Read_16 (16#0102_0304_0506_0708#, 16#000B_0C0D#, 512);
   Expect (Wire (Item) =
             [16#88#, 0, 1, 2, 3, 4, 5, 6, 7, 8, 0, 16#0B#, 16#0C#,
              16#0D#, 0, 0]);
   Item := Write_16 (Unsigned_64'Last, 1, 4096, Force_Unit_Access => False);
   Expect (Wire (Item) =
             [16#8A#, 0, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 1,
              0, 0] and Item.Expected = 4096);
   Item := Write_16 (0, 1, 512, Force_Unit_Access => True);
   Expect (Item.Data (2) = 16#08#);
   Item := Synchronize_Cache;
   Expect (Wire (Item) = [16#35#, 0, 0, 0, 0, 0, 0, 0, 0, 0]
           and Item.Expected = 0 and Item.Direction = No_Data);
   Expect (Fits (16#FFFF#, 65_536) and not Fits (16#1_0000#, 65_536));

   --  Status bytes: exactly the SAM-5 codes.
   for Value in Unsigned_8 loop
      Decode_Status (Value, Status, Valid);
      pragma Assert
        (Valid = (Value in 0 | 2 | 4 | 8 | 16#18# | 16#28# | 16#30# | 16#40#));
   end loop;
   Decode_Status (2, Status, Valid);
   Expect (Valid and Status = Check_Condition);

   --  INQUIRY.
   declare
      Data : Bytes (1 .. 36) := [others => 0];
   begin
      Data (1) := 16#20#;  --  qualifier 1, direct access
      Data (2) := 16#80#;
      Data (3) := 6;
      Data (5) := 31;
      Data (9 .. 16) := [Character'Pos ('S'), Character'Pos ('A'),
                         Character'Pos ('M'), Character'Pos ('S'),
                         Character'Pos ('U'), Character'Pos ('N'),
                         Character'Pos ('G'), 1];
      Data (17 .. 32) := [others => Character'Pos ('K')];
      Data (33 .. 36) := [Character'Pos ('0'), Character'Pos ('1'),
                          Character'Pos ('0'), Character'Pos ('2')];
      Decode_Inquiry (Data, Inquiry_Result, Valid);
      Expect (Valid and Inquiry_Result.Qualifier = 1
              and Inquiry_Result.Device_Type = Direct_Access_Device
              and Inquiry_Result.Removable and Inquiry_Result.Version = 6
              and Inquiry_Result.Vendor = "SAMSUNG?"
              and Inquiry_Result.Product = "KKKKKKKKKKKKKKKK"
              and Inquiry_Result.Revision = "0102");
      Data (1) := 5;
      Decode_Inquiry (Data & Unsigned_8'(0), Inquiry_Result, Valid);
      Expect (Valid and Inquiry_Result.Device_Type = MMC_Device
              and Inquiry_Result.Qualifier = Unit_Connected);
      Data (5) := 30;
      Decode_Inquiry (Data, Inquiry_Result, Valid);
      Expect (not Valid);
      Decode_Inquiry (Data (1 .. 35), Inquiry_Result, Valid);
      Expect (not Valid);
   end;

   --  READ CAPACITY(10) and (16).
   Decode_Capacity_10 ([0, 0, 0, 99, 0, 0, 2, 0], Capacity, Valid, Needs_16);
   Expect (Valid and not Needs_16 and Capacity.Last_Block = 99
           and Capacity.Length = 512);
   Decode_Capacity_10 ([255, 255, 255, 255, 0, 0, 2, 0], Capacity, Valid,
                       Needs_16);
   Expect (not Valid and Needs_16);
   Decode_Capacity_10 ([0, 0, 0, 99, 0, 0, 2], Capacity, Valid, Needs_16);
   Expect (not Valid and not Needs_16);
   declare
      Data : Bytes (1 .. 32) := [others => 0];
   begin
      Data (1 .. 8) := [0, 0, 0, 1, 0, 0, 0, 0];
      Data (9 .. 12) := [0, 0, 16#10#, 0];
      Data (14) := 16#F3#;
      Decode_Capacity_16 (Data, Capacity, Valid);
      Expect (Valid and Capacity.Last_Block = 16#1_0000_0000#
              and Capacity.Length = 4096
              and Capacity.Blocks_Per_Physical_Exponent = 3);
      Decode_Capacity_16 (Data (1 .. 13), Capacity, Valid);
      Expect (not Valid);
   end;

   --  Sense data, both formats, with origin-independent decoding.
   declare
      Fixed : Bytes (11 .. 28) := [others => 0];
   begin
      Fixed (11) := 16#F0#;     --  valid bit set, current fixed
      Fixed (13) := 16#05#;
      Fixed (18) := 10;
      Fixed (23) := 16#24#;
      Fixed (24) := 16#01#;
      Decode_Sense (Fixed, Sense, Valid);
      Expect (Valid and Sense.Format = Fixed_Current
              and Sense.Key = Illegal_Request and Sense.Code = 16#24#
              and Sense.Qualifier = 1);
      Fixed (18) := 9;
      Decode_Sense (Fixed, Sense, Valid);
      Expect (not Valid);
      Fixed (18) := 10;
      Decode_Sense (Fixed (11 .. 27), Sense, Valid);
      Expect (not Valid);
      Fixed (11) := 16#71#;
      Fixed (13) := 16#06#;
      Decode_Sense (Fixed, Sense, Valid);
      Expect (Valid and Sense.Format = Fixed_Deferred
              and Sense.Key = Unit_Attention);
   end;
   Decode_Sense ([16#72#, 16#02#, 16#3A#, 0, 0, 0, 0, 0], Sense, Valid);
   Expect (Valid and Sense.Format = Descriptor_Current
           and Sense.Key = Not_Ready and Sense.Code = 16#3A#);
   Decode_Sense ([16#72#, 16#02#, 16#3A#, 0, 0, 0, 0], Sense, Valid);
   Expect (not Valid);
   Decode_Sense ([16#7F#, 0, 0, 0, 0, 0, 0, 0], Sense, Valid);
   Expect (not Valid);
   Decode_Sense (Empty, Sense, Valid);
   Expect (not Valid);
   for Key in Unsigned_8 range 0 .. 15 loop
      Decode_Sense ([16#73#, Key or 16#F0#, 0, 0, 0, 0, 0, 0], Sense, Valid);
      pragma Assert (Valid and Sense_Key'Enum_Rep (Sense.Key) = Integer (Key));
   end loop;

   Ada.Text_IO.Put_Line ("SCSI-CHECK: PASS (" & Natural'Image (Checks) &
                         " fixtures)");
end Main;
