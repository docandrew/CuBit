--  Byte-exact fixtures for the UFS driver's pure layers: UFSHCI register
--  decoders (JESD223), UPIU frames and request descriptors (JESD220,
--  UFSHCI 6.1/6.2), descriptor decoders and the GPT reader.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with SCSI; use type SCSI.Bytes, SCSI.Status_Code, SCSI.Sense_Key;
with UFSHCI; use UFSHCI;
with UFS_UPIU; use UFS_UPIU;
with UFS_Descriptors; use UFS_Descriptors;
with Partition_Tables;
with UFS_Text;
with UFS_Layout;
with UFS_Quirks;

procedure Main is
   Checks : Natural := 0;
   procedure Expect (Condition : Boolean) is
   begin
      if not Condition then
         --  Log text, DMA layout and the Intel quirk table.
   Expect (UFS_Text.Decimal (0) = "0" and UFS_Text.Decimal (1234) = "1234"
           and UFS_Text.Decimal (Unsigned_64'Last) = "18446744073709551615");
   Expect (UFS_Text.Hex (16#54FF#, 4) = "54ff" and UFS_Text.Hex (5, 2) = "05");
   Expect (UFS_Text.Size (4096 * 1024) = "4096K"
           and UFS_Text.Size (260 * 2 ** 20) = "260M"
           and UFS_Text.Size (118 * 2 ** 30) = "118G"
           and UFS_Text.Size (Unsigned_64'Last) = "16777215T");
   Expect (Partition_Tables.Type_Name (Partition_Tables.EFI_System) = "esp"
           and Partition_Tables.Type_Name ([others => 1]) = "other");
   Expect (UFS_Layout.Command_Table_At (1) = 16#1800#
           and UFS_Layout.Buffer_At (7) = 16#80000#
           and UFS_Layout.Transfer_Descriptor_At (2) = 64);
   Expect (UFS_Quirks.Disables_TX_LCC (16#8086#, 16#54FF#)
           and not UFS_Quirks.Disables_TX_LCC (16#1B36#, 16#0013#));

   Ada.Text_IO.Put_Line ("UFS-CODEC-CHECK: FAIL at check" &
                               Natural'Image (Checks + 1));
      end if;
      pragma Assert (Condition);
      Checks := Checks + 1;
   end Expect;

   Frame : Request_Frame;
begin
   --  CAP: 32 transfer slots, 8 task slots, 64-bit addressing.
   declare
      Caps : constant Controller_Capabilities :=
        Decode_Capabilities (16#0107_001F#);
      Small : constant Controller_Capabilities := Decode_Capabilities (0);
   begin
      Expect (Caps.Transfer_Slots = 32 and Caps.Task_Slots = 8
              and Caps.Addressing_64 and not Caps.Auto_Hibernate);
      Expect (Small.Transfer_Slots = 1 and Small.Task_Slots = 1
              and not Small.Addressing_64);
   end;
   Expect (Decode_Version (16#0310#) = (Major => 3, Minor => 1, Suffix => 0));
   Expect (Offset (Transfer_Doorbell) = 16#58# and Offset (UIC_Command) = 16#90#);
   Expect (Attribute_Argument (PA_Local_TX_LCC_Enable) = 16#155E_0000#);
   Expect (Command_Word (DME_Link_Startup) = 16#16#);

   --  NOP OUT, COMMAND (READ(10) of 8 blocks of 4 KiB), queries.
   Frame := NOP_Out (5);
   Expect (Frame (1) = 0 and Frame (4) = 5 and Frame (5 .. 32) = [5 .. 32 => 0]);
   Frame := Command (3, 0, SCSI.Read_10 (16#0102_0304#, 8, 4096));
   Expect (Frame (1 .. 12) = [16#01#, 16#40#, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0]);
   Expect (Frame (13 .. 16) = [0, 0, 16#80#, 0]);
   Expect (Frame (17 .. 26) = [16#28#, 0, 1, 2, 3, 4, 0, 0, 8, 0]
           and Frame (27 .. 32) = [27 .. 32 => 0]);
   Frame := Command (1, Device_Unit, SCSI.Test_Unit_Ready);
   Expect (Frame (2) = 0 and Frame (3) = 16#D0# and Frame (13 .. 16) = [0, 0, 0, 0]);
   Frame := Read_Descriptor (2, Geometry_Descriptor, 0, 16#FF#);
   Expect (Frame (1) = 16#16# and Frame (6) = 16#01# and Frame (13) = 1
           and Frame (14) = 7 and Frame (15) = 0 and Frame (19 .. 20) = [0, 16#FF#]);
   Frame := Flag_Request (4, Set_Flag, Device_Init);
   Expect (Frame (6) = 16#81# and Frame (13) = 6 and Frame (14) = 1);
   Frame := Flag_Request (4, Read_Flag, Device_Init);
   Expect (Frame (6) = 16#01# and Frame (13) = 5);

   --  NOP IN.
   declare
      Reply : SCSI.Bytes (1 .. 32) := [others => 0];
   begin
      Reply (1) := 16#20#;
      Reply (4) := 5;
      Expect (Valid_NOP_In (Reply, 5) and not Valid_NOP_In (Reply, 4));
      Reply (7) := 1;
      Expect (not Valid_NOP_In (Reply, 5));
      Expect (not Valid_NOP_In (Reply (1 .. 31), 5));
   end;

   --  RESPONSE with CHECK CONDITION and fixed sense (UNIT ATTENTION).
   declare
      Reply : SCSI.Bytes (101 .. 164) := [others => 0];
      Result : Command_Result;
      Valid : Boolean;
   begin
      Reply (101) := 16#21#;
      Reply (104) := 3;
      Reply (108) := 16#02#;
      Reply (111 .. 112) := [0, 20];
      Reply (113 .. 116) := [0, 0, 16#10#, 0];
      Reply (133 .. 134) := [0, 18];
      Reply (135) := 16#70#;
      Reply (137) := 16#06#;
      Reply (142) := 10;
      Reply (147) := 16#29#;
      Decode_Command_Response (Reply, 3, Result, Valid);
      Expect (Valid and Result.Status = SCSI.Check_Condition
              and Result.Residual = 4096 and Result.Sense_Present
              and Result.Sense.Key = SCSI.Unit_Attention
              and Result.Sense.Code = 16#29#);
      Decode_Command_Response (Reply, 2, Result, Valid);
      Expect (not Valid);
      Reply (111 .. 112) := [0, 33];  --  segment past the buffer
      Decode_Command_Response (Reply, 3, Result, Valid);
      Expect (not Valid);
      Reply (111 .. 112) := [0, 0];
      Reply (108) := 16#01#;          --  not a SAM status
      Decode_Command_Response (Reply, 3, Result, Valid);
      Expect (not Valid);
   end;

   --  QUERY RESPONSE carrying a geometry descriptor.
   declare
      Reply : SCSI.Bytes (1 .. 32 + 16#57#) := [others => 0];
      Result : Query_Result;
      Valid : Boolean;
      Geometry : Geometry_Information;
   begin
      Reply (1) := 16#36#;
      Reply (4) := 2;
      Reply (11 .. 12) := [0, 16#57#];
      Reply (13) := 1;
      Reply (14) := 7;
      Reply (33) := 16#57#;
      Reply (34) := 7;
      Reply (37 .. 44) := [0, 0, 0, 0, 16#0E#, 16#E0#, 0, 0];
      Reply (45) := 32;
      Decode_Query_Response (Reply, 2, Read_Descriptor, 7, Result, Valid);
      Expect (Valid and Result.Response_Code = Query_Success
              and Result.Segment_First = 33 and Result.Segment_Bytes = 16#57#);
      Decode_Geometry
        (Reply (Result.Segment_First ..
                Result.Segment_First + Result.Segment_Bytes - 1),
         Geometry, Valid);
      Expect (Valid and Geometry.Raw_Capacity_Sectors = 16#0EE0_0000#
              and Geometry.Maximum_Units = 32);
      Decode_Query_Response (Reply, 2, Read_Descriptor, 0, Result, Valid);
      Expect (not Valid);
      Decode_Query_Response (Reply (1 .. 100), 2, Read_Descriptor, 7, Result,
                             Valid);
      Expect (not Valid);
   end;

   --  Device, unit and string descriptors.
   declare
      Device : SCSI.Bytes (1 .. 16#59#) := [others => 0];
      Info : Device_Information;
      Unit : SCSI.Bytes (1 .. 16#2D#) := [others => 0];
      Unit_Info : Unit_Information;
      Name : constant SCSI.Bytes :=
        [12, 5, 0, Character'Pos ('U'), 0, Character'Pos ('F'),
         0, Character'Pos ('S'), 16#26#, 16#3A#, 0, Character'Pos ('!')];
      Name_Text : Text;
      Valid : Boolean;
   begin
      Device (1) := 16#59#;
      Device (7) := 2;
      Device (17 .. 18) := [16#03#, 16#10#];
      Device (22) := 4;
      Device (25 .. 26) := [16#01#, 16#CE#];
      Decode_Device (Device, Info, Valid);
      Expect (Valid and Info.Specification = 16#0310# and Info.Logical_Units = 2
              and Info.Product_Name = 4 and Info.Manufacturer = 16#01CE#);
      Device (1) := 16#10#;
      Decode_Device (Device, Info, Valid);
      Expect (not Valid);
      Device (1) := 16#5A#;           --  longer than the bytes we hold
      Decode_Device (Device, Info, Valid);
      Expect (not Valid);

      Unit (1) := 16#2D#;
      Unit (2) := 2;
      Unit (3) := 0;
      Unit (4) := 1;
      Unit (11) := 12;
      Unit (12 .. 19) := [0, 0, 0, 0, 0, 16#1D#, 16#C0#, 0];
      Decode_Unit (Unit, 0, Unit_Info, Valid);
      Expect (Valid and Unit_Info.Enabled and Unit_Info.Block_Exponent = 12
              and Unit_Info.Block_Count = 16#1D_C000#);
      Decode_Unit (Unit, 1, Unit_Info, Valid);
      Expect (not Valid);
      Unit (11) := 8;                 --  256-byte blocks are not carried
      Decode_Unit (Unit, 0, Unit_Info, Valid);
      Expect (not Valid);

      Decode_String (Name, Name_Text, Valid);
      Expect (Valid and Name_Text.Length = 5
              and Name_Text.Value (1 .. 5) = "UFS?!");
      --  NUL padding (QEMU pads its product name) ends the text.
      Decode_String (Name (1 .. 6) & SCSI.Bytes'[0, 0, 0, 0, 0, Character'Pos ('X')],
                     Name_Text, Valid);
      Expect (Valid and Name_Text.Length = 2 and Name_Text.Value (1 .. 2) = "UF");
   end;

   --  Request descriptors.
   declare
      Words : Descriptor_Words :=
        Transfer_Descriptor (From_Device, 16#1_2345_6780#, 128, 32, 256, 2);
      Entry_Words : constant Region_Words := Region (16#2_0000_1000#, 8192);
   begin
      Expect (Words (0) = 16#1500_0000# and Words (2) = 16#0F#
              and Words (4) = 16#2345_6780# and Words (5) = 1
              and Words (6) = 16#0080_0020# and Words (7) = 16#0100_0002#);
      Expect (Status_Of (Words) = Invalid_Status);
      Words (2) := 0;
      Expect (Status_Of (Words) = Success);
      --  Only bits 7:0 are the status; reserved bits are ignored.
      Words (2) := 16#FFFF_FF06#;
      Expect (Status_Of (Words) = Aborted);
      Words (2) := 16#0B#;            --  not an assigned code
      Expect (Status_Of (Words) = Invalid_Status);
      Expect (Transfer_Descriptor (To_Device, 0, 0, 0, 0, 0) (0) = 16#1300_0000#);
      Expect (Entry_Words = [16#0000_1000#, 2, 0, 8191]);
   end;

   --  Task management: ABORT TASK and its response.
   declare
      Request : constant Task_Descriptor := Abort_Request (7, 0, 3);
      Reply : Task_Descriptor := Request;
   begin
      Expect (Request (4) = 1 and Request (9) = 16#0F#
              and Request (17) = 16#04# and Request (20) = 7
              and Request (22) = 1 and Request (36) = 3);
      Reply (9) := 0;
      Reply (49) := 16#24#;
      Reply (52) := 7;
      Expect (Decode_Task_Response (Reply, 7) = Abort_Completed);
      Reply (64) := 16#05#;           --  function not supported
      Expect (Decode_Task_Response (Reply, 7) = Abort_Failed);
      Expect (Decode_Task_Response (Reply, 6) = Invalid_Task_Response);
      Reply (9) := 7;
      Expect (Decode_Task_Response (Reply, 7) = Invalid_Task_Response);
   end;

   --  Partition tables: protective MBR and a GPT header with valid CRC.
   declare
      use Partition_Tables;
      MBR : Sector := [others => 0];
      Header_Block : Partition_Tables.Bytes (1 .. 4096) := [others => 0];
      Header : GPT_Header;
      Result : Header_Result;
      CRC : Unsigned_32;
      Entries : Partition_Tables.Bytes (1 .. 128) := [others => 0];
      Item : Partition_Entry;
   begin
      Expect (Classify_MBR (MBR) = No_Table);
      MBR (511 .. 512) := [16#55#, 16#AA#];
      MBR (447 + 4) := 16#EE#;
      Expect (Classify_MBR (MBR) = Protective_GPT);
      MBR (463 + 4) := 16#07#;
      Expect (Classify_MBR (MBR) = Legacy_MBR);
      --  The standard check value of CRC32 over "123456789".
      Expect (CRC32 ([16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#,
                      16#38#, 16#39#]) = 16#CBF4_3926#);

      Header_Block (1 .. 8) := [16#45#, 16#46#, 16#49#, 16#20#, 16#50#,
                                16#41#, 16#52#, 16#54#];
      Header_Block (9 .. 12) := [0, 0, 1, 0];
      Header_Block (13) := 92;
      Header_Block (25) := 1;                     --  my LBA
      Header_Block (33 .. 34) := [16#FF#, 16#FF#]; --  backup LBA 65535
      Header_Block (41) := 6;                     --  first usable
      Header_Block (49 .. 50) := [16#FA#, 16#FF#];  --  last usable
      Header_Block (73) := 2;                     --  entries at LBA 2
      Header_Block (81) := 128;                   --  128 entries
      Header_Block (85) := 128;                   --  of 128 bytes
      CRC := CRC32 (Header_Block (1 .. 92));
      Header_Block (17 .. 20) :=
        [Unsigned_8 (CRC and 16#FF#), Unsigned_8 (Shift_Right (CRC, 8) and 16#FF#),
         Unsigned_8 (Shift_Right (CRC, 16) and 16#FF#),
         Unsigned_8 (Shift_Right (CRC, 24))];
      Decode_Header (Header_Block, 1, 65_536, Header, Result);
      Expect (Result = Header_Valid and Header.Entries_LBA = 2
              and Header.Entry_Count = 128 and Header.Last_Usable = 16#FFFA#);
      Decode_Header (Header_Block, 2, 65_536, Header, Result);
      Expect (Result = Bad_Location);
      Decode_Header (Header_Block, 1, 65_535, Header, Result);
      Expect (Result = Bad_Location);
      Header_Block (60) := 1;                     --  changes the disk GUID
      Decode_Header (Header_Block, 1, 65_536, Header, Result);
      Expect (Result = Bad_Checksum);
      Header_Block (1) := 0;
      Decode_Header (Header_Block, 1, 65_536, Header, Result);
      Expect (Result = Bad_Signature);

      Entries (1) := 16#AF#;
      Entries (33) := 16#00#;
      Entries (34) := 16#08#;                     --  first LBA 2048
      Entries (41 .. 42) := [16#FF#, 16#0F#];       --  last LBA 4095
      Item := Decode_Entry (Entries, 0);
      Expect (not Is_Unused (Item) and Item.First = 2048 and Item.Last = 4095);
      Header.First_Usable := 34;
      Header.Last_Usable := 65_000;
      Expect (Entry_In_Range (Header, Item));
      Header.Last_Usable := 4000;
      Expect (not Entry_In_Range (Header, Item));
   end;

   --  Log text, DMA layout and the Intel quirk table.
   Expect (UFS_Text.Decimal (0) = "0" and UFS_Text.Decimal (1234) = "1234"
           and UFS_Text.Decimal (Unsigned_64'Last) = "18446744073709551615");
   Expect (UFS_Text.Hex (16#54FF#, 4) = "54ff" and UFS_Text.Hex (5, 2) = "05");
   Expect (UFS_Text.Size (4096 * 1024) = "4096K"
           and UFS_Text.Size (260 * 2 ** 20) = "260M"
           and UFS_Text.Size (118 * 2 ** 30) = "118G"
           and UFS_Text.Size (Unsigned_64'Last) = "16777215T");
   Expect (Partition_Tables.Type_Name (Partition_Tables.EFI_System) = "esp"
           and Partition_Tables.Type_Name ([others => 1]) = "other");
   Expect (UFS_Layout.Command_Table_At (1) = 16#1800#
           and UFS_Layout.Buffer_At (7) = 16#80000#
           and UFS_Layout.Transfer_Descriptor_At (2) = 64);
   Expect (UFS_Quirks.Disables_TX_LCC (16#8086#, 16#54FF#)
           and not UFS_Quirks.Disables_TX_LCC (16#1B36#, 16#0013#));

   Ada.Text_IO.Put_Line ("UFS-CODEC-CHECK: PASS (" & Natural'Image (Checks) &
                         " fixtures)");
end Main;
