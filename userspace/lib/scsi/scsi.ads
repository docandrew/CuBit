------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Typed SCSI command blocks and response decoding (SPC-4, SBC-3), shared by
--  every SCSI transport: USB Bulk-Only (usb_optical), UFS (SCSI over UPIU)
--  and later USB mass storage and UAS (HW-001 step 2).
--
--  Pure wire codec: no transport, DMA or timeout policy. Every command
--  block this package builds is one of the constructors below; there is no
--  raw CDB constructor. Multi-byte CDB and parameter fields are big-endian.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package SCSI with SPARK_Mode, Pure is
   type Bytes is array (Positive range <>) of Unsigned_8;

   ---------------------------------------------------------------------------
   --  Operation codes (SPC-4 / SBC-3).
   ---------------------------------------------------------------------------
   type Operation_Code is
     (Test_Unit_Ready, Request_Sense, Inquiry, Read_Capacity_10, Read_10,
      Write_10, Synchronize_Cache_10, Read_16, Write_16,
      Service_Action_In_16);
   for Operation_Code use
     (Test_Unit_Ready      => 16#00#,
      Request_Sense        => 16#03#,
      Inquiry              => 16#12#,
      Read_Capacity_10     => 16#25#,
      Read_10              => 16#28#,
      Write_10             => 16#2A#,
      Synchronize_Cache_10 => 16#35#,
      Read_16              => 16#88#,
      Write_16             => 16#8A#,
      Service_Action_In_16 => 16#9E#);

   function Code (Operation : Operation_Code) return Unsigned_8 is
     (Unsigned_8 (Operation_Code'Enum_Rep (Operation)));

   --  SERVICE ACTION IN(16) service action for READ CAPACITY(16).
   Read_Capacity_16_Action : constant Unsigned_8 := 16#10#;

   ---------------------------------------------------------------------------
   --  Command blocks.
   ---------------------------------------------------------------------------
   type CDB_Size is (CDB_6, CDB_10, CDB_16);
   for CDB_Size use (CDB_6 => 6, CDB_10 => 10, CDB_16 => 16);
   Largest_CDB : constant := 16;
   subtype CDB_Index is Positive range 1 .. Largest_CDB;
   subtype CDB_Bytes is Bytes (CDB_Index);

   function Length (Size : CDB_Size) return CDB_Index is
     (CDB_Size'Enum_Rep (Size));

   type Data_Direction is (No_Data, From_Device, To_Device);

   --  A logical block length a block device may report (SBC: at least 512;
   --  CuBit's block protocol stops at 64 KiB).
   subtype Block_Length is Unsigned_32 range 512 .. 65_536;
   --  Bytes one command moves: the data-transfer length every transport
   --  carries in 32 bits.
   subtype Transfer_Length is Unsigned_32;

   type Command_Block is record
      Operation : Operation_Code := Test_Unit_Ready;
      Size      : CDB_Size := CDB_6;
      --  Bytes past Length (Size) are zero.
      Data      : CDB_Bytes := [others => 0];
      Direction : Data_Direction := No_Data;
      Expected  : Transfer_Length := 0;
   end record;

   --  The command's CDB as sent: Length (Size) bytes.
   function Wire (Item : Command_Block) return Bytes is
     (Item.Data (1 .. Length (Item.Size)))
     with Post => Wire'Result'First = 1
                  and then Wire'Result'Length = Length (Item.Size);

   --  Well-formed: the opcode byte, length and tail agree with the record.
   function Well_Formed (Item : Command_Block) return Boolean is
     (Item.Data (1) = Code (Item.Operation)
      and then (for all Index in Length (Item.Size) + 1 .. Largest_CDB =>
                  Item.Data (Index) = 0)
      and then (Item.Direction = No_Data) = (Item.Expected = 0));

   subtype Allocation_8 is Unsigned_8 range 1 .. Unsigned_8'Last;
   subtype Allocation_16 is Unsigned_16 range 1 .. Unsigned_16'Last;
   --  The standard INQUIRY data this codec decodes (SPC-4 6.6.2).
   Standard_Inquiry_Bytes : constant := 36;
   --  Fixed-format sense data with the additional fields (SPC-4 4.5.3).
   Fixed_Sense_Bytes : constant := 18;
   Capacity_10_Bytes : constant := 8;
   Capacity_16_Bytes : constant := 32;

   function Test_Unit_Ready return Command_Block
     with Post => Well_Formed (Test_Unit_Ready'Result)
                  and then Test_Unit_Ready'Result.Operation = Test_Unit_Ready
                  and then Test_Unit_Ready'Result.Size = CDB_6
                  and then Test_Unit_Ready'Result.Direction = No_Data;

   --  Standard INQUIRY data (no vital product data page).
   function Inquiry (Allocation : Allocation_16) return Command_Block
     with Post => Well_Formed (Inquiry'Result)
                  and then Inquiry'Result.Operation = Inquiry
                  and then Inquiry'Result.Size = CDB_6
                  and then Inquiry'Result.Direction = From_Device
                  and then Inquiry'Result.Expected = Transfer_Length (Allocation);

   --  Fixed-format sense data.
   function Request_Sense (Allocation : Allocation_8) return Command_Block
     with Post => Well_Formed (Request_Sense'Result)
                  and then Request_Sense'Result.Operation = Request_Sense
                  and then Request_Sense'Result.Size = CDB_6
                  and then Request_Sense'Result.Direction = From_Device
                  and then Request_Sense'Result.Expected =
                    Transfer_Length (Allocation);

   function Read_Capacity_10 return Command_Block
     with Post => Well_Formed (Read_Capacity_10'Result)
                  and then Read_Capacity_10'Result.Operation = Read_Capacity_10
                  and then Read_Capacity_10'Result.Size = CDB_10
                  and then Read_Capacity_10'Result.Direction = From_Device
                  and then Read_Capacity_10'Result.Expected = Capacity_10_Bytes;

   function Read_Capacity_16 return Command_Block
     with Post => Well_Formed (Read_Capacity_16'Result)
                  and then Read_Capacity_16'Result.Operation =
                    Service_Action_In_16
                  and then Read_Capacity_16'Result.Size = CDB_16
                  and then Read_Capacity_16'Result.Direction = From_Device
                  and then Read_Capacity_16'Result.Data (2) =
                    Read_Capacity_16_Action
                  and then Read_Capacity_16'Result.Expected = Capacity_16_Bytes;

   --  Whether Count blocks of Size bytes fit one command's transfer length.
   function Fits (Count : Unsigned_32; Size : Block_Length) return Boolean is
     (Unsigned_64 (Count) * Unsigned_64 (Size) <=
        Unsigned_64 (Transfer_Length'Last));

   subtype Block_Count_10 is Unsigned_16 range 1 .. Unsigned_16'Last;
   subtype Block_Count_16 is Unsigned_32 range 1 .. Unsigned_32'Last;

   --  READ and WRITE. Force_Unit_Access: the write is durable on completion.
   function Read_10
     (First : Unsigned_32; Count : Block_Count_10; Size : Block_Length)
      return Command_Block
     with Pre  => Fits (Unsigned_32 (Count), Size),
          Post => Well_Formed (Read_10'Result)
                  and then Read_10'Result.Operation = Read_10
                  and then Read_10'Result.Size = CDB_10
                  and then Read_10'Result.Direction = From_Device
                  and then Read_10'Result.Expected =
                    Transfer_Length (Count) * Size;
   function Write_10
     (First : Unsigned_32; Count : Block_Count_10; Size : Block_Length;
      Force_Unit_Access : Boolean) return Command_Block
     with Pre  => Fits (Unsigned_32 (Count), Size),
          Post => Well_Formed (Write_10'Result)
                  and then Write_10'Result.Operation = Write_10
                  and then Write_10'Result.Size = CDB_10
                  and then Write_10'Result.Direction = To_Device
                  and then Write_10'Result.Expected =
                    Transfer_Length (Count) * Size;
   function Read_16
     (First : Unsigned_64; Count : Block_Count_16; Size : Block_Length)
      return Command_Block
     with Pre  => Fits (Count, Size),
          Post => Well_Formed (Read_16'Result)
                  and then Read_16'Result.Operation = Read_16
                  and then Read_16'Result.Size = CDB_16
                  and then Read_16'Result.Direction = From_Device
                  and then Read_16'Result.Expected = Count * Size;
   function Write_16
     (First : Unsigned_64; Count : Block_Count_16; Size : Block_Length;
      Force_Unit_Access : Boolean) return Command_Block
     with Pre  => Fits (Count, Size),
          Post => Well_Formed (Write_16'Result)
                  and then Write_16'Result.Operation = Write_16
                  and then Write_16'Result.Size = CDB_16
                  and then Write_16'Result.Direction = To_Device
                  and then Write_16'Result.Expected = Count * Size;

   --  Commits the whole medium's volatile cache (LBA 0, zero blocks).
   function Synchronize_Cache return Command_Block
     with Post => Well_Formed (Synchronize_Cache'Result)
                  and then Synchronize_Cache'Result.Operation =
                    Synchronize_Cache_10
                  and then Synchronize_Cache'Result.Direction = No_Data;

   ---------------------------------------------------------------------------
   --  Status (SAM-5): the byte a transport returns with a completed command.
   ---------------------------------------------------------------------------
   type Status_Code is
     (Good, Check_Condition, Condition_Met, Busy, Reservation_Conflict,
      Task_Set_Full, ACA_Active, Task_Aborted);
   for Status_Code use
     (Good                 => 16#00#,
      Check_Condition      => 16#02#,
      Condition_Met        => 16#04#,
      Busy                 => 16#08#,
      Reservation_Conflict => 16#18#,
      Task_Set_Full        => 16#28#,
      ACA_Active           => 16#30#,
      Task_Aborted         => 16#40#);

   procedure Decode_Status
     (Value : Unsigned_8; Status : out Status_Code; Valid : out Boolean)
     with Post => (if Valid then Unsigned_8 (Status_Code'Enum_Rep (Status)) = Value);

   ---------------------------------------------------------------------------
   --  INQUIRY data.
   ---------------------------------------------------------------------------
   type Peripheral_Device_Type is mod 2 ** 5;
   Direct_Access_Device : constant Peripheral_Device_Type := 16#00#;
   MMC_Device           : constant Peripheral_Device_Type := 16#05#;
   Well_Known_Unit      : constant Peripheral_Device_Type := 16#1E#;

   type Peripheral_Qualifier is mod 2 ** 3;
   --  The device type is connected at this unit.
   Unit_Connected : constant Peripheral_Qualifier := 0;

   subtype Vendor_Text is String (1 .. 8);
   subtype Product_Text is String (1 .. 16);
   subtype Revision_Text is String (1 .. 4);

   type Inquiry_Data is record
      Qualifier   : Peripheral_Qualifier := 0;
      Device_Type : Peripheral_Device_Type := 0;
      Removable   : Boolean := False;
      Version     : Unsigned_8 := 0;
      Vendor      : Vendor_Text := [others => ' '];
      Product     : Product_Text := [others => ' '];
      Revision    : Revision_Text := [others => ' '];
   end record;

   --  Valid: the standard 36 bytes are present and the additional length
   --  covers them. Text fields are printable ASCII; other bytes become '?'.
   procedure Decode_Inquiry
     (Data : Bytes; Result : out Inquiry_Data; Valid : out Boolean);

   ---------------------------------------------------------------------------
   --  READ CAPACITY.
   ---------------------------------------------------------------------------
   --  READ CAPACITY(10)'s last-LBA value when the medium needs (16).
   Capacity_10_Overflow : constant Unsigned_32 := Unsigned_32'Last;

   type Capacity_Data is record
      Last_Block : Unsigned_64 := 0;
      Length     : Unsigned_32 := 0;
      --  Logical blocks per physical block, as a power of two (16 only).
      Blocks_Per_Physical_Exponent : Unsigned_8 := 0;
   end record;

   --  Valid: exactly the response's bytes; the length is the raw value.
   --  Needs_16: the medium has more blocks than (10) can report.
   procedure Decode_Capacity_10
     (Data : Bytes; Result : out Capacity_Data;
      Valid : out Boolean; Needs_16 : out Boolean)
     with Post => (if Valid then
                     Result.Last_Block < Unsigned_64 (Capacity_10_Overflow)
                     and then not Needs_16);
   procedure Decode_Capacity_16
     (Data : Bytes; Result : out Capacity_Data; Valid : out Boolean);

   ---------------------------------------------------------------------------
   --  Sense data.
   ---------------------------------------------------------------------------
   type Sense_Key is
     (No_Sense, Recovered_Error, Not_Ready, Medium_Error, Hardware_Error,
      Illegal_Request, Unit_Attention, Data_Protect, Blank_Check,
      Vendor_Specific, Copy_Aborted, Aborted_Command, Obsolete_Key,
      Volume_Overflow, Miscompare, Completed);
   for Sense_Key use
     (No_Sense => 0, Recovered_Error => 1, Not_Ready => 2, Medium_Error => 3,
      Hardware_Error => 4, Illegal_Request => 5, Unit_Attention => 6,
      Data_Protect => 7, Blank_Check => 8, Vendor_Specific => 9,
      Copy_Aborted => 10, Aborted_Command => 11, Obsolete_Key => 12,
      Volume_Overflow => 13, Miscompare => 14, Completed => 15);

   --  Response codes (SPC-4 4.5.1), with the valid bit masked off.
   type Sense_Format is (Fixed_Current, Fixed_Deferred,
                         Descriptor_Current, Descriptor_Deferred);
   for Sense_Format use
     (Fixed_Current => 16#70#, Fixed_Deferred => 16#71#,
      Descriptor_Current => 16#72#, Descriptor_Deferred => 16#73#);

   type Sense_Data is record
      Format : Sense_Format := Fixed_Current;
      Key    : Sense_Key := No_Sense;
      --  Additional sense code and qualifier.
      Code       : Unsigned_8 := 0;
      Qualifier  : Unsigned_8 := 0;
   end record;

   --  Fixed format needs all 18 bytes and an additional length of at least
   --  ten; descriptor format needs its 8-byte header.
   Descriptor_Sense_Header_Bytes : constant := 8;
   procedure Decode_Sense
     (Data : Bytes; Result : out Sense_Data; Valid : out Boolean);
end SCSI;
