with Interfaces; use Interfaces;

--  Read-only SCSI/Bulk-Only Transport wire boundary. No raw CDB constructor
--  is exported: every command this package can produce is non-mutating.
--  Hardware, DMA lifetime, timeout/recovery and endpoint authority belong to
--  the transport adapter, not this pure codec.
package USB_Optical with SPARK_Mode => On is
   type Bytes is array (Positive range <>) of Unsigned_8;
   subtype Logical_Unit is Unsigned_8 range 0 .. 15;
   subtype Read_Block_Count is Unsigned_16 range 1 .. 16;
   Optical_Block_Bytes : constant Unsigned_32 := 2048;
   subtype Command_Wrapper is Bytes (1 .. 31);

   type Command_Kind is
     (Test_Unit_Ready, Inquiry, Request_Sense, Read_Capacity, Read_Blocks);
   subtype Probe_Kind is Command_Kind range Test_Unit_Ready .. Read_Capacity;
   type Command is private;

   function Probe (Kind : Probe_Kind) return Command;
   function Read_Request
     (First_Block : Unsigned_32; Count : Read_Block_Count) return Command;
   function Transfer_Bytes (Item : Command) return Unsigned_32;
   function Read_Fits
     (First_Block : Unsigned_32; Count : Read_Block_Count;
      Media_Blocks : Unsigned_64) return Boolean;
   function Encode
     (Item : Command; Tag : Unsigned_32; LUN : Logical_Unit)
      return Command_Wrapper
     with Post => Encode'Result (16) in
       16#00# | 16#03# | 16#12# | 16#25# | 16#28#;

   type Status_Result is
     (Command_Passed, Command_Failed, Short_Data,
      Invalid_Status, Reset_Recovery_Required);
   --  Received is the byte count reported by the controller, never inferred
   --  from the device's CSW. A successful command must explain all requested
   --  data; a short successful response is not a successful block read.
   function Decode_Status
     (Data : Bytes; Expected_Tag, Expected_Bytes, Received : Unsigned_32)
      return Status_Result;

   function Is_Optical_Inquiry (Data : Bytes) return Boolean;
   type Capacity_Result is
     (Capacity_Valid, Capacity_Truncated, Unsupported_Block_Size,
      Capacity_16_Required);
   procedure Decode_Capacity
     (Data : Bytes; Block_Count : out Unsigned_64;
      Result : out Capacity_Result)
     with Post =>
       (if Result = Capacity_Valid then
           Block_Count in 1 .. Unsigned_64 (Unsigned_32'Last)
        else Block_Count = 0);

   type Sense_Result is
     (Sense_Invalid, Sense_Other, Sense_Not_Ready, Sense_Unit_Attention);
   function Decode_Sense (Data : Bytes) return Sense_Result;
   --  Only a STALL on GET MAX LUN implies a single unit. A timeout or a
   --  malformed successful response must not silently select LUN zero.
   type Control_Result is (Control_Success, Control_Stall, Control_Error);
   procedure Decode_Max_LUN
     (Outcome : Control_Result; Data : Bytes;
      Last_LUN : out Logical_Unit; Valid : out Boolean);

private
   type Command is record
      Kind : Command_Kind := Test_Unit_Ready;
      First_Block : Unsigned_32 := 0;
      Count : Read_Block_Count := 1;
   end record;
end USB_Optical;
