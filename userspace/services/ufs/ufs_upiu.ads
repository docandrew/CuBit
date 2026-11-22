------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  UFS transport wire formats: UFS Protocol Information Units (JESD220
--  UFS 3.1/4.0 section 10) and the host controller's request descriptors
--  (UFSHCI 6.1: UTP transfer and task-management request descriptors and
--  physical region descriptors). Pure codec, no DMA or MMIO.
--
--  Every frame starts at index 1; multi-byte UPIU fields are big-endian,
--  descriptor words little-endian (the controller's native order).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with SCSI;
with UFSHCI; use UFSHCI;

package UFS_UPIU with SPARK_Mode, Pure is
   subtype Bytes is SCSI.Bytes;

   ---------------------------------------------------------------------------
   --  UPIU transaction codes (UFS 10.5).
   ---------------------------------------------------------------------------
   type Transaction_Code is
     (NOP_Out, Command, Data_Out, Task_Management_Request, Query_Request,
      NOP_In, Response, Data_In, Task_Management_Response,
      Ready_To_Transfer, Query_Response, Reject);
   for Transaction_Code use
     (NOP_Out                  => 16#00#,
      Command                  => 16#01#,
      Data_Out                 => 16#02#,
      Task_Management_Request  => 16#04#,
      Query_Request            => 16#16#,
      NOP_In                   => 16#20#,
      Response                 => 16#21#,
      Data_In                  => 16#22#,
      Task_Management_Response => 16#24#,
      Ready_To_Transfer        => 16#31#,
      Query_Response           => 16#36#,
      Reject                   => 16#3F#);

   --  Every request this driver sends is one 32-byte UPIU.
   Request_Bytes : constant := 32;
   subtype Request_Frame is Bytes (1 .. Request_Bytes);
   --  Responses: a 32-byte UPIU and its data segment.
   Response_Header_Bytes : constant := 32;

   --  A task tag names one outstanding request: this driver uses the
   --  transfer slot.
   subtype Task_Tag is Unsigned_8 range 0 .. Maximum_Transfer_Slots - 1;

   --  LUN field: bit 7 marks a well-known logical unit (UFS 10.6.5).
   type Logical_Unit is new Unsigned_8;
   Report_LUNs_Unit : constant Logical_Unit := 16#81#;
   Device_Unit      : constant Logical_Unit := 16#D0#;
   --  Ordinary units are numbered 0 .. bMaxNumberLU - 1 (at most 32).
   subtype User_Unit is Logical_Unit range 0 .. 31;

   ---------------------------------------------------------------------------
   --  Requests.
   ---------------------------------------------------------------------------
   function NOP_Out (Tag : Task_Tag) return Request_Frame;

   --  A SCSI command to Unit. No data-out segment: write data travels
   --  through the request's physical region descriptors.
   function Command (Tag : Task_Tag; Unit : Logical_Unit;
                     Item : SCSI.Command_Block) return Request_Frame
     with Pre => SCSI.Well_Formed (Item);

   --  Query requests (UFS 10.7.8).
   type Query_Opcode is
     (Query_NOP, Read_Descriptor, Write_Descriptor, Read_Attribute,
      Write_Attribute, Read_Flag, Set_Flag, Clear_Flag, Toggle_Flag);
   for Query_Opcode use
     (Query_NOP => 0, Read_Descriptor => 1, Write_Descriptor => 2,
      Read_Attribute => 3, Write_Attribute => 4, Read_Flag => 5,
      Set_Flag => 6, Clear_Flag => 7, Toggle_Flag => 8);

   type Descriptor_Kind is
     (Device_Descriptor, Configuration_Descriptor, Unit_Descriptor,
      Interconnect_Descriptor, String_Descriptor, Geometry_Descriptor,
      Power_Descriptor, Health_Descriptor);
   for Descriptor_Kind use
     (Device_Descriptor => 0, Configuration_Descriptor => 1,
      Unit_Descriptor => 2, Interconnect_Descriptor => 4,
      String_Descriptor => 5, Geometry_Descriptor => 7,
      Power_Descriptor => 8, Health_Descriptor => 9);

   type Device_Flag is (Device_Init);
   for Device_Flag use (Device_Init => 1);

   --  The longest descriptor a query returns in one data segment.
   Maximum_Descriptor_Bytes : constant := 255;
   subtype Descriptor_Length is Unsigned_16 range 1 .. Maximum_Descriptor_Bytes;
   subtype Descriptor_Index is Unsigned_8;

   function Read_Descriptor
     (Tag : Task_Tag; Kind : Descriptor_Kind; Index : Descriptor_Index;
      Length : Descriptor_Length) return Request_Frame;

   subtype Flag_Opcode is Query_Opcode range Read_Flag .. Clear_Flag;
   function Flag_Request
     (Tag : Task_Tag; Opcode : Flag_Opcode; Flag : Device_Flag)
      return Request_Frame;

   ---------------------------------------------------------------------------
   --  Responses.
   ---------------------------------------------------------------------------
   --  The NOP IN that answers NOP_Out (Tag).
   function Valid_NOP_In (Data : Bytes; Tag : Task_Tag) return Boolean;

   --  The UPIU response field (UFS 10.7.2.3).
   Target_Success : constant Unsigned_8 := 0;

   Maximum_Sense_Bytes : constant := 18;
   type Command_Result is record
      Target_Response : Unsigned_8 := Target_Success;
      Status          : SCSI.Status_Code := SCSI.Good;
      --  Bytes the device did not transfer.
      Residual        : Unsigned_32 := 0;
      Sense_Present   : Boolean := False;
      Sense           : SCSI.Sense_Data;
   end record;

   --  Valid: a RESPONSE UPIU for Tag whose data segment fits Data, with a
   --  known status byte. Sense is decoded when the segment carries it.
   procedure Decode_Command_Response
     (Data : Bytes; Tag : Task_Tag; Result : out Command_Result;
      Valid : out Boolean);

   --  Query response codes (UFS 10.7.9.3): success is zero.
   Query_Success : constant Unsigned_8 := 0;

   type Query_Result is record
      Response_Code : Unsigned_8 := Query_Success;
      --  Data segment (descriptor bytes) within the response.
      Segment_First : Positive := 1;
      Segment_Bytes : Natural := 0;
      Flag_Value    : Boolean := False;
   end record;

   --  Valid: a QUERY RESPONSE for Tag echoing Opcode and identifier
   --  Identifier, with its whole data segment inside Data.
   procedure Decode_Query_Response
     (Data : Bytes; Tag : Task_Tag; Opcode : Query_Opcode;
      Identifier : Unsigned_8; Result : out Query_Result; Valid : out Boolean)
     with Post =>
       (if Valid then
          Result.Segment_Bytes <= Maximum_Descriptor_Bytes
          and then (Result.Segment_Bytes = 0
                    or else (Result.Segment_First >= Data'First
                             and then Result.Segment_First - Data'First
                                        + Result.Segment_Bytes
                                      <= Data'Length)));

   ---------------------------------------------------------------------------
   --  UTP transfer request descriptor (UFSHCI 6.1.1): eight 32-bit words.
   ---------------------------------------------------------------------------
   type Descriptor_Word_Index is range 0 .. 7;
   type Descriptor_Words is array (Descriptor_Word_Index) of Register_Value;

   type Transfer_Direction is (No_Transfer, To_Device, From_Device);

   --  Overall command status (UFSHCI 6.1.1, OCS).
   type Command_Status is
     (Success, Invalid_Command_Table, Invalid_PRDT, Data_Buffer_Mismatch,
      Response_Size_Mismatch, Peer_Communication_Failure, Aborted,
      Fatal_Error, Device_Fatal_Error, Invalid_Crypto_Configuration,
      General_Crypto_Error, Invalid_Status);
   for Command_Status use
     (Success => 0, Invalid_Command_Table => 1, Invalid_PRDT => 2,
      Data_Buffer_Mismatch => 3, Response_Size_Mismatch => 4,
      Peer_Communication_Failure => 5, Aborted => 6, Fatal_Error => 7,
      Device_Fatal_Error => 8, Invalid_Crypto_Configuration => 9,
      General_Crypto_Error => 10, Invalid_Status => 16#0F#);

   --  Physical addresses the controller reads: the command descriptor is
   --  128-byte aligned (UFSHCI 6.1.1 UCDBA).
   Command_Descriptor_Alignment : constant := 128;
   subtype Physical_Address is Unsigned_64;
   --  Offsets and lengths inside a command descriptor, in 32-bit words.
   Word_Bytes : constant := 4;
   subtype Descriptor_Offset is Unsigned_16;

   --  The largest physical region one descriptor entry covers.
   Maximum_Region_Bytes : constant := 256 * 1024;
   --  Entries per request this driver uses.
   Maximum_Regions : constant := 32;
   subtype Region_Count is Natural range 0 .. Maximum_Regions;

   function Transfer_Descriptor
     (Direction        : Transfer_Direction;
      Command_Table    : Physical_Address;
      Response_Offset  : Descriptor_Offset;
      Response_Words   : Descriptor_Offset;
      Regions_Offset   : Descriptor_Offset;
      Regions          : Region_Count) return Descriptor_Words
     with Pre => Command_Table mod Command_Descriptor_Alignment = 0;

   --  The controller's completion status in a descriptor it returned.
   function Status_Of (Words : Descriptor_Words) return Command_Status;

   --  One physical region descriptor: four words.
   type Region_Word_Index is range 0 .. 3;
   type Region_Words is array (Region_Word_Index) of Register_Value;
   subtype Region_Bytes is Positive range Word_Bytes .. Maximum_Region_Bytes;

   function Region
     (Address : Physical_Address; Size : Region_Bytes) return Region_Words
     with Pre => Address mod Word_Bytes = 0 and then Size mod Word_Bytes = 0;

   ---------------------------------------------------------------------------
   --  Task management (UFS 10.7.6, UFSHCI 6.2): 80-byte descriptors.
   ---------------------------------------------------------------------------
   type Task_Function is
     (Abort_Task, Abort_Task_Set, Clear_Task_Set, Logical_Unit_Reset,
      Query_Task, Query_Task_Set);
   for Task_Function use
     (Abort_Task => 16#01#, Abort_Task_Set => 16#02#,
      Clear_Task_Set => 16#04#, Logical_Unit_Reset => 16#08#,
      Query_Task => 16#80#, Query_Task_Set => 16#81#);

   Task_Descriptor_Bytes : constant := 80;
   subtype Task_Descriptor is Bytes (1 .. Task_Descriptor_Bytes);

   --  Asks the device to abort the command Target on Unit. The request
   --  itself is tagged Tag.
   function Abort_Request
     (Tag : Task_Tag; Unit : Logical_Unit; Target : Task_Tag)
      return Task_Descriptor;

   --  Service responses (UFS 10.7.7): the abort happened or the task was
   --  already gone.
   type Task_Outcome is (Abort_Completed, Abort_Failed, Invalid_Task_Response);
   function Decode_Task_Response (Data : Task_Descriptor; Tag : Task_Tag)
     return Task_Outcome;
end UFS_UPIU;
