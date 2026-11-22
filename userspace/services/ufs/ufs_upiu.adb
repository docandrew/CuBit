------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body UFS_UPIU with SPARK_Mode is
   Byte_Bits : constant := 8;
   Byte_Mask : constant := 16#FF#;

   --  UPIU header fields (UFS 10.6), 1-based.
   Type_Byte           : constant := 1;
   Flags_Byte          : constant := 2;
   LUN_Byte            : constant := 3;
   Tag_Byte            : constant := 4;
   Function_Byte       : constant := 6;   --  query / task function
   Response_Byte       : constant := 7;
   Status_Byte         : constant := 8;
   Segment_Length      : constant := 11;  --  bytes 11 .. 12
   --  Transaction-specific fields start after the 12-byte header.
   Expected_Length     : constant := 13;  --  COMMAND: bytes 13 .. 16
   CDB_First           : constant := 17;  --  COMMAND: bytes 17 .. 32
   Residual_Count      : constant := 13;  --  RESPONSE: bytes 13 .. 16
   Query_Opcode_Byte   : constant := 13;
   Query_Identifier    : constant := 14;
   Query_Index         : constant := 15;
   Query_Length        : constant := 19;  --  bytes 19 .. 20
   Query_Flag_Value    : constant := 24;
   --  The data segment follows the 32-byte UPIU.
   Segment_First       : constant := Response_Header_Bytes + 1;
   --  RESPONSE data segment: sense length (2 bytes), then sense data.
   Sense_Length_Bytes  : constant := 2;

   --  COMMAND flags (UFS 10.7.1.1): R and W; task attribute simple (0).
   Read_Flag_Bit  : constant Unsigned_8 := 16#40#;
   Write_Flag_Bit : constant Unsigned_8 := 16#20#;

   --  Query function (UFS 10.7.8.1).
   Standard_Read_Request  : constant Unsigned_8 := 16#01#;
   Standard_Write_Request : constant Unsigned_8 := 16#81#;

   function Code (Item : Transaction_Code) return Unsigned_8 is
     (Unsigned_8 (Transaction_Code'Enum_Rep (Item)));

   function Header (Kind : Transaction_Code; Tag : Task_Tag)
     return Request_Frame is
     ([Type_Byte => Code (Kind), Tag_Byte => Tag, others => 0]);

   function Big_Endian_16 (High, Low : Unsigned_8) return Natural is
     (Natural (High) * 2 ** Byte_Bits + Natural (Low));

   function NOP_Out (Tag : Task_Tag) return Request_Frame is
     (Header (NOP_Out, Tag));

   function Command (Tag : Task_Tag; Unit : Logical_Unit;
                     Item : SCSI.Command_Block) return Request_Frame
   is
      Frame : Request_Frame := Header (Command, Tag);
   begin
      Frame (Flags_Byte) :=
        (case Item.Direction is
           when SCSI.No_Data     => 0,
           when SCSI.From_Device => Read_Flag_Bit,
           when SCSI.To_Device   => Write_Flag_Bit);
      Frame (LUN_Byte) := Unsigned_8 (Unit);
      for Offset in 0 .. 3 loop
         Frame (Expected_Length + Offset) := Unsigned_8
           (Shift_Right (Item.Expected, (3 - Offset) * Byte_Bits)
            and Byte_Mask);
      end loop;
      for Index in SCSI.CDB_Index loop
         Frame (CDB_First + Index - 1) := Item.Data (Index);
      end loop;
      return Frame;
   end Command;

   function Query
     (Tag : Task_Tag; Opcode : Query_Opcode; Identifier : Unsigned_8)
      return Request_Frame
   is
      Frame : Request_Frame := Header (Query_Request, Tag);
   begin
      Frame (Function_Byte) :=
        (if Opcode in Write_Descriptor | Write_Attribute | Set_Flag
                    | Clear_Flag | Toggle_Flag
         then Standard_Write_Request else Standard_Read_Request);
      Frame (Query_Opcode_Byte) :=
        Unsigned_8 (Query_Opcode'Enum_Rep (Opcode));
      Frame (Query_Identifier) := Identifier;
      return Frame;
   end Query;

   function Read_Descriptor
     (Tag : Task_Tag; Kind : Descriptor_Kind; Index : Descriptor_Index;
      Length : Descriptor_Length) return Request_Frame
   is
      Frame : Request_Frame :=
        Query (Tag, Read_Descriptor,
               Unsigned_8 (Descriptor_Kind'Enum_Rep (Kind)));
   begin
      Frame (Query_Index) := Index;
      Frame (Query_Length) := Unsigned_8 (Shift_Right (Length, Byte_Bits));
      Frame (Query_Length + 1) := Unsigned_8 (Length and Byte_Mask);
      return Frame;
   end Read_Descriptor;

   function Flag_Request
     (Tag : Task_Tag; Opcode : Flag_Opcode; Flag : Device_Flag)
      return Request_Frame is
     (Query (Tag, Opcode, Unsigned_8 (Device_Flag'Enum_Rep (Flag))));

   ---------------------------------------------------------------------------
   --  Responses.
   ---------------------------------------------------------------------------
   function Valid_NOP_In (Data : Bytes; Tag : Task_Tag) return Boolean is
     (Data'Length >= Response_Header_Bytes
      and then Data (Data'First + (Type_Byte - 1)) = Code (NOP_In)
      and then Data (Data'First + (Tag_Byte - 1)) = Tag
      and then Data (Data'First + (Response_Byte - 1)) = Target_Success);

   procedure Decode_Command_Response
     (Data : Bytes; Tag : Task_Tag; Result : out Command_Result;
      Valid : out Boolean)
   is
      Status_Valid : Boolean;
   begin
      Result := (others => <>);
      Valid := False;
      if Data'Length < Response_Header_Bytes then
         return;
      end if;
      declare
         Frame : constant Bytes (1 .. Response_Header_Bytes) :=
           Data (Data'First .. Data'First + (Response_Header_Bytes - 1));
         Segment : constant Natural :=
           Big_Endian_16 (Frame (Segment_Length), Frame (Segment_Length + 1));
      begin
         if Frame (Type_Byte) /= Code (Response)
           or else Frame (Tag_Byte) /= Tag
           or else Segment > Data'Length - Response_Header_Bytes
         then
            return;
         end if;
         SCSI.Decode_Status (Frame (Status_Byte), Result.Status, Status_Valid);
         if not Status_Valid then
            return;
         end if;
         Result.Target_Response := Frame (Response_Byte);
         Result.Residual :=
           Shift_Left (Unsigned_32 (Frame (Residual_Count)), 3 * Byte_Bits)
           or Shift_Left (Unsigned_32 (Frame (Residual_Count + 1)), 2 * Byte_Bits)
           or Shift_Left (Unsigned_32 (Frame (Residual_Count + 2)), Byte_Bits)
           or Unsigned_32 (Frame (Residual_Count + 3));
         if Segment >= Sense_Length_Bytes then
            declare
               First : constant Positive :=
                 Data'First + (Segment_First - 1);
               Sense_Bytes : constant Natural :=
                 Big_Endian_16 (Data (First), Data (First + 1));
            begin
               if Sense_Bytes > 0
                 and then Sense_Bytes <= Segment - Sense_Length_Bytes
               then
                  SCSI.Decode_Sense
                    (Data (First + Sense_Length_Bytes ..
                           First + (Sense_Length_Bytes - 1) + Sense_Bytes),
                     Result.Sense, Result.Sense_Present);
               end if;
            end;
         end if;
         Valid := True;
      end;
   end Decode_Command_Response;

   procedure Decode_Query_Response
     (Data : Bytes; Tag : Task_Tag; Opcode : Query_Opcode;
      Identifier : Unsigned_8; Result : out Query_Result; Valid : out Boolean)
   is
   begin
      Result := (others => <>);
      Valid := False;
      if Data'Length < Response_Header_Bytes then
         return;
      end if;
      declare
         Frame : constant Bytes (1 .. Response_Header_Bytes) :=
           Data (Data'First .. Data'First + (Response_Header_Bytes - 1));
         Segment : constant Natural :=
           Big_Endian_16 (Frame (Segment_Length), Frame (Segment_Length + 1));
      begin
         if Frame (Type_Byte) /= Code (Query_Response)
           or else Frame (Tag_Byte) /= Tag
           or else Frame (Query_Opcode_Byte) /=
             Unsigned_8 (Query_Opcode'Enum_Rep (Opcode))
           or else Frame (Query_Identifier) /= Identifier
           or else Segment > Maximum_Descriptor_Bytes
           or else Segment > Data'Length - Response_Header_Bytes
         then
            return;
         end if;
         Result.Response_Code := Frame (Response_Byte);
         if Segment > 0 then
            Result.Segment_First := Data'First + (Segment_First - 1);
         end if;
         Result.Segment_Bytes := Segment;
         Result.Flag_Value := (Frame (Query_Flag_Value) and 1) /= 0;
         Valid := True;
      end;
   end Decode_Query_Response;

   ---------------------------------------------------------------------------
   --  Request descriptors.
   ---------------------------------------------------------------------------
   --  DW0: command type 31:28 (UFS storage), data direction 26:25,
   --  interrupt on completion 24.
   UFS_Storage_Command : constant Register_Value := 1 * 2 ** 28;
   Host_To_Device      : constant Register_Value := 2 ** 25;
   Device_To_Host      : constant Register_Value := 2 ** 26;
   Interrupt_On_Done   : constant Register_Value := 2 ** 24;
   OCS_Mask            : constant Register_Value := 16#FF#;
   Field_Shift         : constant := 2 ** 16;
   Low_Word_Mask       : constant Unsigned_64 := 16#FFFF_FFFF#;

   function Transfer_Descriptor
     (Direction        : Transfer_Direction;
      Command_Table    : Physical_Address;
      Response_Offset  : Descriptor_Offset;
      Response_Words   : Descriptor_Offset;
      Regions_Offset   : Descriptor_Offset;
      Regions          : Region_Count) return Descriptor_Words is
     ([0 => UFS_Storage_Command or Interrupt_On_Done
            or (case Direction is
                  when No_Transfer => 0,
                  when To_Device   => Host_To_Device,
                  when From_Device => Device_To_Host),
       2 => Register_Value (Command_Status'Enum_Rep (Invalid_Status)),
       4 => Register_Value (Command_Table and Low_Word_Mask),
       5 => Register_Value (Shift_Right (Command_Table, 32)),
       --  DW6: response UPIU offset 31:16, length 15:0 (in words);
       --  DW7: region table offset 31:16, entry count 15:0.
       6 => Register_Value (Response_Offset) * Field_Shift
            + Register_Value (Response_Words),
       7 => Register_Value (Regions_Offset) * Field_Shift
            + Register_Value (Regions),
       others => 0]);

   function Status_Of (Words : Descriptor_Words) return Command_Status is
      Value : constant Register_Value := Words (2) and OCS_Mask;
   begin
      for Candidate in Command_Status loop
         if Register_Value (Command_Status'Enum_Rep (Candidate)) = Value then
            return Candidate;
         end if;
      end loop;
      return Invalid_Status;
   end Status_Of;

   function Region
     (Address : Physical_Address; Size : Region_Bytes) return Region_Words is
     ([0 => Register_Value (Address and Low_Word_Mask),
       1 => Register_Value (Shift_Right (Address, 32)),
       2 => 0,
       --  Data byte count, zero based.
       3 => Register_Value (Size - 1)]);

   ---------------------------------------------------------------------------
   --  Task management descriptors (UFSHCI 6.2.1).
   ---------------------------------------------------------------------------
   --  Byte 4 holds DW0 bits 31:24 (interrupt); byte 9 the OCS.
   Task_Interrupt_Byte : constant := 4;
   Task_Interrupt_Bit  : constant Unsigned_8 := 16#01#;
   Task_OCS_Byte       : constant := 9;
   Task_Request_First  : constant := 17;
   Task_Response_First : constant := 49;
   --  TASK MANAGEMENT REQUEST input parameters 1 (LUN) and 2 (task tag),
   --  big-endian words at UPIU bytes 13 and 17: the value in the last byte.
   Parameter_1_Last    : constant := 16;
   Parameter_2_Last    : constant := 20;
   --  Service responses (UFS 10.7.7.2).
   Function_Complete   : constant Unsigned_8 := 16#00#;
   Function_Succeeded  : constant Unsigned_8 := 16#08#;

   function Abort_Request
     (Tag : Task_Tag; Unit : Logical_Unit; Target : Task_Tag)
      return Task_Descriptor
   is
      Result : Task_Descriptor := [others => 0];
      Base : constant := Task_Request_First - 1;
   begin
      Result (Task_Interrupt_Byte) := Task_Interrupt_Bit;
      Result (Task_OCS_Byte) :=
        Unsigned_8 (Command_Status'Enum_Rep (Invalid_Status));
      Result (Base + Type_Byte) := Code (Task_Management_Request);
      Result (Base + LUN_Byte) := Unsigned_8 (Unit);
      Result (Base + Tag_Byte) := Tag;
      Result (Base + Function_Byte) :=
        Unsigned_8 (Task_Function'Enum_Rep (Abort_Task));
      Result (Base + Parameter_1_Last) := Unsigned_8 (Unit);
      Result (Base + Parameter_2_Last) := Target;
      return Result;
   end Abort_Request;

   function Decode_Task_Response (Data : Task_Descriptor; Tag : Task_Tag)
     return Task_Outcome
   is
      Base : constant := Task_Response_First - 1;
   begin
      if Data (Task_OCS_Byte) /=
           Unsigned_8 (Command_Status'Enum_Rep (Success))
        or else Data (Base + Type_Byte) /= Code (Task_Management_Response)
        or else Data (Base + Tag_Byte) /= Tag
      then
         return Invalid_Task_Response;
      elsif Data (Base + Response_Byte) = Target_Success
        and then Data (Base + Parameter_1_Last) in
          Function_Complete | Function_Succeeded
      then
         return Abort_Completed;
      else
         return Abort_Failed;
      end if;
   end Decode_Task_Response;
end UFS_UPIU;
