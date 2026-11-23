------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body USB_Bulk_Only with SPARK_Mode is
   Byte_Mask : constant := 16#FF#;

   --  Value as four little-endian bytes at First.
   procedure Put_Word
     (Data : in out Command_Wrapper; First : Positive; Value : Unsigned_32)
     with Pre  => First <= CBW_Bytes - 3,
          Post => Little_Endian (Data, First) = Value
                  and then (for all Index in Data'Range =>
                              (if Index not in First .. First + 3 then
                                 Data (Index) = Data'Old (Index)));

   procedure Put_Word
     (Data : in out Command_Wrapper; First : Positive; Value : Unsigned_32) is
   begin
      Data (First) := Unsigned_8 (Value and Byte_Mask);
      Data (First + 1) := Unsigned_8 (Shift_Right (Value, 8) and Byte_Mask);
      Data (First + 2) := Unsigned_8 (Shift_Right (Value, 16) and Byte_Mask);
      Data (First + 3) := Unsigned_8 (Shift_Right (Value, 24));
   end Put_Word;

   function Encode
     (Block : SCSI.Command_Block; Tag : Unsigned_32; LUN : Logical_Unit)
      return Command_Wrapper
   is
      Result : Command_Wrapper := [others => 0];
   begin
      for Index in SCSI.CDB_Index loop
         Result (CBW_CB_Field + Index - 1) := Block.Data (Index);
         pragma Loop_Invariant
           (for all Earlier in SCSI.CDB_Index'First .. Index =>
              Result (CBW_CB_Field + Earlier - 1) = Block.Data (Earlier));
         pragma Loop_Invariant
           (for all Other in 1 .. CBW_CB_Field - 1 => Result (Other) = 0);
      end loop;
      Result (CBW_Flags_Field) :=
        (if Block.Direction = SCSI.From_Device then Data_In_Flag else 0);
      Result (CBW_LUN_Field) := LUN;
      Result (CBW_CB_Length_Field) := Unsigned_8 (SCSI.Length (Block.Size));
      Put_Word (Result, 1, CBW_Signature);
      Put_Word (Result, CBW_Tag_Field, Tag);
      Put_Word (Result, CBW_Length_Field, Block.Expected);
      return Result;
   end Encode;

   function Decode_Status
     (Data : Bytes; Expected_Tag : Unsigned_32;
      Expected_Bytes, Moved : SCSI.Transfer_Length) return Status_Report
   is
      Rejected : constant Status_Report := (Kind => Invalid, Residue => 0, Moved => 0);
   begin
      if Data'Length /= CSW_Bytes then
         return Rejected;
      end if;
      declare
         Status : constant Unsigned_8 := Data (Data'First + (CSW_Status_Field - 1));
         Residue : constant Unsigned_32 :=
           Little_Endian (Data, Data'First + (CSW_Residue_Field - 1));
      begin
         if Little_Endian (Data, Data'First) /= CSW_Signature
           or else Little_Endian (Data, Data'First + (CSW_Tag_Field - 1)) /= Expected_Tag
           or else Status > Status_Phase_Error
         then
            return Rejected;
         elsif Status = Status_Phase_Error then
            --  BOT 6.7: the residue means nothing after a phase error.
            return (Kind => Phase_Error, Residue => 0, Moved => 0);
         elsif Residue > Expected_Bytes or else Moved > Expected_Bytes then
            return Rejected;
         end if;
         return (Kind    => (if Status = Status_Passed then Good else Check_Condition),
                 Residue => Residue,
                 Moved   => Moved);
      end;
   end Decode_Status;

   procedure Decode_Max_LUN
     (Outcome : Control_Result; Data : Bytes;
      Last_LUN : out Logical_Unit; Valid : out Boolean)
   is
   begin
      Last_LUN := 0;
      Valid := False;
      case Outcome is
         when Control_Stall =>
            Valid := True;
         when Control_Error =>
            null;
         when Control_Success =>
            if Data'Length = 1 and then Data (Data'First) <= Logical_Unit'Last then
               Last_LUN := Data (Data'First);
               Valid := True;
            end if;
      end case;
   end Decode_Max_LUN;
end USB_Bulk_Only;
