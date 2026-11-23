------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The USB Mass Storage Bulk-Only Transport wire format (BOT 1.0, 5.1 to
--  5.3, 6.3): the command block wrapper around any SCSI command block of
--  package SCSI, the command status wrapper's validation, and GET MAX LUN.
--
--  @description
--  Pure codec: no DMA, endpoints, timeouts or recovery (the xHCI adapter
--  owns those). Every CSW and GET MAX LUN reply is untrusted device data;
--  the contracts say exactly which ones are accepted and what they mean.
--  The byte count a transfer really moved comes from the host controller,
--  never from the device: a CSW cannot claim data the host did not see.
--  (USB_Optical keeps its own read-only wrapper for the boot medium.)
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with SCSI;

package USB_Bulk_Only with SPARK_Mode, Pure is
   use type SCSI.Data_Direction;
   subtype Bytes is SCSI.Bytes;
   subtype Logical_Unit is Unsigned_8 range 0 .. 15;

   --  Class-specific requests on the interface (BOT 3.1, 3.2).
   Mass_Storage_Reset : constant Unsigned_8 := 16#FF#;
   Get_Max_LUN        : constant Unsigned_8 := 16#FE#;

   ---------------------------------------------------------------------------
   --  Command Block Wrapper (BOT 5.1), 1-based byte positions.
   ---------------------------------------------------------------------------
   CBW_Bytes : constant := 31;
   subtype Command_Wrapper is Bytes (1 .. CBW_Bytes);
   --  "USBC" and "USBS" as little-endian words.
   CBW_Signature : constant Unsigned_32 := 16#4342_5355#;
   CSW_Signature : constant Unsigned_32 := 16#5342_5355#;
   CBW_Tag_Field    : constant := 5;    --  bytes 5 .. 8
   CBW_Length_Field : constant := 9;    --  bytes 9 .. 12
   CBW_Flags_Field  : constant := 13;
   CBW_LUN_Field    : constant := 14;
   CBW_CB_Length_Field : constant := 15;
   CBW_CB_Field     : constant := 16;   --  bytes 16 .. 31
   --  bmCBWFlags bit 7: data from the device to the host.
   Data_In_Flag : constant Unsigned_8 := 16#80#;

   function Little_Endian (Data : Bytes; First : Positive) return Unsigned_32
   is (Unsigned_32 (Data (First))
       or Shift_Left (Unsigned_32 (Data (First + 1)), 8)
       or Shift_Left (Unsigned_32 (Data (First + 2)), 16)
       or Shift_Left (Unsigned_32 (Data (First + 3)), 24))
     with Pre => First >= Data'First and then Data'Last >= First
                 and then Data'Last - First >= 3;

   --  The CBW carrying Block for LUN with Tag: data length and direction
   --  are the command's own, so the device is never told to move more
   --  than the command means.
   function Encode
     (Block : SCSI.Command_Block; Tag : Unsigned_32; LUN : Logical_Unit)
      return Command_Wrapper
     with Pre  => SCSI.Well_Formed (Block),
          Post =>
            Little_Endian (Encode'Result, 1) = CBW_Signature
            and then Little_Endian (Encode'Result, CBW_Tag_Field) = Tag
            and then Little_Endian (Encode'Result, CBW_Length_Field) =
              Block.Expected
            and then Encode'Result (CBW_Flags_Field) =
              (if Block.Direction = SCSI.From_Device then Data_In_Flag else 0)
            and then Encode'Result (CBW_LUN_Field) = LUN
            and then Encode'Result (CBW_CB_Length_Field) =
              Unsigned_8 (SCSI.Length (Block.Size))
            and then (for all Index in SCSI.CDB_Index =>
                        Encode'Result (CBW_CB_Field + Index - 1) =
                          Block.Data (Index));

   ---------------------------------------------------------------------------
   --  Command Status Wrapper (BOT 5.2, 6.3).
   ---------------------------------------------------------------------------
   CSW_Bytes : constant := 13;
   CSW_Tag_Field     : constant := 5;    --  bytes 5 .. 8
   CSW_Residue_Field : constant := 9;    --  bytes 9 .. 12
   CSW_Status_Field  : constant := 13;
   --  bCSWStatus values.
   Status_Passed      : constant Unsigned_8 := 0;
   Status_Failed      : constant Unsigned_8 := 1;
   Status_Phase_Error : constant Unsigned_8 := 2;

   type Status_Kind is
     (Good,              --  the command passed
      Check_Condition,   --  the command failed: REQUEST SENSE says why
      Phase_Error,       --  the device lost the protocol: reset recovery
      Invalid);          --  not a valid, meaningful CSW: reset recovery

   type Status_Report is record
      Kind    : Status_Kind := Invalid;
      --  dCSWDataResidue (Good and Check_Condition only).
      Residue : SCSI.Transfer_Length := 0;
      --  The data bytes the host controller moved (Good and
      --  Check_Condition only).
      Moved   : SCSI.Transfer_Length := 0;
   end record;

   --  Data: the CSW as received. Expected_Bytes: the CBW's data length.
   --  Moved: the data bytes the host controller transferred.
   --  Valid (6.3.1): 13 bytes, the signature and the CBW's tag. Meaningful
   --  (6.3.2): a phase error, or a pass/fail whose residue is at most the
   --  CBW's length. The controller can never move more than the CBW asked.
   function Decode_Status
     (Data : Bytes; Expected_Tag : Unsigned_32;
      Expected_Bytes, Moved : SCSI.Transfer_Length) return Status_Report
     with Post =>
       (if Data'Length /= CSW_Bytes
          or else Little_Endian (Data, Data'First) /= CSW_Signature
          or else Little_Endian (Data, Data'First + (CSW_Tag_Field - 1)) /=
            Expected_Tag
          or else Data (Data'First + (CSW_Status_Field - 1)) > Status_Phase_Error
        then Decode_Status'Result = (Kind => Invalid, Residue => 0, Moved => 0)
        elsif Data (Data'First + (CSW_Status_Field - 1)) = Status_Phase_Error then
          Decode_Status'Result = (Kind => Phase_Error, Residue => 0, Moved => 0)
        elsif Little_Endian (Data, Data'First + (CSW_Residue_Field - 1)) >
            Expected_Bytes
          or else Moved > Expected_Bytes
        then Decode_Status'Result = (Kind => Invalid, Residue => 0, Moved => 0)
        else
          Decode_Status'Result =
            (Kind    =>
               (if Data (Data'First + (CSW_Status_Field - 1)) = Status_Passed
                then Good else Check_Condition),
             Residue => Little_Endian (Data, Data'First + (CSW_Residue_Field - 1)),
             Moved   => Moved));

   --  The command passed and moved exactly Expected_Bytes: what a block
   --  READ or WRITE must report before its data counts.
   function Complete
     (Report : Status_Report; Expected_Bytes : SCSI.Transfer_Length)
      return Boolean
   is (Report.Kind = Good and then Report.Residue = 0
       and then Report.Moved = Expected_Bytes);

   ---------------------------------------------------------------------------
   --  GET MAX LUN (BOT 3.2).
   ---------------------------------------------------------------------------
   --  How the control transfer ended. Only a STALL means "one unit" (a
   --  device may stall the request when it has a single LUN); a timeout or
   --  error, or a malformed answer, selects nothing.
   type Control_Result is (Control_Success, Control_Stall, Control_Error);
   procedure Decode_Max_LUN
     (Outcome : Control_Result; Data : Bytes;
      Last_LUN : out Logical_Unit; Valid : out Boolean)
     with Post =>
       Valid = (Outcome = Control_Stall
                or else (Outcome = Control_Success and then Data'Length = 1
                         and then Data (Data'First) <= Logical_Unit'Last))
       and then Last_LUN =
         (if Valid and then Outcome = Control_Success
          then Data (Data'First) else 0);
end USB_Bulk_Only;
