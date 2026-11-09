------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  NVMe wire values the driver's recovery path decodes and encodes: command
--  identifiers, the completion entry's status field, the Abort command and
--  the controller's ready timeout (CAP.TO).
--
--  Reference: NVM Express Base Specification, Revision 2.0 (sections named
--  per value).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package NVMe_Protocol with SPARK_Mode, Pure is

   --  CDW0 bits 31:16 and completion DW3 bits 15:0 (4.2, 4.6).
   type Command_Identifier is mod 2 ** 16;

   --  Submission queue identifiers (completion DW2 bits 31:16).
   type Queue_Identifier is mod 2 ** 16;
   Admin_Queue : constant Queue_Identifier := 0;
   IO_Queue    : constant Queue_Identifier := 1;

   ---------------------------------------------------------------------------
   --  Completion status field (completion DW3 bits 31:16, 4.6.1): bit 0 the
   --  phase tag, bits 8:1 the status code, bits 11:9 the status code type,
   --  bit 15 Do Not Retry.
   ---------------------------------------------------------------------------
   type Status_Field is mod 2 ** 16;

   type Phase_Tag is mod 2;
   Phase_Bit        : constant Status_Field := 2 ** 0;
   Status_Code_Shift : constant := 1;
   Status_Code_Mask  : constant Status_Field := 16#FF#;
   Status_Type_Shift : constant := 9;
   Status_Type_Mask  : constant Status_Field := 2#111#;
   --  Bits 15:1: status code, type, retry delay, more, do not retry.
   Status_Mask : constant Status_Field := 16#FFFE#;

   type Status_Code is mod 2 ** 8;
   type Status_Code_Type is mod 2 ** 3;

   --  Status code types (4.6.1.2).
   Generic_Command_Status : constant Status_Code_Type := 0;

   --  Generic command status values (4.6.1.2.1, figure 102).
   Successful_Completion       : constant Status_Code := 16#00#;
   Command_Abort_Requested     : constant Status_Code := 16#07#;
   Command_Aborted_SQ_Deletion : constant Status_Code := 16#08#;

   function Phase_Of (Status : Status_Field) return Phase_Tag is
     (Phase_Tag (Status and Phase_Bit));

   function Code_Of (Status : Status_Field) return Status_Code is
     (Status_Code ((Status / 2 ** Status_Code_Shift) and Status_Code_Mask));

   function Type_Of (Status : Status_Field) return Status_Code_Type is
     (Status_Code_Type ((Status / 2 ** Status_Type_Shift) and Status_Type_Mask));

   --  Every bit but the phase tag is zero.
   function Succeeded (Status : Status_Field) return Boolean is
     ((Status and Status_Mask) = 0);

   --  The controller aborted the command (an Abort or a queue deletion).
   function Was_Aborted (Status : Status_Field) return Boolean is
     (Type_Of (Status) = Generic_Command_Status
      and then Code_Of (Status) in Command_Abort_Requested
                                 | Command_Aborted_SQ_Deletion);

   ---------------------------------------------------------------------------
   --  Abort (admin opcode 08h, 5.1): CDW10 bits 15:0 the submission queue,
   --  bits 31:16 the command identifier. Completion DW0 bit 0 is set when
   --  the command was NOT aborted.
   ---------------------------------------------------------------------------
   Admin_Abort : constant Unsigned_8 := 16#08#;
   Abort_Command_Shift : constant := 16;
   Abort_Not_Performed : constant Unsigned_32 := 2 ** 0;

   function Abort_Dword_10
     (Queue : Queue_Identifier; Command : Command_Identifier) return Unsigned_32
   is
     (Unsigned_32 (Queue) or Unsigned_32 (Command) * 2 ** Abort_Command_Shift);

   function Abort_Performed (Result : Unsigned_32) return Boolean is
     ((Result and Abort_Not_Performed) = 0);

   ---------------------------------------------------------------------------
   --  CAP.TO (3.1.1, bits 31:24): the worst case time for CSTS.RDY to follow
   --  CC.EN, in 500 ms units.
   ---------------------------------------------------------------------------
   Ready_Timeout_Shift : constant := 24;
   Ready_Timeout_Mask  : constant Unsigned_64 := 16#FF#;
   Ready_Timeout_Unit_Microseconds : constant := 500_000;

   --  CSTS.CFS (3.1.6, bit 1): controller fatal status.
   Fatal_Status : constant Unsigned_32 := 2 ** 1;

   subtype Ready_Microseconds is Unsigned_64
     range Ready_Timeout_Unit_Microseconds
       .. Ready_Timeout_Unit_Microseconds * 256;

   --  The spec's bound, or one unit when the controller reports zero.
   function Ready_Timeout (Capabilities : Unsigned_64) return Ready_Microseconds
   is
     (Ready_Timeout_Unit_Microseconds *
        Unsigned_64'Max
          (1, Shift_Right (Capabilities, Ready_Timeout_Shift)
                and Ready_Timeout_Mask));

end NVMe_Protocol;
