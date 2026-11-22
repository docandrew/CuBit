------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  UFS requests over the controller's transfer slots: SCSI commands, NOP,
--  descriptor and flag queries. Start_SCSI and Finish_SCSI are the async
--  pair the block service uses; the Run_ procedures post on one slot and
--  wait with a deadline, for bring-up before the service starts.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with SCSI;
with UFS_UPIU; use UFS_UPIU;
with UFS_Layout; use UFS_Layout;

package UFS_Commands is
   type Command_Result is
     (Passed, Check_Condition, Transport_Error, Invalid_Response, Timed_Out);

   type SCSI_Outcome is record
      Result        : Command_Result := Invalid_Response;
      Status        : SCSI.Status_Code := SCSI.Good;
      Sense_Present : Boolean := False;
      Sense         : SCSI.Sense_Data;
      --  Bytes the device transferred.
      Moved         : Unsigned_32 := 0;
      --  The controller's overall command status and the UPIU response.
      Transfer_Status : Command_Status := Success;
      Target_Response : Unsigned_8 := Target_Success;
   end record;

   --  Hex key=value pairs explaining a failed SCSI command (log lines).
   function Failure_Text (Outcome : SCSI_Outcome) return String;

   --  Data for a command travels in the slot's buffer.
   function Fits_Buffer (Command : SCSI.Command_Block) return Boolean is
     (SCSI.Well_Formed (Command)
      and then Command.Expected <= Buffer_Bytes
      and then Command.Expected mod Word_Bytes = 0);

   procedure Start_SCSI
     (Item : Slot; Unit : Logical_Unit; Command : SCSI.Command_Block)
     with Pre => Fits_Buffer (Command);

   --  After UFS_Controller.Completed (Item).
   function Finish_SCSI (Item : Slot; Command : SCSI.Command_Block)
     return SCSI_Outcome;

   --  Bring-up: one request at a time on slot Bring_Up_Slot, each waiting
   --  at most Micros.
   Bring_Up_Slot : constant Slot := Slot'First;

   procedure Run_SCSI
     (Unit : Logical_Unit; Command : SCSI.Command_Block; Micros : Unsigned_64;
      Outcome : out SCSI_Outcome)
     with Pre => Fits_Buffer (Command);

   procedure Run_NOP (Micros : Unsigned_64; Answered : out Boolean);

   --  Why the last NOP or query failed, as hex key=value pairs.
   function Last_Request_Failure return String;

   subtype Descriptor_Buffer is SCSI.Bytes (1 .. Maximum_Descriptor_Bytes);
   procedure Run_Read_Descriptor
     (Kind : Descriptor_Kind; Index : Descriptor_Index; Micros : Unsigned_64;
      Data : out Descriptor_Buffer; Length : out Natural; Valid : out Boolean)
     with Post => Length <= Maximum_Descriptor_Bytes;

   procedure Run_Flag
     (Opcode : Flag_Opcode; Flag : Device_Flag; Micros : Unsigned_64;
      Value : out Boolean; Valid : out Boolean);
end UFS_Commands;
