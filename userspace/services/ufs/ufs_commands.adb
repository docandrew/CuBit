------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CuBit.Busy_Poll;
with UFS_Controller;
with UFS_Log;

package body UFS_Commands is
   use type SCSI.Status_Code;
   use type SCSI.Data_Direction;

   --  The last NOP or query: whether it timed out, its OCS, its query
   --  response code.
   Last_Timed_Out : Boolean := False;
   Last_Status : Command_Status := Success;
   Last_Query_Response : Unsigned_8 := Query_Success;

   function Status_Code (Item : Command_Status) return Unsigned_64 is
     (Unsigned_64 (Command_Status'Enum_Rep (Item)));

   function Last_Request_Failure return String is
     ((if Last_Timed_Out then " timeout" else "") &
      UFS_Log.Hex_Field ("ocs", Status_Code (Last_Status), 2) &
      UFS_Log.Hex_Field ("resp", Unsigned_64 (Last_Query_Response), 2));

   function Failure_Text (Outcome : SCSI_Outcome) return String is
     (" " & (case Outcome.Result is
               when Passed => "passed",
               when Check_Condition => "check-condition",
               when Transport_Error => "transport",
               when Invalid_Response => "invalid-response",
               when Timed_Out => "timeout") &
      UFS_Log.Hex_Field ("ocs", Status_Code (Outcome.Transfer_Status), 2) &
      UFS_Log.Hex_Field ("resp", Unsigned_64 (Outcome.Target_Response), 2) &
      UFS_Log.Hex_Field
        ("status", Unsigned_64 (SCSI.Status_Code'Enum_Rep (Outcome.Status)), 2) &
      (if Outcome.Sense_Present then
         UFS_Log.Hex_Field
           ("key", Unsigned_64 (SCSI.Sense_Key'Enum_Rep (Outcome.Sense.Key)), 1) &
         UFS_Log.Hex_Field ("asc", Unsigned_64 (Outcome.Sense.Code), 2) &
         UFS_Log.Hex_Field ("ascq", Unsigned_64 (Outcome.Sense.Qualifier), 2)
       else ""));

   function Direction_Of (Command : SCSI.Command_Block)
     return Transfer_Direction is
     (case Command.Direction is
        when SCSI.No_Data     => No_Transfer,
        when SCSI.From_Device => From_Device,
        when SCSI.To_Device   => To_Device);

   procedure Start_SCSI
     (Item : Slot; Unit : Logical_Unit; Command : SCSI.Command_Block) is
   begin
      UFS_Controller.Post
        (Item, UFS_UPIU.Command (Task_Tag (Item), Unit, Command),
         Direction_Of (Command), Natural (Command.Expected));
   end Start_SCSI;

   function Finish_SCSI (Item : Slot; Command : SCSI.Command_Block)
     return SCSI_Outcome
   is
      Outcome : SCSI_Outcome;
      Result : UFS_UPIU.Command_Result;
      Valid : Boolean;
   begin
      Outcome.Transfer_Status := UFS_Controller.Status (Item);
      if Outcome.Transfer_Status /= Success then
         Outcome.Result := Transport_Error;
         return Outcome;
      end if;
      Decode_Command_Response
        (UFS_Controller.Response (Item), Task_Tag (Item), Result, Valid);
      if not Valid or else Result.Residual > Command.Expected then
         Outcome.Result := Invalid_Response;
         return Outcome;
      end if;
      Outcome.Status := Result.Status;
      Outcome.Target_Response := Result.Target_Response;
      Outcome.Sense_Present := Result.Sense_Present;
      Outcome.Sense := Result.Sense;
      Outcome.Moved := Command.Expected - Result.Residual;
      --  A CHECK CONDITION comes with response Target Failure (UFS 10.7.2.3);
      --  only GOOD needs Target Success to count as passed.
      Outcome.Result :=
        (if Result.Status = SCSI.Check_Condition then Check_Condition
         elsif Result.Target_Response /= Target_Success then Transport_Error
         elsif Result.Status = SCSI.Good then Passed
         else Transport_Error);
      return Outcome;
   end Finish_SCSI;

   --  Waits for the bring-up slot; on expiry aborts it so it is free again.
   function Wait_Bring_Up (Micros : Unsigned_64; Unit : Logical_Unit)
     return Boolean
   is
      Start : constant Unsigned_64 := CuBit.Busy_Poll.Now;
      Aborted : Boolean;
   begin
      loop
         if UFS_Controller.Completed (Bring_Up_Slot) then
            return True;
         end if;
         exit when not CuBit.Busy_Poll.Within (Start, Micros);
         CuBit.Busy_Poll.Relax;
      end loop;
      UFS_Controller.Abort_Slot (Bring_Up_Slot, Unit, Aborted);
      return False;
   end Wait_Bring_Up;

   procedure Run_SCSI
     (Unit : Logical_Unit; Command : SCSI.Command_Block; Micros : Unsigned_64;
      Outcome : out SCSI_Outcome) is
   begin
      Start_SCSI (Bring_Up_Slot, Unit, Command);
      if Wait_Bring_Up (Micros, Unit) then
         Outcome := Finish_SCSI (Bring_Up_Slot, Command);
      else
         Outcome := (Result => Timed_Out, others => <>);
      end if;
   end Run_SCSI;

   procedure Run_NOP (Micros : Unsigned_64; Answered : out Boolean) is
   begin
      UFS_Controller.Post
        (Bring_Up_Slot, NOP_Out (Task_Tag (Bring_Up_Slot)), No_Transfer, 0);
      Last_Timed_Out := not Wait_Bring_Up (Micros, Device_Unit);
      Last_Status := UFS_Controller.Status (Bring_Up_Slot);
      Last_Query_Response := Query_Success;
      Answered := not Last_Timed_Out
        and then Last_Status = Success
        and then Valid_NOP_In (UFS_Controller.Response (Bring_Up_Slot),
                               Task_Tag (Bring_Up_Slot));
   end Run_NOP;

   --  Posts a query and decodes its response.
   procedure Run_Query
     (Request : Request_Frame; Opcode : Query_Opcode; Identifier : Unsigned_8;
      Micros : Unsigned_64; Response : out UFS_Controller.Response_Area;
      Result : out Query_Result; Valid : out Boolean) is
   begin
      Result := (others => <>);
      Response := [others => 0];
      UFS_Controller.Post (Bring_Up_Slot, Request, No_Transfer, 0);
      Last_Timed_Out := not Wait_Bring_Up (Micros, Device_Unit);
      Last_Status := UFS_Controller.Status (Bring_Up_Slot);
      Last_Query_Response := Query_Success;
      Valid := not Last_Timed_Out and then Last_Status = Success;
      if Valid then
         Response := UFS_Controller.Response (Bring_Up_Slot);
         Decode_Query_Response
           (Response, Task_Tag (Bring_Up_Slot), Opcode, Identifier, Result,
            Valid);
         Last_Query_Response := Result.Response_Code;
         Valid := Valid and then Result.Response_Code = Query_Success;
      end if;
   end Run_Query;

   procedure Run_Read_Descriptor
     (Kind : Descriptor_Kind; Index : Descriptor_Index; Micros : Unsigned_64;
      Data : out Descriptor_Buffer; Length : out Natural; Valid : out Boolean)
   is
      Response : UFS_Controller.Response_Area;
      Result : Query_Result;
      Identifier : constant Unsigned_8 :=
        Unsigned_8 (Descriptor_Kind'Enum_Rep (Kind));
   begin
      Data := [others => 0];
      Length := 0;
      Run_Query
        (Read_Descriptor (Task_Tag (Bring_Up_Slot), Kind, Index,
                          Maximum_Descriptor_Bytes),
         UFS_UPIU.Read_Descriptor, Identifier, Micros, Response, Result,
         Valid);
      if Valid and then Result.Segment_Bytes > 0 then
         Length := Result.Segment_Bytes;
         Data (1 .. Length) :=
           Response (Result.Segment_First ..
                     Result.Segment_First + Length - 1);
      end if;
   end Run_Read_Descriptor;

   procedure Run_Flag
     (Opcode : Flag_Opcode; Flag : Device_Flag; Micros : Unsigned_64;
      Value : out Boolean; Valid : out Boolean)
   is
      Response : UFS_Controller.Response_Area;
      Result : Query_Result;
   begin
      Run_Query
        (Flag_Request (Task_Tag (Bring_Up_Slot), Opcode, Flag), Opcode,
         Unsigned_8 (Device_Flag'Enum_Rep (Flag)), Micros, Response, Result,
         Valid);
      Value := Valid and then Result.Flag_Value;
   end Run_Flag;
end UFS_Commands;
