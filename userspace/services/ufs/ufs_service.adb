------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.Block_Work; use CuBit.Block_Work;
with CuBit.Block_Driver;
with CuBit.Device_Identities;
with CuBit.String;
with SCSI;
with UFS_Commands; use UFS_Commands;
with UFS_Controller;
with UFS_Layout; use UFS_Layout;
with UFS_Log;
with UFS_UPIU;

package body UFS_Service is
   --  A command the device has not finished in this long is aborted
   --  (Linux's SCSI default is 30 s).
   Command_Microseconds : constant := 30_000_000;

   Unit : UFS_UPIU.Logical_Unit := 0;
   --  The command each busy slot carries (Finish_SCSI needs it back).
   Commands : array (Slot) of SCSI.Command_Block;
   --  The last expired command, for the timeout log line.
   Last_Expired : Slot := Slot'First;
   Last_Aborted : Boolean := False;

   --  What the unit offers a holder of the write grant. UFS devices keep a
   --  volatile write cache: SYNCHRONIZE CACHE commits it, and a FUA write
   --  is durable on completion. Everyone else sees the unit read only.
   Unit_Features : constant Device_Features :=
     FEATURE_FLUSH or FEATURE_VOLATILE_CACHE or FEATURE_FUA;

   function Read_Command (Item : Transfer_Request) return SCSI.Command_Block is
     (if Item.First_Block <= Unsigned_64 (Unsigned_32'Last)
         and then Item.Count <= Unsigned_64 (Unsigned_16'Last)
      then SCSI.Read_10 (Unsigned_32 (Item.First_Block),
                         SCSI.Block_Count_10 (Item.Count), Item.Block_Bytes)
      else SCSI.Read_16 (Item.First_Block, SCSI.Block_Count_16 (Item.Count),
                         Item.Block_Bytes));

   function Write_Command (Item : Transfer_Request) return SCSI.Command_Block is
     (if Item.First_Block <= Unsigned_64 (Unsigned_32'Last)
         and then Item.Count <= Unsigned_64 (Unsigned_16'Last)
      then SCSI.Write_10 (Unsigned_32 (Item.First_Block),
                          SCSI.Block_Count_10 (Item.Count), Item.Block_Bytes,
                          Item.Force_Unit_Access)
      else SCSI.Write_16 (Item.First_Block, SCSI.Block_Count_16 (Item.Count),
                          Item.Block_Bytes, Item.Force_Unit_Access));

   procedure Copy (Target, Source : System.Address; Count : Unsigned_64) is
      Copied : System.Address;
      pragma Unreferenced (Copied);
   begin
      Copied := CuBit.String.memcpy
        (Target, Source, System.Storage_Elements.Storage_Count (Count));
   end Copy;

   procedure Post (Item : Slot; Work : Work_Order) is
   begin
      case Work.Kind is
         when Cache_Flush =>
            Commands (Item) := SCSI.Synchronize_Cache;
         when Block_Transfer =>
            if Work.Granted.Item.Direction = Device_Write then
               --  The controller only sees the slot's bounce buffer.
               Copy (UFS_Controller.Buffer (Item), Work.Granted.Buffer,
                     Bytes (Work.Granted.Item));
               Commands (Item) := Write_Command (Work.Granted.Item);
            else
               Commands (Item) := Read_Command (Work.Granted.Item);
            end if;
      end case;
      Start_SCSI (Item, Unit, Commands (Item));
   end Post;

   function Finished (Item : Slot) return Boolean is
     (UFS_Controller.Completed (Item));

   procedure Collect (Item : Slot; Work : Work_Order; Outcome : out Work_Outcome)
   is
      Result : constant SCSI_Outcome := Finish_SCSI (Item, Commands (Item));
      Succeeded : constant Boolean := Result.Result = Passed;
   begin
      case Work.Kind is
         when Cache_Flush =>
            if not Succeeded then
               UFS_Log.Note ("flush failed" & Failure_Text (Result));
            end if;
            Outcome := (Passed => Succeeded, Moved => 0);
         when Block_Transfer =>
            Outcome := (Passed => Succeeded, Moved => Unsigned_64 (Result.Moved));
            if Succeeded and then Outcome.Moved = Bytes (Work.Granted.Item) then
               --  Only a whole, successful read reaches the caller's grant.
               if Work.Granted.Item.Direction = Device_Read then
                  Copy (Work.Granted.Buffer, UFS_Controller.Buffer (Item),
                        Outcome.Moved);
               end if;
            elsif Work.Granted.Item.Direction = Device_Write then
               UFS_Log.Note ("write failed" & Failure_Text (Result) &
                             UFS_Log.Field ("lba", Work.Granted.Item.First_Block));
            else
               --  Nothing of a failed or short read reached the caller.
               Outcome.Moved := 0;
            end if;
      end case;
   end Collect;

   procedure Cancel (Item : Slot) is
   begin
      UFS_Controller.Abort_Slot (Item, Unit, Last_Aborted);
      Last_Expired := Item;
   end Cancel;

   procedure Note (Event : Driver_Event; Count : Unsigned_64) is
   begin
      case Event is
         when Command_Expired =>
            UFS_Log.Note ("io timeout" &
                          UFS_Log.Field ("slot", Unsigned_64 (Last_Expired)) &
                          UFS_Log.Field ("aborted", Boolean'Pos (Last_Aborted)) &
                          UFS_Log.Field ("count", Count));
         when Write_Refused =>
            UFS_Log.Note ("write refused: no write grant" &
                          UFS_Log.Field ("count", Count));
      end case;
   end Note;

   package Driver is new CuBit.Block_Driver
     (Slot                  => Slot,
      Writes                => Write_Grant_Required,
      Deadline_Microseconds => Command_Microseconds,
      Post                  => Post,
      Finished              => Finished,
      Collect               => Collect,
      Cancel                => Cancel,
      Note                  => Note);

   procedure Start (Device : UFS_Identify.Identity) is
   begin
      Unit := Device.Unit;
      Driver.Start
        (State =>
           (Presence      => Available,
            Blocks        => Device.Blocks,
            Logical_Size  => Device.Block_Size,
            Physical_Size => Device.Block_Size,
            Max_Transfer  => Buffer_Bytes / Unsigned_64 (Device.Block_Size),
            Features      => Unit_Features,
            Media         => Fixed_Media),
         Identity =>
           (Model  => CuBit.Device_Identities.To_Text
                        (Device.Model.Value (1 .. Device.Model.Length)),
            Serial => CuBit.Device_Identities.To_Text
                        (Device.Serial.Value (1 .. Device.Serial.Length))));
   end Start;

   procedure Poll (Progressed : out Boolean) renames Driver.Poll;

   function Outstanding return Boolean is (Driver.Outstanding);
end UFS_Service;
