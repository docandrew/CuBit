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
with Boot_Log;
with SCSI;
with SCSI_Disks;
with XHCI;

package body USB_Disk_Service is
   use type XHCI.Disk_Presence;
   use type XHCI.Disk_Outcome;

   --  Bulk-only transport runs one command at a time.
   type Slot is (Only_Slot);

   --  A backstop only: xhci bounds every stage of a command, its REQUEST
   --  SENSE and a reset recovery (about three minutes at the very worst).
   Command_Microseconds : constant := 300_000_000;

   --  What a holder of the write grant sees: a stick may hold completed
   --  writes in a cache, SYNCHRONIZE CACHE commits them. No FUA: many
   --  bridges mishandle it.
   Disk_Features : constant Device_Features :=
     FEATURE_FLUSH or FEATURE_VOLATILE_CACHE or FEATURE_REMOVABLE;

   --  The unit the request in the (only) slot is for.
   Current : CuBit.Block_Unit_Badges.Unit := CuBit.Block_Unit_Badges.Unit'First;

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
                          SCSI.Block_Count_10 (Item.Count), Item.Block_Bytes, False)
      else SCSI.Write_16 (Item.First_Block, SCSI.Block_Count_16 (Item.Count),
                          Item.Block_Bytes, False));

   procedure Copy (Target, Source : System.Address; Count : Unsigned_64) is
      Copied : System.Address;
      pragma Unreferenced (Copied);
   begin
      Copied := CuBit.String.memcpy
        (Target, Source, System.Storage_Elements.Storage_Count (Count));
   end Copy;

   --  The command each busy slot carries.
   Commands : array (Slot) of SCSI.Command_Block;

   procedure Post (Item : Slot; Work : Work_Order) is
   begin
      case Work.Kind is
         when Cache_Flush =>
            Commands (Item) := SCSI.Synchronize_Cache;
         when Block_Transfer =>
            --  Valid_Transfer: at most Max_Transfer blocks, the buffer's size.
            if Work.Granted.Item.Direction = Device_Write then
               --  The controller only sees xhci's private bounce buffer.
               Copy (XHCI.Disk_Buffer, Work.Granted.Buffer, Bytes (Work.Granted.Item));
               Commands (Item) := Write_Command (Work.Granted.Item);
            else
               Commands (Item) := Read_Command (Work.Granted.Item);
            end if;
      end case;
      XHCI.Start_Disk_Command (Current, Commands (Item));
   end Post;

   function Finished (Item : Slot) return Boolean is
     (Item = Only_Slot and then XHCI.Disk_Command_Done);

   procedure Collect (Item : Slot; Work : Work_Order; Outcome : out Work_Outcome) is
      Result : constant XHCI.Disk_Result := XHCI.Disk_Command_Result;
      Passed : constant Boolean := Result.Outcome = XHCI.Disk_Passed;
   begin
      case Work.Kind is
         when Cache_Flush =>
            Outcome := (Passed => Passed, Moved => 0);
         when Block_Transfer =>
            if Passed and then Unsigned_64 (Result.Moved) = Bytes (Work.Granted.Item) then
               --  Only a whole, successful read reaches the caller's grant.
               if Work.Granted.Item.Direction = Device_Read then
                  Copy (Work.Granted.Buffer, XHCI.Disk_Buffer, Bytes (Work.Granted.Item));
               end if;
               Outcome := (Passed => True, Moved => Bytes (Work.Granted.Item));
            else
               Outcome := (Passed => False, Moved => 0);
            end if;
      end case;
      if not Passed then
         Boot_Log.Line ("xhci: usb disk unit" & CuBit.Block_Unit_Badges.Unit'Image (Current) & " " &
                     SCSI.Operation_Code'Image (Commands (Item).Operation) &
                        " failed: " & XHCI.Disk_Outcome'Image (Result.Outcome) & " sense=" &
                        SCSI_Disks.Sense_Meaning'Image (Result.Sense));
      end if;
   end Collect;

   procedure Cancel (Item : Slot) is
      pragma Unreferenced (Item);
   begin
      XHCI.Cancel_Disk_Command;
   end Cancel;

   procedure Note (Event : Driver_Event; Count : Unsigned_64) is
   begin
      Boot_Log.Line ("xhci: usb disk " &
                     (case Event is
                        when Command_Expired => "command expired",
                        when Write_Refused => "write refused: no write grant") &
                     " count=" & Unsigned_64'Image (Count));
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

   function State (Disk : XHCI.Disk_Status) return Device_State is
   begin
      if Disk.Presence /= XHCI.Disk_Ready then
         return (Presence => No_Device);
      end if;
      declare
         Size : constant Logical_Block_Size :=
           Logical_Block_Size (SCSI_Disks.Bytes_Of (Disk.Geometry.Size));
      begin
         return (Presence      => Available,
                 Blocks        => Disk.Geometry.Blocks,
                 Logical_Size  => Size,
                 Physical_Size => Size,
                 Max_Transfer  => XHCI.Disk_Buffer_Bytes / Unsigned_64 (Size),
                 Features      => Disk_Features or
                                  (if Disk.Write_Protected then FEATURE_READ_ONLY else 0),
                 Media         => Removable_Media);
      end;
   end State;

   function Trimmed (Text : String) return String is
      Last : Natural := Text'Last;
   begin
      while Last >= Text'First and then Text (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      return Text (Text'First .. Last);
   end Trimmed;

   procedure Complete (Progressed : out Boolean) is
   begin
      Driver.Collect_Finished (Progressed);
   end Complete;

   function Busy return Boolean is (Driver.Outstanding);

   procedure Serve
     (Sender : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Unit : CuBit.Block_Unit_Badges.Unit)
   is
      Disk : constant XHCI.Disk_Status := XHCI.Disk (Unit);
   begin
      Current := Unit;
      --  The unit's badge is its write grant (devmgr minted it for the
      --  filesystem alone); a write-protected medium still refuses writes.
      Driver.Serve_As
        (Sender, Request, State (Disk),
         (Model  => CuBit.Device_Identities.To_Text
                      (Trimmed (Disk.Identity.Vendor) & " " & Trimmed (Disk.Identity.Product)),
          Serial => CuBit.Device_Identities.To_Text ("")),
         Writes_Granted);
   end Serve;
end USB_Disk_Service;
