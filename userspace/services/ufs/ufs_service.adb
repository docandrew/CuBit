------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.Busy_Poll;
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

   --  What a busy slot is doing: a block transfer holds the caller's grant;
   --  a cache flush (SYNCHRONIZE CACHE) holds none.
   type Work_Kind is (Block_Transfer, Cache_Flush);

   type Request_Record is record
      Busy    : Boolean := False;
      Kind    : Work_Kind := Block_Transfer;
      Granted : Loan;
      Reply   : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
      Command : SCSI.Command_Block;
      Started : Unsigned_64 := 0;
   end record;

   Requests : array (Slot) of Request_Record;
   Device : Device_State := (Presence => Faulted);
   Unit : UFS_UPIU.Logical_Unit := 0;
   Timeouts : Unsigned_64 := 0;
   Refused_Writes : Unsigned_64 := 0;

   --  What the unit offers a holder of the write grant. UFS devices keep a
   --  volatile write cache: SYNCHRONIZE CACHE commits it, and a FUA write
   --  is durable on completion. Everyone else sees the unit read only
   --  (Granted_View).
   Unit_Features : constant Device_Features :=
     FEATURE_FLUSH or FEATURE_VOLATILE_CACHE or FEATURE_FUA;

   procedure Start (Device : UFS_Identify.Identity) is
   begin
      Unit := Device.Unit;
      UFS_Service.Device :=
        (Presence      => Available,
         Blocks        => Device.Blocks,
         Logical_Size  => Device.Block_Size,
         Physical_Size => Device.Block_Size,
         Max_Transfer  => Buffer_Bytes / Unsigned_64 (Device.Block_Size),
         Features      => Unit_Features,
         Media         => Fixed_Media);
   end Start;

   function Outstanding return Boolean is
     (for some Item of Requests => Item.Busy);

   procedure Answer (Item : Slot; Response : Message) is
      Ignored : Unsigned_64;
   begin
      Ignored := replyCap (Requests (Item).Reply, Response);
      Requests (Item) := (others => <>);
   end Answer;

   procedure Complete (Item : Slot) is
      Current : Request_Record renames Requests (Item);
      Outcome : constant SCSI_Outcome := Finish_SCSI (Item, Current.Command);
      Expected : Unsigned_64;
      Moved : Unsigned_64 := 0;
      Response : Message;
      Copied : System.Address;
      pragma Unreferenced (Copied);
   begin
      if Current.Kind = Cache_Flush then
         if Outcome.Result /= Passed then
            UFS_Log.Note ("flush failed" & Failure_Text (Outcome));
         end if;
         Answer (Item, Status_Reply (Outcome.Result = Passed, 0));
         return;
      end if;
      Expected := Bytes (Current.Granted.Item);
      if Outcome.Result = Passed and then Unsigned_64 (Outcome.Moved) = Expected
      then
         --  Only a whole, successful read reaches the caller's grant (a
         --  write's data was copied out of it before its command started).
         if Current.Granted.Item.Direction = Device_Read then
            Copied := CuBit.String.memcpy
              (Current.Granted.Buffer, UFS_Controller.Buffer (Item),
               System.Storage_Elements.Storage_Count (Expected));
         end if;
         Moved := Expected;
      elsif Current.Granted.Item.Direction = Device_Write then
         UFS_Log.Note ("write failed" & Failure_Text (Outcome) &
                       UFS_Log.Field ("lba", Current.Granted.Item.First_Block));
      end if;
      Settle (Current.Granted, Moved, Response);
      Answer (Item, Response);
   end Complete;

   procedure Expire (Item : Slot) is
      Aborted : Boolean;
      Response : Message;
   begin
      UFS_Controller.Abort_Slot (Item, Unit, Aborted);
      Timeouts := Timeouts + 1;
      --  Counted, not flooded: the first, then each doubling.
      if (Timeouts and (Timeouts - 1)) = 0 then
         UFS_Log.Note ("io timeout" & UFS_Log.Field ("slot", Unsigned_64 (Item)) &
                       UFS_Log.Field ("aborted", Boolean'Pos (Aborted)) &
                       UFS_Log.Field ("count", Timeouts));
      end if;
      if Requests (Item).Kind = Cache_Flush then
         Answer (Item, Status_Reply (False, 0));
      else
         Settle (Requests (Item).Granted, 0, Response);
         Answer (Item, Response);
      end if;
   end Expire;

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

   procedure Respond (To : Process_ID; Response : Message) is
      Ignored : Unsigned_64;
   begin
      Ignored := reply (To, Response);
   end Respond;

   --  Saves the sender's reply capability for slot Free. False: it could
   --  not be saved (the caller answers at once).
   function Keep_Reply (Free : Slot) return Boolean is
      Saved : constant CuBit.Handles.IPC.Saved_Reply :=
        CuBit.Handles.IPC.Save_Reply;
   begin
      if Saved.Saved then
         Requests (Free).Reply := Saved.Handle;
      end if;
      return Saved.Saved;
   end Keep_Reply;

   procedure Begin_Work
     (Free : Slot; Kind : Work_Kind; Command : SCSI.Command_Block) is
   begin
      Requests (Free).Kind := Kind;
      Requests (Free).Command := Command;
      Requests (Free).Started := CuBit.Busy_Poll.Now;
      Requests (Free).Busy := True;
      Start_SCSI (Free, Unit, Command);
   end Begin_Work;

   --  Takes one request into Free. Found: there was one.
   procedure Accept_Request (Free : Slot; Found : out Boolean) is
      Sender : Process_ID;
      Request : Message;
      Admitted : Boolean;
      Response : Message;
   begin
      Poll_Service_Request (Sender, Request, Found);
      if not Found then
         return;
      end if;
      declare
         --  The unit as this sender may use it: writes only through the
         --  write grant's badge (the kernel stamps authorityTag).
         View : constant Device_State :=
           Granted_View (Device, Authority_Of (Request.authorityTag));
         Decoded : constant Decoded_Request := Decode (Request, View);
      begin
         case Decoded.Kind is
            when Describe =>
               Respond (Sender, Describe_Reply (View));
            when Rejected =>
               if Request.tag.label = OP_WRITE_BLOCKS
                 and then Authority_Of (Request.authorityTag) = Read_Only_Access
               then
                  Refused_Writes := Refused_Writes + 1;
                  --  Counted, not flooded: the first, then each doubling.
                  if (Refused_Writes and (Refused_Writes - 1)) = 0 then
                     UFS_Log.Note ("write refused: no write grant" &
                                   UFS_Log.Field ("count", Refused_Writes));
                  end if;
               end if;
               Respond (Sender, Status_Reply (False, 0));
            when Flush =>
               if not Keep_Reply (Free) then
                  Respond (Sender, Status_Reply (False, 0));
                  return;
               end if;
               Begin_Work (Free, Cache_Flush, SCSI.Synchronize_Cache);
            when Transfer =>
               Admit (Sender, Decoded.Data, Requests (Free).Granted, Admitted);
               if not Admitted then
                  Respond (Sender, Status_Reply (False, 0));
                  return;
               end if;
               if not Keep_Reply (Free) then
                  Settle (Requests (Free).Granted, 0, Response);
                  Respond (Sender, Status_Reply (False, 0));
                  return;
               end if;
               if Decoded.Data.Direction = Device_Write then
                  --  The controller only sees the slot's bounce buffer.
                  declare
                     Copied : System.Address;
                     pragma Unreferenced (Copied);
                  begin
                     Copied := CuBit.String.memcpy
                       (UFS_Controller.Buffer (Free),
                        Requests (Free).Granted.Buffer,
                        System.Storage_Elements.Storage_Count
                          (Bytes (Decoded.Data)));
                  end;
                  Begin_Work (Free, Block_Transfer, Write_Command (Decoded.Data));
               else
                  Begin_Work (Free, Block_Transfer, Read_Command (Decoded.Data));
               end if;
         end case;
      end;
   end Accept_Request;

   procedure Poll (Progressed : out Boolean) is
      Found : Boolean;
   begin
      Progressed := False;
      for Item in Slot loop
         if Requests (Item).Busy then
            if UFS_Controller.Completed (Item) then
               Complete (Item);
               Progressed := True;
            elsif not CuBit.Busy_Poll.Within
                        (Requests (Item).Started, Command_Microseconds)
            then
               Expire (Item);
               Progressed := True;
            end if;
         end if;
      end loop;
      for Item in Slot loop
         if not Requests (Item).Busy then
            Accept_Request (Item, Found);
            exit when not Found;
            Progressed := True;
         end if;
      end loop;
   end Poll;
end UFS_Service;
