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

   type Request_Record is record
      Busy    : Boolean := False;
      Granted : Loan;
      Reply   : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
      Command : SCSI.Command_Block;
      Started : Unsigned_64 := 0;
   end record;

   Requests : array (Slot) of Request_Record;
   Device : Device_State := (Presence => Faulted);
   Unit : UFS_UPIU.Logical_Unit := 0;
   Timeouts : Unsigned_64 := 0;

   procedure Start (Device : UFS_Identify.Identity) is
   begin
      Unit := Device.Unit;
      UFS_Service.Device :=
        (Presence      => Available,
         Blocks        => Device.Blocks,
         Logical_Size  => Device.Block_Size,
         Physical_Size => Device.Block_Size,
         Max_Transfer  => Buffer_Bytes / Unsigned_64 (Device.Block_Size),
         Features      => FEATURE_READ_ONLY,
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
      Expected : constant Unsigned_64 := Bytes (Current.Granted.Item);
      Moved : Unsigned_64 := 0;
      Response : Message;
      Copied : System.Address;
      pragma Unreferenced (Copied);
   begin
      if Outcome.Result = Passed and then Unsigned_64 (Outcome.Moved) = Expected
      then
         --  Only a whole, successful transfer reaches the caller's grant.
         Copied := CuBit.String.memcpy
           (Current.Granted.Buffer, UFS_Controller.Buffer (Item),
            System.Storage_Elements.Storage_Count (Expected));
         Moved := Expected;
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
      Settle (Requests (Item).Granted, 0, Response);
      Answer (Item, Response);
   end Expire;

   function Read_Command (Item : Transfer_Request) return SCSI.Command_Block is
     (if Item.First_Block <= Unsigned_64 (Unsigned_32'Last)
         and then Item.Count <= Unsigned_64 (Unsigned_16'Last)
      then SCSI.Read_10 (Unsigned_32 (Item.First_Block),
                         SCSI.Block_Count_10 (Item.Count), Item.Block_Bytes)
      else SCSI.Read_16 (Item.First_Block, SCSI.Block_Count_16 (Item.Count),
                         Item.Block_Bytes));

   procedure Respond (To : Process_ID; Response : Message) is
      Ignored : Unsigned_64;
   begin
      Ignored := reply (To, Response);
   end Respond;

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
         Decoded : constant Decoded_Request := Decode (Request, Device);
      begin
         case Decoded.Kind is
            when Describe =>
               Respond (Sender, Describe_Reply (Device));
            when Flush | Rejected =>
               Respond (Sender, Status_Reply (False, 0));
            when Transfer =>
               --  Decode refuses writes: the device is read only.
               Admit (Sender, Decoded.Data, Requests (Free).Granted, Admitted);
               if not Admitted then
                  Respond (Sender, Status_Reply (False, 0));
                  return;
               end if;
               declare
                  Saved : constant CuBit.Handles.IPC.Saved_Reply :=
                    CuBit.Handles.IPC.Save_Reply;
               begin
                  if not Saved.Saved then
                     Settle (Requests (Free).Granted, 0, Response);
                     Respond (Sender, Status_Reply (False, 0));
                     return;
                  end if;
                  Requests (Free).Reply := Saved.Handle;
               end;
               Requests (Free).Command := Read_Command (Decoded.Data);
               Requests (Free).Started := CuBit.Busy_Poll.Now;
               Requests (Free).Busy := True;
               Start_SCSI (Free, Unit, Requests (Free).Command);
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
