------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Driver_Runtime; use CuBit.Block_Driver_Runtime;

package body CuBit.Block_Driver is

   type Request_Record is record
      Busy    : Boolean := False;
      Work    : Work_Order;
      Reply   : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
      Started : Unsigned_64 := 0;
   end record;

   Requests : array (Slot) of Request_Record;
   Device : Device_State := (Presence => Faulted);
   Device_Identity : CuBit.Device_Identities.Identity;
   Expired : Unsigned_64 := 0;
   Refused : Unsigned_64 := 0;

   procedure Count (Event : Driver_Event; Counter : in out Unsigned_64);
   function Result_Reply (Work : Work_Order; Outcome : Work_Outcome)
     return Message;
   procedure Finish (Item : Slot; Outcome : Work_Outcome);
   procedure Complete (Item : Slot);
   procedure Expire (Item : Slot);
   procedure Identify (Sender : Process_ID; Item : Transfer_Request);
   procedure Begin_Work (Free : Slot; Sender : Process_ID; Work : Work_Order);
   procedure Take (Free : Slot; Sender : Process_ID; Request : Message);
   procedure Accept_Request (Free : Slot; Found : out Boolean);

   --  Counted, not flooded: the first, then each doubling.
   procedure Count (Event : Driver_Event; Counter : in out Unsigned_64) is
   begin
      Counter := Counter + 1;
      if (Counter and (Counter - 1)) = 0 then
         Note (Event, Counter);
      end if;
   end Count;

   procedure Start
     (State    : Device_State;
      Identity : CuBit.Device_Identities.Identity) is
   begin
      Device := State;
      Device_Identity := Identity;
   end Start;

   function Outstanding return Boolean is
     (for some Item of Requests => Item.Busy);

   function Expired_Commands return Unsigned_64 is (Expired);
   function Refused_Writes return Unsigned_64 is (Refused);

   --  The reply for a collected command; a transfer's grant goes back.
   function Result_Reply (Work : Work_Order; Outcome : Work_Outcome)
     return Message
   is
      Response : Message;
   begin
      case Work.Kind is
         when Cache_Flush =>
            Response := Status_Reply (Outcome.Passed, 0);
         when Block_Transfer =>
            Settle
              (Work.Granted,
               (if Outcome.Passed or else Outcome.Moved < Bytes (Work.Granted.Item)
                then Outcome.Moved else 0),
               Response);
      end case;
      return Response;
   end Result_Reply;

   procedure Finish (Item : Slot; Outcome : Work_Outcome) is
   begin
      Answer (Requests (Item).Reply, Result_Reply (Requests (Item).Work, Outcome));
      Requests (Item) := (others => <>);
   end Finish;

   procedure Complete (Item : Slot) is
      Outcome : Work_Outcome;
   begin
      Collect (Item, Requests (Item).Work, Outcome);
      Finish (Item, Outcome);
   end Complete;

   procedure Expire (Item : Slot) is
   begin
      Cancel (Item);
      Count (Command_Expired, Expired);
      Finish (Item, (Passed => False, Moved => 0));
   end Expire;

   --  OP_IDENTIFY_DEVICE: the identity record into the caller's grant at
   --  once (no device command), the rest of the block zero.
   procedure Identify (Sender : Process_ID; Item : Transfer_Request) is
      package DId renames CuBit.Device_Identities;
      Granted : Loan;
      Admitted : Boolean;
      Response : Message;
   begin
      Admit (Sender, Item, Granted, Admitted);
      if not Admitted then
         Respond (Sender, Status_Reply (False, 0));
         return;
      end if;
      declare
         Block : DId.Byte_Array (1 .. Natural (Bytes (Item)))
           with Import, Address => Granted.Buffer;
      begin
         Block := [others => 0];
         Block (1 .. DId.Record_Bytes) := DId.Encode (Device_Identity);
      end;
      Settle (Granted, Bytes (Item), Response);
      Respond (Sender, Response);
   end Identify;

   --  Starts Work on Free. Finished at once: answered now. Otherwise the
   --  sender's reply is kept for later; if it cannot be, the command is
   --  cancelled and the request answered as failed.
   procedure Begin_Work (Free : Slot; Sender : Process_ID; Work : Work_Order) is
      Kept : Boolean;
      Outcome : Work_Outcome;
      Response : Message;
   begin
      Requests (Free).Work := Work;
      Requests (Free).Started := Now;
      Post (Free, Work);
      if Finished (Free) then
         Collect (Free, Work, Outcome);
         Respond (Sender, Result_Reply (Work, Outcome));
         Requests (Free) := (others => <>);
         return;
      end if;
      Keep_Reply (Requests (Free).Reply, Kept);
      if Kept then
         Requests (Free).Busy := True;
      else
         Cancel (Free);
         if Work.Kind = Block_Transfer then
            Settle (Work.Granted, 0, Response);
         end if;
         Respond (Sender, Status_Reply (False, 0));
         Requests (Free) := (others => <>);
      end if;
   end Begin_Work;

   --  Handles Request from Sender, using slot Free if it needs the device.
   procedure Take (Free : Slot; Sender : Process_ID; Request : Message) is
   begin
      declare
         --  The device as this sender may use it: writes only through the
         --  write grant's badge (the kernel stamps authorityTag).
         Authority : constant Write_Authority :=
           (case Writes is
              when Write_Grant_Required => Authority_Of (Request.authorityTag),
              when Any_Client_Writes => Writes_Granted);
         View : constant Device_State := Granted_View (Device, Authority);
         Decoded : constant Decoded_Request := Decode (Request, View);
         Identity_Request : constant Identify_Request :=
           Decode_Identify (Request, View);
      begin
         if Identity_Request.Valid then
            Identify (Sender, Identity_Request.Data);
            return;
         end if;
         case Decoded.Kind is
            when Describe =>
               Respond (Sender, Describe_Reply (View));
            when Rejected =>
               if Request.tag.label = OP_WRITE_BLOCKS
                 and then Authority = Read_Only_Access
               then
                  Count (Write_Refused, Refused);
               end if;
               Respond (Sender, Status_Reply (False, 0));
            when Flush =>
               Begin_Work (Free, Sender, (Kind => Cache_Flush));
            when Transfer =>
               declare
                  Granted : Loan;
                  Admitted : Boolean;
               begin
                  Admit (Sender, Decoded.Data, Granted, Admitted);
                  if Admitted then
                     Begin_Work
                       (Free, Sender, (Kind => Block_Transfer, Granted => Granted));
                  else
                     Respond (Sender, Status_Reply (False, 0));
                  end if;
               end;
         end case;
      end;
   end Take;

   --  Takes one request into Free. Found: there was one.
   procedure Accept_Request (Free : Slot; Found : out Boolean) is
      Sender : Process_ID;
      Request : Message;
   begin
      Next_Request (Sender, Request, Found);
      if Found then
         Take (Free, Sender, Request);
      end if;
   end Accept_Request;

   procedure Wait_And_Serve is
      Sender : Process_ID;
      Request : Message;
   begin
      Wait_Request (Sender, Request);
      Take (Slot'First, Sender, Request);
   end Wait_And_Serve;

   procedure Poll (Progressed : out Boolean) is
      Found : Boolean;
   begin
      Progressed := False;
      for Item in Slot loop
         if Requests (Item).Busy then
            if Finished (Item) then
               Complete (Item);
               Progressed := True;
            elsif not Within (Requests (Item).Started, Deadline_Microseconds)
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
end CuBit.Block_Driver;
