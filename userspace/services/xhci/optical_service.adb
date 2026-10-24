with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.String;
with USB_Optical;
with XHCI;

package body Optical_Service is
   use type USB_Optical.Status_Result;

   --  The read in progress: its held grant and saved reply.
   Deferred_Reply : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
   Pending : Boolean := False;
   Current : Loan;

   Optical_Block_Size : constant Logical_Block_Size :=
     Logical_Block_Size (USB_Optical.Optical_Block_Bytes);

   function State return Device_State is
     (if XHCI.Optical_Block_Count = 0 then (Presence => Faulted)
      else (Presence      => Available,
            Blocks        => XHCI.Optical_Block_Count,
            Logical_Size  => Optical_Block_Size,
            Physical_Size => Optical_Block_Size,
            Max_Transfer  =>
              Unsigned_64 (USB_Optical.Read_Block_Count'Last),
            Features      => FEATURE_READ_ONLY or FEATURE_REMOVABLE,
            Media         => Optical_Media));

   procedure Respond (To : Process_ID; Response : Message) is
      Ignored : Unsigned_64;
   begin
      Ignored := reply (To, Response);
   end Respond;

   --  Answer the deferred read (its saved reply is used up).
   procedure Respond_Deferred (Response : Message) is
      Ignored : Unsigned_64;
   begin
      Ignored := replyCap (Deferred_Reply, Response);
      Deferred_Reply := CuBit.Handles.No_Reply;
   end Respond_Deferred;

   procedure Poll (Progressed : out Boolean) is
      Sender : Process_ID;
      Request : Message;
      Response : Message;
      Found, Done, Accepted : Boolean;
      Result : USB_Optical.Status_Result;
      Copied : System.Address;
      Owner : Process_ID;
   begin
      Progressed := False;
      if Pending then
         XHCI.Poll_Optical_Read (Done, Result, Progressed);
         if Done then
            if Result = USB_Optical.Command_Passed then
               --  The controller never sees caller memory. Only a completely
               --  validated data phase + CSW may be copied into the loan.
               Copied := CuBit.String.memcpy
                 (Current.Buffer, XHCI.Optical_Read_Buffer,
                  System.Storage_Elements.Storage_Count (Bytes (Current.Item)));
            end if;
            Settle
              (Current,
               (if Result = USB_Optical.Command_Passed
                then Bytes (Current.Item) else 0),
               Response);
            Respond_Deferred (Response);
            Pending := False;
            Progressed := True;
         end if;
      end if;

      Poll_Service_Request (Sender, Request, Found);
      if not Found then
         return;
      end if;
      Progressed := True;
      Owner := Registered_Driver (DRIVER_FS);
      if Owner = No_Process or else Sender /= Owner then
         Respond (Sender, Status_Reply (False, 0));
         return;
      end if;
      declare
         Device : constant Device_State := State;
         Decoded : constant Decoded_Request := Decode (Request, Device);
      begin
         case Decoded.Kind is
            when Describe =>
               Respond (Sender, Describe_Reply (Device));
               return;
            when Flush | Rejected =>
               Respond (Sender, Status_Reply (False, 0));
               return;
            when Transfer =>
               --  One read at a time.
               if Pending then
                  Respond (Sender, Status_Reply (False, 0));
                  return;
               end if;
               Admit (Sender, Decoded.Data, Current, Accepted);
               if not Accepted then
                  Respond (Sender, Status_Reply (False, 0));
                  return;
               end if;
         end case;
      end;
      declare
         Saved : constant CuBit.Handles.IPC.Saved_Reply :=
           CuBit.Handles.IPC.Save_Reply;
      begin
         if not Saved.Saved then
            Settle (Current, 0, Response);
            Respond (Sender, Status_Reply (False, 0));
            return;
         end if;
         Deferred_Reply := Saved.Handle;
      end;
      --  Decode bounds the block below Optical_Block_Count (at most 32 bits,
      --  USB_Optical.Decode_Capacity) and the count to Read_Block_Count.
      XHCI.Start_Optical_Read
        (Unsigned_32 (Current.Item.First_Block),
         USB_Optical.Read_Block_Count (Current.Item.Count), Accepted);
      if not Accepted then
         Settle (Current, 0, Response);
         Respond_Deferred (Status_Reply (False, 0));
         return;
      end if;
      Pending := True;
   end Poll;
end Optical_Service;
