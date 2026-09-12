with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Memory_Grants;
with CuBit.String;
with USB_Optical;
with XHCI;

package body Optical_Service is
   use type USB_Optical.Status_Result;
   Deferred_Reply : constant CapabilitySlot := 62;
   Pending : Boolean := False;
   Destination : System.Address := System.Null_Address;
   Reference : CuBit.Memory_Grants.Grant_Reference;
   Transfer_Bytes : Unsigned_64 := 0;

   procedure Respond
     (Slot : CapabilitySlot; Success : Boolean; Bytes : Unsigned_64 := 0)
   is
      Ignored : Unsigned_64;
   begin
      Ignored := replyCap
        (Slot, (tag => (label => (if Success then REPLY_OK else REPLY_ERROR),
                        length => 1, flags => 0, reserved => 0),
                authorityTag => 0, words => [0 => Bytes, others => 0]));
   end Respond;

   procedure Poll (Progressed : out Boolean) is
      Sender : ProcessID;
      Request : Message;
      Found, Done, Accepted, Returned : Boolean;
      Result : USB_Optical.Status_Result;
      Ignored : Unsigned_64;
      Copied : System.Address;
      Owner : Unsigned_64;
   begin
      Progressed := False;
      if Pending then
         XHCI.Poll_Optical_Read (Done, Result, Progressed);
         if Done then
            if Result = USB_Optical.Command_Passed then
               --  The controller never sees caller memory. Only a completely
               --  validated data phase + CSW may be copied into the loan.
               Copied := CuBit.String.memcpy
                 (Destination, XHCI.Optical_Read_Buffer,
                  System.Storage_Elements.Storage_Count (Transfer_Bytes));
            end if;
            CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
            Respond (Deferred_Reply,
                     Returned and then Result = USB_Optical.Command_Passed,
                     (if Returned and then Result = USB_Optical.Command_Passed
                      then Transfer_Bytes else 0));
            Pending := False;
            Progressed := True;
         end if;
      end if;

      Poll_Service_Request (Sender, Request, Found);
      if not Found then
         return;
      end if;
      Progressed := True;
      Owner := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_FS);
      if Owner = 0 or else Owner = Unsigned_64'Last or else
        Sender /= Owner or else XHCI.Optical_Block_Count = 0
      then
         Respond (CapabilitySlot'Last, False);
         return;
      end if;
      if Request.tag.label = OP_DESCRIBE_DEVICE and then
        Request.tag.length = 0
      then
         Ignored := replyCap
           (CapabilitySlot'Last,
            (tag => (label => REPLY_OK, length => 4, flags => 0, reserved => 0),
             authorityTag => 0,
             words => [XHCI.Optical_Block_Count, Pack_Sizes (2048, 2048), 16,
                       Pack_Properties (FEATURE_READ_ONLY or FEATURE_REMOVABLE,
                                        Optical_Media)]));
         return;
      end if;
      if Pending or else Request.tag.label /= OP_READ_BLOCKS or else
        Request.tag.length /= 4 or else Request.words (2) not in 1 .. 16 or else
        Request.words (0) > Unsigned_64 (Unsigned_32'Last) or else
        Request.words (0) >= XHCI.Optical_Block_Count or else
        Request.words (2) > XHCI.Optical_Block_Count - Request.words (0) or else
        Request.words (1) > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
        Request.words (3) not in 1 .. CuBit.Memory_Grants.MAXIMUM_GENERATION
      then
         Respond (CapabilitySlot'Last, False);
         return;
      end if;
      Reference :=
        (slot => CuBit.Memory_Grants.Global_Grant_Slot (Request.words (1)),
         generation => CuBit.Memory_Grants.Grant_Generation (Request.words (3)));
      Transfer_Bytes := Request.words (2) *
        Unsigned_64 (USB_Optical.Optical_Block_Bytes);
      CuBit.Memory_Grants.Acquire
        (Reference, Sender, 0, Transfer_Bytes, CuBit.Memory_Grants.Write_Access,
         Destination, Accepted);
      if not Accepted then
         Respond (CapabilitySlot'Last, False);
         return;
      end if;
      if saveReplyCap (Unsigned_64 (Deferred_Reply)) /= 1 then
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         Respond (CapabilitySlot'Last, False);
         return;
      end if;
      XHCI.Start_Optical_Read
        (Unsigned_32 (Request.words (0)),
         USB_Optical.Read_Block_Count (Request.words (2)), Accepted);
      if not Accepted then
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         Respond (Deferred_Reply, False);
         return;
      end if;
      Pending := True;
   end Poll;
end Optical_Service;
