------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Unit_Badges;
with CuBit.Block_Requests;
with Optical_Service;
with USB_Disk_Service;
with XHCI;

package body Storage_Requests is
   package Units renames CuBit.Block_Unit_Badges;

   procedure Refuse (Sender : Process_ID) is
      Ignored : Unsigned_64;
   begin
      Ignored := reply (Sender, CuBit.Block_Requests.Status_Reply (False, 0));
   end Refuse;

   procedure Poll (Progressed : out Boolean) is
      Optical_Progress, Disk_Progress, Found : Boolean;
      Sender : Process_ID;
      Request : Message;
      Owner : Process_ID;
   begin
      Optical_Service.Complete (Optical_Progress);
      USB_Disk_Service.Complete (Disk_Progress);
      Progressed := Optical_Progress or else Disk_Progress;
      if Optical_Service.Busy or else USB_Disk_Service.Busy or else XHCI.Storage_Busy then
         return;
      end if;
      Poll_Service_Request (Sender, Request, Found);
      if not Found then
         return;
      end if;
      Progressed := True;
      Owner := Registered_Driver (DRIVER_FS);
      if Owner = No_Process or else Sender /= Owner then
         Refuse (Sender);
      elsif Units.Unit_Of (Request.authorityTag) /= Units.No_Unit then
         USB_Disk_Service.Serve (Sender, Request, Units.Unit_Of (Request.authorityTag));
      elsif Request.authorityTag = 0 and then XHCI.Optical_Selected then
         Optical_Service.Serve (Sender, Request);
      else
         --  Any other badge (the whole-device grant is never minted on
         --  this endpoint), or no boot medium: nothing to reach.
         Refuse (Sender);
      end if;
   end Poll;
end Storage_Requests;
