package body Intel_GPU_Probe_Export is
   package Q renames Native_GPU_Probe_Protocol;
   package V renames Intel_GPU_Buffer_Views;
   use type V.View_State;

   function Pixels return Intel_GPU_Buffer_Reply.Backing is
      Result : constant Intel_GPU_Buffer_Reply.Backing := Completed_Backing;
   begin
      if not Intel_GPU_Buffer_Reply.Valid (Result) or else
        Result.Bytes /= Q.Pixel_Bytes then return (Ready => False); end if;
      return Result;
   end Pixels;
   procedure Share is new V.Share_Completed (Pixels);

   procedure Reject_Delivery (Object : in out Export_State) is
   begin
      V.Retire (Object.View);
   end Reject_Delivery;

   procedure Process_Request
     (Object : in out Export_State; Sender, Stamp : Unsigned_64;
      Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Q.Words; Response : out Q.Words) is
      Slot : CuBit.Messages.CapabilitySlot;
      Identity : Unsigned_64;
   begin
      Response := Q.Reply (Q.Invalid_Request);
      if Label /= Q.Label or else Length /= 4 or else Flags /= 0 or else
        Reserved /= 0 or else not Q.Valid_Request (Request) then return; end if;
      Response := Q.Reply (Q.Denied);
      Recipient (Sender, Stamp, Slot, Identity);
      if Identity = 0 then return; end if;
      if V.State (Object.View) /= V.Empty and then
        (Object.Sender /= Sender or else Object.Stamp /= Stamp or else
         Object.Identity /= Identity or else Object.Slot /= Slot)
      then return; end if;
      Response := Q.Reply (Q.Unavailable);
      if Request (1) = 0 then
         if V.State (Object.View) = V.Empty then
            Object.Sender := Sender; Object.Stamp := Stamp;
            Object.Identity := Identity; Object.Slot := Slot;
            Share (Object.View, Slot, Identity);
            Object.Reference := V.Wire_Reference (Object.View);
            -- Revalidate admission after sharing; callbacks may pump events.
            Recipient (Sender, Stamp, Slot, Identity);
            if Identity = 0 or else Identity /= Object.Identity or else
              Slot /= Object.Slot then
               Reject_Delivery (Object); return;
            end if;
         end if;
         if V.State (Object.View) = V.Shared then
            Response := Q.Reply (Q.Success, Object.Reference);
         end if;
      else
         if Object.Reference = 0 or else Request (2) /= Object.Reference then
            Response := Q.Reply (Q.Denied); return;
         end if;
         V.Retire (Object.View);
         if V.State (Object.View) = V.Retired then
            Response := Q.Reply (Q.Success);
         elsif V.State (Object.View) = V.Retiring then
            Response := Q.Reply (Q.Pending);
         end if;
      end if;
   end Process_Request;

   procedure Handle
     (Object : in out Export_State; Sender, Stamp : Unsigned_64;
      Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Q.Words; Response : out Q.Words) is
   begin
      Response := Q.Reply (Q.Unavailable);
      if Object.Busy then return; end if;
      Object.Busy := True;
      Process_Request (Object, Sender, Stamp, Label, Length, Flags, Reserved,
                       Request, Response);
      Object.Busy := False;
   end Handle;
end Intel_GPU_Probe_Export;
