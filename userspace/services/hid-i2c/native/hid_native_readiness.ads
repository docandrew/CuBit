pragma Ada_2022;
with CuBit.Handles;
with HID_Startup_Readiness;
generic
   Target : CuBit.Handles.Endpoint;
package HID_Native_Readiness is
   procedure Send (Item : HID_Startup_Readiness.Outgoing;
      Deadline : HID_Startup_Readiness.W.Millisecond; Acknowledged : out Boolean);
   -- Synchronous only down to devmgr's readiness handler, which must not call
   -- this driver back. Finite explicit deadline; no implicit retry on timeout.
   procedure Decline_Without_Clock (Queued : out Boolean);
   -- Best effort decline only; accepted submission is not acknowledgement.
   -- No completion requested, no report of readiness without a valid clock.
end HID_Native_Readiness;
