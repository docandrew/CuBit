with Interfaces; use Interfaces;
with CuBit.Messages;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Views;
with Native_GPU_Probe_Protocol;

generic
   with function Completed_Backing return Intel_GPU_Buffer_Reply.Backing;
   -- Trusted startup binding, checked against kernel-authenticated caller
   -- metadata. Never derive this slot or identity from request words.
   with procedure Recipient
     (Sender, Stamp : Unsigned_64; Slot : out CuBit.Messages.CapabilitySlot;
      Identity : out Unsigned_64);
package Intel_GPU_Probe_Export is
   type Export_State is limited private;
   procedure Handle
     (Object : in out Export_State; Sender, Stamp : Unsigned_64;
      Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Native_GPU_Probe_Protocol.Words;
      Response : out Native_GPU_Probe_Protocol.Words);
   procedure Reject_Delivery (Object : in out Export_State);
   -- Serialized service owner only. Retain immutable backing throughout;
   -- this object never releases allocations or authorizes render submission.
private
   type Export_State is limited record
      Busy : Boolean := False;
      View : Intel_GPU_Buffer_Views.View;
      Sender, Stamp, Identity, Reference : Unsigned_64 := 0;
      Slot : CuBit.Messages.CapabilitySlot := 0;
   end record;
end Intel_GPU_Probe_Export;
