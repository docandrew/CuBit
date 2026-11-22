with Interfaces;
with Intel_GPU_GuC_CT_Setup;
generic
   -- Includes authenticated running firmware, exclusive mailbox ownership,
   -- and zeroed/flushed, GPU-published CT backing matching the Execute inputs.
   -- Serialized, non-reentrant callbacks; no reset/reuse while registered.
   with function Owner_Ready return Boolean;
   with procedure Exchange
     (Request : Intel_GPU_GuC_CT_Setup.Request; Reply : out Interfaces.Unsigned_32;
      Success : out Boolean);
package Intel_GPU_GuC_CT_Register is
   type Registration is limited private;
   type Result is (Rejected, Ownership_Lost, Transport_Failed,
                   Registration_Refused, Enable_Refused, Complete);
   procedure Execute (Object : in out Registration;
     GPU_Start, Backing_Bytes, Pin_Bias : Interfaces.Unsigned_64;
     Status : out Result);
   function Attempted (Object : Registration) return Boolean;
   function Enabled (Object : Registration) return Boolean;
   function Last_Step (Object : Registration) return Natural;
   function Last_Reply (Object : Registration) return Interfaces.Unsigned_32;
   -- Step1..6: receive/send descriptor, ring, size. Step7: enable.
   -- Once admitted, never retry or release backing after failure: firmware
   -- may retain some addresses or may already have enabled the channel.
private
   type Registration is limited record
      Started : Boolean := False;
      Ready : Boolean := False;
      Step : Natural range 0 .. 7 := 0;
      Reply : Interfaces.Unsigned_32 := 0;
   end record;
end Intel_GPU_GuC_CT_Register;
