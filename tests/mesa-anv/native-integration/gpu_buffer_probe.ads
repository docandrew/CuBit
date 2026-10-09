with CuBit.Messages;
package GPU_Buffer_Probe is
   procedure Server
     (Sender : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message);
   procedure Client (Slot : CuBit.Messages.CapabilitySlot);
end GPU_Buffer_Probe;
