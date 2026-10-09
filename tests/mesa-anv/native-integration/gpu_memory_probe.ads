with CuBit.Messages;
package GPU_Memory_Probe is
   procedure Client (Slot, Empty : CuBit.Messages.CapabilitySlot);
   procedure Server (Sender : CuBit.Messages.Process_ID;
                     Request : CuBit.Messages.Message);
end GPU_Memory_Probe;
