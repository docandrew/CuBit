with CuBit.Messages;
package GPU_Budget_Probe is
   -- Synthetic native transport test, not the Intel allocator/supervisor.
   procedure Client (Slot : CuBit.Messages.CapabilitySlot);
   procedure Server (Sender : CuBit.Messages.ProcessID;
                     Request : CuBit.Messages.Message);
end GPU_Budget_Probe;
