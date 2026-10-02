with CuBit.Messages;
package GPU_Admission_Probe is
   -- Synthetic GPU controller through real kernel IPC. Client uses a
   -- non-grantable source and must abort; no public rendering is enabled.
   procedure Client (Slot : CuBit.Messages.CapabilitySlot);
   -- Private test bootstrap supplies scoped CSPACE and a grantable source.
   procedure Authorized_Client (Slot : CuBit.Messages.CapabilitySlot;
                                Cross_Process : Boolean := False);
   procedure Dispatch_Client (Slot : CuBit.Messages.CapabilitySlot);
   procedure Memory_Client (Slot : CuBit.Messages.CapabilitySlot);
   procedure Server (Sender : CuBit.Messages.ProcessID;
                     Request : CuBit.Messages.Message);
end GPU_Admission_Probe;
