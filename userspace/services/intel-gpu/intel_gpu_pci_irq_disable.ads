with Interfaces;
with Intel_GPU_PCI_Power;
generic
   -- Bounded, nonraising callbacks under one exclusive PCI/device owner.
   -- Snapshot reads must use the same device throughout; word writes must
   -- not use DWORD RMW. Success means transport completion, not CPU IRQ drain.
   with procedure Read_Config
     (Data : out Intel_GPU_PCI_Power.Configuration; Success : out Boolean);
   with procedure Write_Word
     (Offset : Natural; Value : Interfaces.Unsigned_16; Success : out Boolean);
package Intel_GPU_PCI_IRQ_Disable is
   type Phase is (Fresh, Consumed_No_Writes, Uncertain, PCI_Disabled);
   function State return Phase;
   type Result is (Rejected, Read_Failed, Invalid_Device, Invalid_Plan,
                   Configuration_Changed, Write_Failed, Verification_Failed, Complete);
   -- Owner_Ready is trusted exclusive ownership/serialization, never a client
   -- claim. At most five snapshots and three word writes, no rollback/retry.
   -- PCI_Disabled does not block other devices or drain pending CPU interrupts.
   -- Native caller must separately retain ownership, source masks and IRQ state.
   procedure Execute (Owner_Ready : Boolean; Status : out Result);
end Intel_GPU_PCI_IRQ_Disable;
