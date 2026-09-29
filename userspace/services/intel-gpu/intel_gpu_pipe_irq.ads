with Interfaces;
with Intel_GPU_Display_Topology;
generic
   Item : Intel_GPU_Display_Topology.Pipe;
   -- Serialized, bounded, nonraising ordered MMIO callbacks. Success includes
   -- transport validity; all-ones IMR is a legitimate fully masked value.
   with procedure Read_32 (Offset : Interfaces.Unsigned_32;
                           Value : out Interfaces.Unsigned_32; Success : out Boolean);
   with procedure Write_32 (Offset, Value : Interfaces.Unsigned_32;
                            Success : out Boolean);
package Intel_GPU_Pipe_IRQ is
   -- ADL-N display13 specialization: absent plane6/7 IMR bits17/18 are
   -- excluded from readback equality. All other IMR bits remain checked;
   -- IER must still read zero and IIR must drain to zero. Not a generic
   -- acceptance mask for older seven-plane display engines.
   type Phase is (Fresh, Uncertain, Masked);
   function State return Phase;
   function Last_Read_Offset return Interfaces.Unsigned_32;
   function Last_Read_Value return Interfaces.Unsigned_32;
   function Expected_Value return Interfaces.Unsigned_32;
   type Result is (Rejected, Write_Failed, Read_Failed, Verify_Failed,
                   Pending_Events, Complete);
   -- Trusted prerequisites, not client-supplied assertions: exclusive device
   -- and display owner, live pipe-power reference, and independently blocked
   -- upstream delivery with in-flight handlers drained. Does not establish
   -- those prerequisites, mask other sources or acquire a power reference.
   -- One-shot; any attempted write leaves state Uncertain until all checks
   -- pass. Never restore inherited enables or release resources after failure.
   procedure Quiesce
     (Owner_Ready, Power_Ready, Delivery_Blocked : Boolean; Status : out Result);
end Intel_GPU_Pipe_IRQ;
