with Interfaces;
with Intel_GPU_Display_Topology;
generic
   Item : Intel_GPU_Display_Topology.Pipe;
package Intel_GPU_Native_Pipe is
   -- Initial boot only: no Intel IRQ handler has been registered and the
   -- broker has disabled PCI interrupt sources. Not a runtime IRQ-drain API.
   -- Inputs are trusted retained driver state, not flags accepted over IPC.
   -- IRQ_Page_Ready requires the dedicated 0x44000 writable page mapping.
   -- C/D additionally require the completed native DC-off transition's
   -- retained reference, not merely a sampled clear DC enable register.
   function Acquire
     (Owner, IRQ_Page_Ready, Upstream_Blocked : Boolean;
      Parent_References : Interfaces.Unsigned_64) return String;
   function Held return Boolean;
end Intel_GPU_Native_Pipe;
