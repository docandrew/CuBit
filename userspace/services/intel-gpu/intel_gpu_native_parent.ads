with Intel_GPU_Display_Topology;
generic
   Item : Intel_GPU_Display_Topology.Request_Well;
package Intel_GPU_Native_Parent is
   -- ADL-N only, after authenticated bootstrap and display-page mapping.
   -- Retains the parent reference permanently during bring-up. Does not
   -- establish pipe/DC-off references or enable access to plane registers.
   -- Only PW1/PW2 are admitted. PW2 additionally requires the trusted local
   -- PW1 Held state; unsupported generic instances reject before any MMIO.
   function Acquire (Owner, Ancestor_Held : Boolean) return String;
   function Held return Boolean;
end Intel_GPU_Native_Parent;
