with Interfaces;
package Intel_GPU_PHY_Mapping is
   function Ready return Boolean;
   -- Trusted local ownership and BAR, not IPC-client arguments. Ready means
   -- all three mappings exist; it grants no power reference or restore success.
   -- One attempt, no partial retry/unmap, including lost broker replies.
   function Prepare (Owner : Boolean; Register_Base : Interfaces.Unsigned_64)
     return String;
end Intel_GPU_PHY_Mapping;
