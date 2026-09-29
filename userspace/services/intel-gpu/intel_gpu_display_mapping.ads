with Interfaces;
package Intel_GPU_Display_Mapping is
   Virtual_Base : constant Interfaces.Unsigned_64 := 16#6140_0000#;
   function Ready return Boolean;
   -- One attempt only, including partial grants/maps and lost replies. No
   -- release/rebind exists during bring-up. Ownership is trusted local state,
   -- not a flag from an IPC client. Success is NOT a hardware power reference.
   function Prepare
     (Owner : Boolean; Register_Base : Interfaces.Unsigned_64) return String;
end Intel_GPU_Display_Mapping;
