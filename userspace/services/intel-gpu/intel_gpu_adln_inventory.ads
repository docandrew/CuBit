with Interfaces;
package Intel_GPU_ADLN_Inventory with SPARK_Mode is
   use Interfaces;
   -- Initial policy deliberately admits only the validated N95 device.
   -- Decode requires a successfully read, stable 0x9140 fuse register.
   Fuse_Register : constant Unsigned_32 := 16#9140#;
   type Engine is (Render, Copy, Video_0, Video_2, Enhance_0);
   type Engine_Set is array (Engine) of Boolean;
   type Domain is (GT, Render_Domain, VDBOX_0, VDBOX_2, VEBOX_0);
   type Domain_Set is array (Domain) of Boolean;
   type Inventory is record
      Valid : Boolean := False;
      Engines : Engine_Set := [others => False];
      Domains : Domain_Set := [others => False];
   end record;
   function Decode (Vendor, Device : Unsigned_16; Fuse : Unsigned_32)
     return Inventory;
   function Request_Register (Item : Domain) return Unsigned_32;
   function Ack_Register (Item : Domain) return Unsigned_32;
   function Engine_Base (Item : Engine) return Unsigned_32;
   function Pending_Register (Item : Engine) return Unsigned_32;
   -- Software access policy, not an MMU-enforced sub-page capability.
   -- Only stop, prefetch-disable, prepare, and preparation-cancel writes.
   function Engine_Write_Allowed
     (Description : Inventory; Offset, Value : Unsigned_32) return Boolean;
end Intel_GPU_ADLN_Inventory;
