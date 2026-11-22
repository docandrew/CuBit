with Interfaces; use Interfaces;
generic
   -- Trusted caller has mapped Reset_Pages UC/NX and continuously holds the
   -- ADL-N invalidator's device/forcewake/reset/exclusion/drain prerequisites.
   with function Owner_Ready return Boolean;
   -- Separate domains; admitting GuC must not broaden an RCS/OA adapter.
   GuC_Only : Boolean := False;
package Intel_GPU_Native_TLB_IO is
   function Address_For (Offset : Unsigned_32) return Unsigned_64;
   procedure Read_Register (Offset : Unsigned_32; Value : out Unsigned_32;
                            OK : out Boolean);
   procedure Write_Register (Offset, Value : Unsigned_32; OK : out Boolean);
   function Failed return Boolean;
   -- Only GFX/OA words (default) OR GuC alone, and request value1;
   -- no read-modify-write.
   -- Any rejection/lost ownership latches failure for this adapter lifetime.
   -- This adapter does not acquire authority or recover an MMIO bus fault.
end Intel_GPU_Native_TLB_IO;
