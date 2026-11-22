with Interfaces;
package Intel_GPU_Metadata_Initialize is
   -- CPU-owned, writable committed metadata only; never MMIO or GPU backing.
   -- The caller establishes mapping ownership before this bounded operation.
   function Clear (Address, Bytes : Interfaces.Unsigned_64) return Boolean;
end Intel_GPU_Metadata_Initialize;
