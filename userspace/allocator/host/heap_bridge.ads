with Interfaces.C;
with System;
-- Hosted benchmark adapter, NOT the native runtime or verified address boundary.
package Heap_Bridge is
   procedure Initialize with Export, Convention => C, External_Name => "ca_init";
   function Allocate (Size : Interfaces.C.size_t) return System.Address
     with Export, Convention => C, External_Name => "ca_malloc";
   procedure Release (Pointer : System.Address)
     with Export, Convention => C, External_Name => "ca_free";
   function Usable_Size (Pointer : System.Address) return Interfaces.C.size_t
     with Export, Convention => C, External_Name => "ca_usable_size";
   function Reserved_Bytes return Interfaces.C.size_t
     with Export, Convention => C, External_Name => "ca_reserved_bytes";
   function Metadata_Bytes return Interfaces.C.size_t
     with Export, Convention => C, External_Name => "ca_metadata_bytes";
end Heap_Bridge;
