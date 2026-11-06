--  The host's stand-in for CuAlloc in this hosted test (the glyph cache
--  the toolkit links allocates through cualloc_allocate / cualloc_free).
with System;
with Interfaces.C;
package CuAlloc_Host is
   function Allocate (Bytes, Alignment : Interfaces.C.size_t) return System.Address
     with Export, Convention => C, External_Name => "cualloc_allocate";
   procedure Free (Item : System.Address)
     with Export, Convention => C, External_Name => "cualloc_free";
end CuAlloc_Host;
