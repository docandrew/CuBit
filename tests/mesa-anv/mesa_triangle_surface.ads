with Interfaces; use Interfaces;
with CuBit.Handles.C_ABI;
package Mesa_Triangle_Surface is
   --  Desktop's endpoint (the startup "desktop" entry), for the C fixture.
   function Desktop_Endpoint return CuBit.Handles.C_ABI.C_Endpoint
     with Export, Convention => C, External_Name => "cubit_test_desktop_endpoint";
   function Create (Width, Height : Unsigned_32) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_test_triangle_create";
   function Present (Surface : Unsigned_64; Width, Height : Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_test_triangle_present";
   function Destroy (Surface : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_test_triangle_destroy";
end Mesa_Triangle_Surface;
