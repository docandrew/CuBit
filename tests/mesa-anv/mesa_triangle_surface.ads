with Interfaces; use Interfaces;
package Mesa_Triangle_Surface is
   function Desktop_Slot return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_test_desktop_slot";
   function Create (Width, Height : Unsigned_32) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_test_triangle_create";
   function Present (Surface : Unsigned_64; Width, Height : Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_test_triangle_present";
   function Destroy (Surface : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_test_triangle_destroy";
end Mesa_Triangle_Surface;
