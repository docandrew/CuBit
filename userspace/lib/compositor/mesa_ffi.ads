with System;
with Interfaces;
with Compositor_Formats;
--  Trusted foreign boundary: Mesa accesses retained shared memory. Proof of
--  SPARK policy does not prove these imports or Mesa's completion semantics.
package Mesa_FFI with SPARK_Mode => Off is
   subtype Word is Interfaces.Unsigned_32;
   subtype Image is Compositor_Formats.Image;
   subtype Draw is Compositor_Formats.Draw;
   function Create return System.Address
     with Import, Convention => C, External_Name => "cubit_mesa_create";
   function Import_Image (Context : System.Address; Value : access constant Image)
     return System.Address
     with Import, Convention => C, External_Name => "cubit_mesa_import";
   function Render (Context, Target, Source : System.Address;
                    Value : access constant Draw) return Word
     with Import, Convention => C, External_Name => "cubit_mesa_draw";
   function Fill (Context, Target : System.Address; Left, Top, Width, Height, Color : Word) return Word
     with Import, Convention => C, External_Name => "cubit_mesa_fill";
   function Release (Context, Value : System.Address) return Word
     with Import, Convention => C, External_Name => "cubit_mesa_release";
   procedure Destroy (Context : System.Address)
     with Import, Convention => C, External_Name => "cubit_mesa_destroy";
end Mesa_FFI;
