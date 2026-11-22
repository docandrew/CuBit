pragma Ada_2022;
with Interfaces;

--  Manifest protocol metadata is a request, never an authority grant.
package CuBit.Render_Authority with Pure, SPARK_Mode is
   --  Distinct from the GPU service endpoint used by display ownership.
   Manifest_Request : constant Interfaces.Unsigned_8 := 11;
end CuBit.Render_Authority;
