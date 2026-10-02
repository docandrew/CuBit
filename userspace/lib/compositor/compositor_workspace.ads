-- Logical desktop bounds are not a pixel allocation. Keep coordinate/stride
-- arithmetic representable independently of any physical output's storage.
package Compositor_Workspace with SPARK_Mode, Pure is
   Maximum_Extent : constant := 65_535;
   function Valid (Width, Height : Natural) return Boolean is
     (Width in 1 .. Maximum_Extent and then Height in 1 .. Maximum_Extent and then
      Width <= (Natural'Last / 4) / Height)
     with Post => (if Valid'Result then
       Long_Long_Integer (Width) * Long_Long_Integer (Height) * 4 <= Long_Long_Integer (Natural'Last));
   function Pitch (Width : Positive) return Positive is (Width * 4)
     with Pre => Width <= Maximum_Extent,
       Post => Pitch'Result = Width * 4;
end Compositor_Workspace;
