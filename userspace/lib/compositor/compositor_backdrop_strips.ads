with Compositor_Image_Sampling;
-- Fixed scratch metadata for software wallpaper; no pixel storage or pointers.
package Compositor_Backdrop_Strips with SPARK_Mode, Pure is
   package S renames Compositor_Image_Sampling;
   use type S.Axis_Sample;
   Width : constant := 64;
   subtype Slot is Natural range 0 .. Width - 1;
   subtype Count is Positive range 1 .. Width;
   type Samples is array (Slot) of S.Axis_Sample;
   function Prepare (Plan : S.Layout; First : S.Index; Length : Count) return Samples
     with Pre => First + Length <= S.Extent'Last,
       Post => (for all I in Slot =>
         (if I < Length then Prepare'Result (I) =
             S.Horizontal (Plan, S.Position ((First + I) * 256))
          else not Prepare'Result (I).Valid));
end Compositor_Backdrop_Strips;
