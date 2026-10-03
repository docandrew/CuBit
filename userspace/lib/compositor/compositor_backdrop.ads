with Interfaces;
with CuBit.Display_Geometry;
with Compositor_Image_Sampling;
-- Physical-output backdrop placement, independent of desktop DPI and origin.
-- This descriptor carries geometry only, never pixels or import authority.
package Compositor_Backdrop with SPARK_Mode, Pure is
   package G renames CuBit.Display_Geometry;
   package S renames Compositor_Image_Sampling;
   subtype Word is Interfaces.Unsigned_32;
   subtype Wide is Interfaces.Unsigned_64;
   subtype Signed is Interfaces.Integer_64;
   use type Word, Wide, Signed, Interfaces.Integer_32;
   subtype Origin is Signed range -Signed (S.Maximum_Draw) .. Signed (S.Maximum_Draw);
   subtype Draw_Size is Wide range 1 .. Wide (S.Maximum_Draw);
   -- Nonnegative signed fields have the same 32-bit C representation here.
   -- Signed arithmetic keeps clipping proofs in integer theory.
   subtype Pixel_Word is Interfaces.Integer_32 range 0 .. 65_535;
   subtype Source_Size is Word range 1 .. 65_535;
   type Draw is record
      Left, Top : Origin := 0;
      Width, Height : Draw_Size := 1;
      Clip_X, Clip_Y, Clip_W, Clip_H : Pixel_Word := 0;
      Source_W, Source_H : Source_Size := 1;
   end record with Convention => C;
   function Valid (D : Draw; W, H : G.Physical_Extent) return Boolean is
     (D.Clip_W /= 0 and then D.Clip_H /= 0 and then
      Natural (D.Clip_X) + Natural (D.Clip_W) <= Natural (W) and then
      Natural (D.Clip_Y) + Natural (D.Clip_H) <= Natural (H));
   type Result (Visible : Boolean := False) is record
      case Visible is
         when True => Value : Draw;
         when False => null;
      end case;
   end record;
   function Plan
     (W, H, Source_W, Source_H : G.Physical_Extent;
      Mode : S.Placement; Damage : G.Physical_Rectangle) return Result
     with Post => (if Plan'Result.Visible then
       Valid (Plan'Result.Value, W, H) and then
       Plan'Result.Value.Source_W = Word (Source_W) and then
       Plan'Result.Value.Source_H = Word (Source_H));
end Compositor_Backdrop;
