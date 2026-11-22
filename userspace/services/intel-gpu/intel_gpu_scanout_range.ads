with Interfaces;
package Intel_GPU_Scanout_Range with SPARK_Mode is
   use Interfaces;
   type Extent is record
      Valid : Boolean := False;
      First, Bytes : Unsigned_64 := 0;
   end record;
   -- Pure geometry for an already-decoded LINEAR packed-pixel plane.
   -- Surface is a GGTT byte address, NOT the boot framebuffer CPU address.
   -- Reserve complete rows from Surface, including leading offset rows and
   -- padding, then round outward to GGTT pages. Conservative over-reservation
   -- is intentional. This neither decodes registers nor establishes ownership.
   -- Caller must separately reject tiled/compressed/planar layouts, retain
   -- live AND pending surfaces, and account for every enabled plane/cursor.
   function Linear
     (Table_Bytes, Surface, Pitch, Width, Height, X, Y,
      Pixel_Bytes : Unsigned_64) return Extent
   with Global => null,
     Post => (if Linear'Result.Valid then
       Linear'Result.First = Surface and then
       Surface mod 4096 = 0 and then Linear'Result.Bytes > 0 and then
       Linear'Result.Bytes mod 4096 = 0 and then
       Surface < Table_Bytes / 8 * 4096 and then
       Linear'Result.Bytes <= Table_Bytes / 8 * 4096 - Surface);
end Intel_GPU_Scanout_Range;
