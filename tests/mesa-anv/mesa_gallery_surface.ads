with Interfaces; use Interfaces;
with System;
package Mesa_Gallery_Surface is
   -- Copy a completed CPU-visible image into the normal retired frame pair.
   -- 0 published, 2 user close, 4 writable frame unavailable, 10..15 failure.
   -- Never retains Source; publication/backing lifetime stays in Frame_Pair.
   function Frame (Source : System.Address; Width, Height, Pitch : Unsigned_32)
     return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_test_gallery_frame";
   function Close return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_test_gallery_close";
   procedure Rate (Milli_FPS : Unsigned_64)
     with Export, Convention => C, External_Name => "cubit_test_gallery_rate";
end Mesa_Gallery_Surface;
