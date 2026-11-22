with System;
with Compositor_Row_Copy;
-- Trusted in-process boundary: caller holds completed GPU readback and an
-- exclusive output writer. Capacities describe accessible, nonaliasing memory.
package Compositor_Readback_Copy with SPARK_Mode is
   -- No Ada globals; Target's raw bytes are written synchronously. Mapping,
   -- exclusivity and physical non-aliasing remain caller/FFI obligations.
   Maximum_Batch_Bytes : constant := 256 * 1024;
   procedure Copy
     (Source, Target : System.Address;
      Width, Height, First_Row : Compositor_Row_Copy.G.Pixel_Edge;
      Source_Bytes, Target_Bytes, Target_Pitch, Byte_Budget : Natural;
      Copied_Rows : out Natural)
     with Global => null,
       Post => Copied_Rows <= Natural (Height) -
         Natural'Min (Natural (First_Row), Natural (Height));
   -- Copies only Repair, with row progress relative to Repair.Top. Same
   -- ownership and mapping obligations as Copy; budget capped at 256 KiB.
   procedure Copy_Region
     (Source, Target : System.Address;
      Width, Height, First_Row : Compositor_Row_Copy.G.Pixel_Edge;
      Repair : Compositor_Row_Copy.G.Physical_Rectangle;
      Source_Bytes, Target_Bytes, Target_Pitch, Byte_Budget : Natural;
      Copied_Rows : out Natural)
     with Global => null,
       Post => (if Copied_Rows > 0 then
         Compositor_Row_Copy.Wide (First_Row) + Compositor_Row_Copy.Wide (Copied_Rows) <=
           Compositor_Row_Copy.Wide (Repair.Bottom) - Compositor_Row_Copy.Wide (Repair.Top));
   -- Zero rows: no writes. Reject virtual overlap/wrap before copying.
   -- Never write padding, publish a frame, or retire either owner here.
end Compositor_Readback_Copy;
