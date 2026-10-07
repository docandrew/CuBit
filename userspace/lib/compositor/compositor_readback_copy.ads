with System;
with Compositor_Row_Copy;
-- Trusted in-process boundary: caller holds completed GPU readback and an
-- exclusive output writer. Capacities describe accessible, nonaliasing memory.
package Compositor_Readback_Copy with SPARK_Mode => Off is
   Maximum_Batch_Bytes : constant := 256 * 1024;
   procedure Copy
     (Source, Target : System.Address;
      Width, Height, First_Row : Compositor_Row_Copy.G.Pixel_Edge;
      Source_Bytes, Target_Bytes, Target_Pitch, Byte_Budget : Natural;
      Copied_Rows : out Natural);
   -- Zero rows: no writes. Reject virtual overlap/wrap before copying.
   -- Never write padding, publish a frame, or retire either owner here.
end Compositor_Readback_Copy;
