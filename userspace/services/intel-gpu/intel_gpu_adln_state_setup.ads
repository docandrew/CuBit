with Interfaces; use Interfaces;
package Intel_GPU_ADLN_State_Setup with SPARK_Mode is
   -- Internal fixed probe fragment, NOT a standalone executable batch.
   -- RCS-only initial 3D selection precedes base setup. Batch-start must leave
   -- legacy streamer bit10 clear; reissue dependent state before any draw.
   type Words is array (Natural range 0 .. 40) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   function Build (MOCS : Unsigned_32) return Image;
end Intel_GPU_ADLN_State_Setup;
