with Interfaces; use Interfaces;
with Intel_GPU_Submission_Image;
package Intel_GPU_Submission_Materialize with SPARK_Mode is
   Byte_Count : constant := Intel_GPU_Submission_Image.Byte_Count;
   type Bytes is array (Natural range <>) of Unsigned_8;
   -- Supply only the private retained submission tail, not the whole firmware
   -- allocation: adjacent firmware/CT storage may already be device-owned.
   -- This copies little-endian words, including all zero padding. It neither
   -- flushes nor publishes memory; the caller owns those ordering obligations.
   procedure Write
     (Image : Intel_GPU_Submission_Image.Image;
      Buffer : in out Bytes; Success : out Boolean)
     with Post =>
       Success = (Image.Valid and Buffer'Length = Byte_Count) and then
       (if not Success then Buffer = Buffer'Old);
   -- Native split preparation: leave the four private VM table pages intact
   -- for the sealed VM writer; write context/ring/batch/completion as usual.
   procedure Write_Non_VM
     (Image : Intel_GPU_Submission_Image.Image;
      Buffer : in out Bytes; Success : out Boolean)
     with Post =>
       Success = (Image.Valid and Buffer'Length = Byte_Count) and then
       (if not Success then Buffer = Buffer'Old);
end Intel_GPU_Submission_Materialize;
