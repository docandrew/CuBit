pragma Ada_2022;
with Interfaces; use Interfaces;

--  Pure, bounded ext2 directory-block decoding and rename preparation.
--  No disk access and no address overlays: rejected preparation never changes
--  the supplied block. Persistence is a separate, fallible operation.
package Directory_Blocks with SPARK_Mode => On is
   Maximum_Bytes : constant := 4096;
   subtype Byte_Count is Natural range 0 .. Maximum_Bytes;
   subtype Block_Length is Byte_Count range 8 .. Maximum_Bytes;
   type Block_Data is array (Positive range 1 .. Maximum_Bytes) of Unsigned_8;
   type Prepare_Result is
     (Prepared, Unchanged, Source_Not_Found, Destination_Exists,
      Invalid_Name, Malformed_Block, Insufficient_Space);

   procedure Prepare_Rename
     (Data : in out Block_Data; Size : Block_Length;
      Maximum_Inode : Unsigned_32; Old_Name, New_Name : String;
      Result : out Prepare_Result)
     with Post =>
       (if Result /= Prepared then Data = Data'Old);
end Directory_Blocks;
