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

   --  Remove Name's record as Linux ext2 does: it merges into the preceding
   --  record of its block or, as the block's first record, becomes unused
   --  (inode 0, span kept). Removed and Kind are its inode and file type.
   procedure Prepare_Remove
     (Data : in out Block_Data; Size : Block_Length;
      Maximum_Inode : Unsigned_32; Name : String;
      Removed : out Unsigned_32; Kind : out Unsigned_8;
      Result : out Prepare_Result)
     with Post =>
       (if Result = Prepared then Removed in 1 .. Maximum_Inode
        else Data = Data'Old and Removed = 0);

   --  Count the live records other than "." and "..". Result is Prepared,
   --  or Malformed_Block (Children then meaningless).
   procedure Count_Children
     (Data : Block_Data; Size : Block_Length; Maximum_Inode : Unsigned_32;
      Children : out Byte_Count; Result : out Prepare_Result)
     with Post => Result in Prepared | Malformed_Block;

   --  Minimum block holding "." and "..".
   Minimum_Directory_Block : constant := 24;

   --  A new directory's first block: "." (Self) then ".." (Parent), the
   --  latter spanning the rest of the block.
   procedure Initial_Block
     (Data : out Block_Data; Size : Block_Length;
      Self, Parent : Unsigned_32; Directory_Kind : Unsigned_8)
     with Pre => Size >= Minimum_Directory_Block and Size mod 4 = 0;
end Directory_Blocks;
