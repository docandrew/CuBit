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

   --  As Prepare_Remove (the same record removed, the same refusals,
   --  duplicate names included), also saying which bytes changed:
   --  Changed_First .. Changed_Last, at most four (the preceding record's
   --  span, or the record's inode), so the caller can write just those.
   Maximum_Changed_Bytes : constant := 4;
   procedure Remove_In_Place
     (Data : in out Block_Data; Size : Block_Length;
      Maximum_Inode : Unsigned_32; Name : String;
      Removed : out Unsigned_32; Kind : out Unsigned_8;
      Changed_First, Changed_Last : out Positive;
      Result : out Prepare_Result)
     with Post =>
       (if Result = Prepared then
          Removed in 1 .. Maximum_Inode and
          Changed_First <= Changed_Last and
          Changed_Last <= Size and
          Changed_Last - Changed_First < Maximum_Changed_Bytes and
          (for all I in Data'Range =>
             (if I < Changed_First or I > Changed_Last then Data (I) = Data'Old (I)))
        else Data = Data'Old and Removed = 0);

   --  Point Name's record at New_Inode, of type New_Kind (rename replacing
   --  an existing name, as Linux ext2_set_link does). Old is the inode it
   --  named. Only that record's inode and type bytes change: Changed_First
   --  .. Changed_Last, eight bytes at most. Duplicate names are malformed.
   procedure Retarget
     (Data : in out Block_Data; Size : Block_Length;
      Maximum_Inode : Unsigned_32; Name : String;
      New_Inode : Unsigned_32; New_Kind : Unsigned_8;
      Old : out Unsigned_32; Changed_First, Changed_Last : out Positive;
      Result : out Prepare_Result)
     with Pre => New_Inode in 1 .. Maximum_Inode,
          Post =>
       (if Result = Prepared then
          Old in 1 .. Maximum_Inode and
          Changed_First <= Changed_Last and Changed_Last <= Size and
          Changed_Last - Changed_First < 8 and
          (for all I in Data'Range =>
             (if I < Changed_First or I > Changed_Last then Data (I) = Data'Old (I)))
        else Data = Data'Old and Old = 0);

   --  A directory's first block: point its ".." (the second record, after
   --  ".") at New_Parent. Old is the parent it named. Only those four
   --  inode bytes change.
   procedure Retarget_Parent
     (Data : in out Block_Data; Size : Block_Length;
      Maximum_Inode : Unsigned_32; New_Parent : Unsigned_32;
      Old : out Unsigned_32; Changed_First : out Positive;
      Result : out Prepare_Result)
     with Pre => New_Parent in 1 .. Maximum_Inode,
          Post =>
       (if Result = Prepared then
          Old in 1 .. Maximum_Inode and Changed_First + 3 <= Size and
          (for all I in Data'Range =>
             (if I < Changed_First or I > Changed_First + 3 then
                Data (I) = Data'Old (I)))
        else Data = Data'Old and Old = 0);

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

   --  A block a directory grows by: one unused record (inode 0) spanning
   --  it, as Linux ext2 adds directory blocks.
   procedure Empty_Block (Data : out Block_Data; Size : Block_Length)
     with Pre => Size mod 4 = 0;
end Directory_Blocks;
