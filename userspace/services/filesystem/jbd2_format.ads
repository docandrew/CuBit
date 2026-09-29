with Interfaces; use Interfaces;

--  Value-only decoding of JBD2 (ext3/ext4 journal) on-disk structures, as
--  Linux's jbd2 writes them. Every journal block is untrusted input: the
--  decoders never read outside the block and reject rather than guess. No
--  I/O here. All multi-byte journal fields are big-endian.
package Jbd2_Format with Pure, SPARK_Mode is
   Magic : constant Unsigned_32 := 16#C03B_3998#;

   --  h_blocktype values.
   Descriptor_Kind    : constant Unsigned_32 := 1;
   Commit_Kind        : constant Unsigned_32 := 2;
   Superblock_V1_Kind : constant Unsigned_32 := 3;
   Superblock_V2_Kind : constant Unsigned_32 := 4;
   Revoke_Kind        : constant Unsigned_32 := 5;

   --  Journal superblock features.
   Compat_Checksum       : constant Unsigned_32 := 16#01#; -- crc32 commit v1
   Incompat_Revoke       : constant Unsigned_32 := 16#01#;
   Incompat_64bit        : constant Unsigned_32 := 16#02#;
   Incompat_Async_Commit : constant Unsigned_32 := 16#04#;
   Incompat_Csum_V2      : constant Unsigned_32 := 16#08#;
   Incompat_Csum_V3      : constant Unsigned_32 := 16#10#;
   Incompat_Fast_Commit  : constant Unsigned_32 := 16#20#;

   --  What this implementation replays: every checksum generation (v1 crc32
   --  of whole transactions, v2/v3 crc32c per block), revokes, 64-bit block
   --  numbers and async commit. Fast commits are rejected, not ignored.
   Supported_Compat : constant Unsigned_32 := Compat_Checksum;
   Supported_Incompat : constant Unsigned_32 :=
     Incompat_Revoke or Incompat_64bit or Incompat_Async_Commit or
     Incompat_Csum_V2 or Incompat_Csum_V3;

   --  Descriptor tag flags.
   Tag_Escape    : constant Unsigned_32 := 16#1#; -- data began with Magic
   Tag_Same_UUID : constant Unsigned_32 := 16#2#; -- no UUID follows the tag
   Tag_Deleted   : constant Unsigned_32 := 16#4#;
   Tag_Last      : constant Unsigned_32 := 16#8#;

   Checksum_Type_Crc32  : constant Unsigned_8 := 1; -- v1 commit blocks
   Checksum_Type_Crc32c : constant Unsigned_8 := 4;
   Crc32_Checksum_Bytes : constant Unsigned_8 := 4;

   --  The journal block size equals the filesystem block size.
   Maximum_Block_Bytes : constant := 4096;
   subtype Block_Bytes is Natural range 1024 .. Maximum_Block_Bytes
     with Static_Predicate => Block_Bytes in 1024 | 2048 | 4096;
   subtype Byte_Index is Natural range 0 .. Maximum_Block_Bytes - 1;
   type Block is array (Byte_Index) of Unsigned_8;

   Header_Bytes : constant := 12;
   UUID_Bytes : constant := 16;
   Tail_Bytes : constant := 4; -- descriptor/revoke block checksum
   Revoke_Header_Bytes : constant := 16;
   Superblock_Bytes : constant := 1024;
   Superblock_Checksum_Offset : constant := 16#FC#;
   Commit_Checksum_Type_Offset : constant := 12;
   Commit_Checksum_Size_Offset : constant := 13;
   Commit_Checksum_Offset : constant := 16;
   Maximum_Users : constant := 48;

   function Be32 (Data : Block; Offset : Byte_Index) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Data (Offset)), 24) or
      Shift_Left (Unsigned_32 (Data (Offset + 1)), 16) or
      Shift_Left (Unsigned_32 (Data (Offset + 2)), 8) or
      Unsigned_32 (Data (Offset + 3)))
     with Pre => Offset <= Maximum_Block_Bytes - 4;

   function Be16 (Data : Block; Offset : Byte_Index) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Data (Offset)), 8) or Unsigned_32 (Data (Offset + 1)))
     with Pre => Offset <= Maximum_Block_Bytes - 2;

   type Header is record
      Magic_Value, Kind, Sequence : Unsigned_32;
   end record;

   function Header_Of (Data : Block) return Header is
     ((Magic_Value => Be32 (Data, 0), Kind => Be32 (Data, 4),
       Sequence => Be32 (Data, 8)));

   --  Linux tid_gt: A is later than B in the 32-bit wrapping sequence.
   function Later (A, B : Unsigned_32) return Boolean is
     (A /= B and then A - B < 2 ** 31);

   type UUID is array (0 .. UUID_Bytes - 1) of Unsigned_8;

   type Journal_Superblock is record
      Kind, Journal_Block_Bytes, Max_Length, First, Sequence, Start : Unsigned_32;
      Errno, Compat, Incompat, Ro_Compat, Users : Unsigned_32;
      Checksum_Type : Unsigned_8;
      Identity : UUID;
   end record;

   Empty_Superblock : constant Journal_Superblock :=
     (Kind | Journal_Block_Bytes | Max_Length | First | Sequence | Start |
      Errno | Compat | Incompat | Ro_Compat | Users => 0,
      Checksum_Type => 0, Identity => [others => 0]);

   function Has (Features, Feature : Unsigned_32) return Boolean is
     ((Features and Feature) /= 0);

   function Checksummed (Incompat : Unsigned_32) return Boolean is
     (Has (Incompat, Incompat_Csum_V2) or else Has (Incompat, Incompat_Csum_V3));

   --  Linux journal_tag_bytes: 16 with csum v3; otherwise 12, plus 2 for
   --  csum v2, minus 4 without 64-bit block numbers.
   subtype Tag_Length is Natural range 8 .. 16;
   function Tag_Bytes (Incompat : Unsigned_32) return Tag_Length is
     (if Has (Incompat, Incompat_Csum_V3) then 16
      else 12 + (if Has (Incompat, Incompat_Csum_V2) then 2 else 0) -
           (if Has (Incompat, Incompat_64bit) then 0 else 4));

   --  Bytes of a descriptor or revoke block available to records.
   function Usable_Bytes (Size : Block_Bytes; Incompat : Unsigned_32)
      return Natural is
     (Size - (if Checksummed (Incompat) then Tail_Bytes else 0));

   --  Decode and check a journal superblock for an internal journal of
   --  Journal_Blocks blocks. Valid requires the magic, a v1 or v2 kind, the
   --  filesystem's block size, a log area inside the journal, a Start that is
   --  zero (clean) or inside the log area, only supported features, and for
   --  checksummed journals the crc32c type and a matching superblock
   --  checksum (Stored_Checksum_Matches, computed by the caller).
   procedure Decode_Superblock
     (Data : Block; Size : Block_Bytes; Journal_Blocks : Unsigned_32;
      Stored_Checksum_Matches : Boolean;
      Super : out Journal_Superblock; Valid : out Boolean)
     with Post =>
       (if Valid then
          Super.Journal_Block_Bytes = Unsigned_32 (Size) and then
          Super.First >= 1 and then Super.First < Super.Max_Length and then
          Super.Max_Length <= Journal_Blocks and then
          (Super.Start = 0 or else
           (Super.Start >= Super.First and then Super.Start < Super.Max_Length)) and then
          (Super.Compat and not Supported_Compat) = 0 and then
          (Super.Incompat and not Supported_Incompat) = 0);

   --  One descriptor tag: the home block of the next log block.
   type Block_Tag is record
      Home : Unsigned_64;
      Flags : Unsigned_32;
      Checksum : Unsigned_32; -- 16 bits for csum v2, 32 for v3
   end record;

   --  Decode the tag at Offset in a descriptor block, advancing Offset past
   --  it and its UUID. Found is False when no whole tag fits before Limit.
   --  Offset never moves backwards and never past Limit.
   procedure Next_Tag
     (Data : Block; Limit : Natural; Incompat : Unsigned_32;
      Offset : in out Natural; Tag : out Block_Tag; Found : out Boolean)
     with Pre => Limit <= Maximum_Block_Bytes and then Offset <= Limit,
          Post => Offset >= Offset'Old and then Offset <= Limit and then
                  (if Found then Offset > Offset'Old);

   --  A revoke block's record area: r_count bytes including its 16-byte
   --  header, whole records only, within the usable bytes.
   function Revoke_Record_Bytes (Incompat : Unsigned_32) return Positive is
     (if Has (Incompat, Incompat_64bit) then 8 else 4);

   function Revoke_Count_Valid
     (Data : Block; Size : Block_Bytes; Incompat : Unsigned_32) return Boolean is
     (Be32 (Data, Header_Bytes) >= Revoke_Header_Bytes and then
      Be32 (Data, Header_Bytes) <= Unsigned_32 (Usable_Bytes (Size, Incompat)) and then
      (Be32 (Data, Header_Bytes) - Revoke_Header_Bytes) mod
        Unsigned_32 (Revoke_Record_Bytes (Incompat)) = 0);

   function Revoke_Records
     (Data : Block; Size : Block_Bytes; Incompat : Unsigned_32) return Natural
     with Pre => Revoke_Count_Valid (Data, Size, Incompat),
          Post => Revoke_Records'Result <=
                    (Maximum_Block_Bytes - Revoke_Header_Bytes) / 4 and then
                  Natural (Be32 (Data, Header_Bytes)) <= Maximum_Block_Bytes and then
                  Revoke_Header_Bytes + Revoke_Records'Result *
                    Revoke_Record_Bytes (Incompat) <=
                      Natural (Be32 (Data, Header_Bytes));

   function Revoked_Block
     (Data : Block; Size : Block_Bytes; Incompat : Unsigned_32; Index : Natural)
      return Unsigned_64
     with Pre => Revoke_Count_Valid (Data, Size, Incompat) and then
                 Index < Revoke_Records (Data, Size, Incompat);

   --  Castagnoli CRC32 as Linux's crc32c(): no pre/post inversion; callers
   --  seed it (jbd2 seeds with ~0, then with the journal UUID's crc).
   function Crc32c (Seed : Unsigned_32; Data : Block; First, Length : Natural)
      return Unsigned_32
     with Pre => First <= Maximum_Block_Bytes and then
                 Length <= Maximum_Block_Bytes - First;

   --  Big-endian CRC32 as Linux's crc32_be() (v1 transaction checksums): no
   --  pre/post inversion; jbd2 seeds each transaction with ~0.
   function Crc32_Be (Seed : Unsigned_32; Data : Block; First, Length : Natural)
      return Unsigned_32
     with Pre => First <= Maximum_Block_Bytes and then
                 Length <= Maximum_Block_Bytes - First;

   --  The same over a UUID (the journal checksum seed) and a big-endian word.
   function Crc32c_UUID (Seed : Unsigned_32; Identity : UUID) return Unsigned_32;
   function Crc32c_Be32 (Seed, Value : Unsigned_32) return Unsigned_32;
end Jbd2_Format;
