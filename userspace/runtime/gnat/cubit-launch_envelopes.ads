------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A launch block's startup table (CAP-002): where the launcher puts the
--  child's name -> handle table inside the one block the kernel maps.
--
--  @description
--  A launch block is the payload the child already knows (launch
--  arguments from procmgr, or a typed launch configuration from devmgr),
--  then the startup table, then a trailer:
--
--     payload (P bytes) | table (T bytes) | zero pad to a word
--     | u64 T | u64 Magic
--
--  The payload keeps its address (the block's start), so its readers are
--  unchanged: the start code (crt0) finds the trailer and gives them P as
--  the block's length. A block without the trailer has no startup table
--  (the kernel's block for devmgr). Split is proved to return a payload and
--  a table that lie inside the block and to accept exactly what Append
--  produces for tables of at most Maximum_Table_Bytes.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Startup_Tables;

package CuBit.Launch_Envelopes with Pure, SPARK_Mode is

   --  "CUBITLB1" read as a little-endian word.
   Magic         : constant Unsigned_64 := 16#3142_4C54_4942_5543#;
   Word_Bytes    : constant := 8;
   Trailer_Bytes : constant := 2 * Word_Bytes;
   Maximum_Table_Bytes : constant := CuBit.Startup_Tables.Maximum_Table_Bytes;
   --  The most an envelope adds to its payload.
   Maximum_Overhead : constant :=
     (Maximum_Table_Bytes + Word_Bytes - 1) / Word_Bytes * Word_Bytes + Trailer_Bytes;
   Maximum_Block_Bytes : constant := 64 * 1024 + Maximum_Overhead;

   subtype Block_Length is Natural range 0 .. Maximum_Block_Bytes;
   subtype Block_Index is Positive range 1 .. Maximum_Block_Bytes;
   type Block is array (Block_Index range <>) of Unsigned_8;

   subtype Table_Count is Natural range 0 .. Maximum_Table_Bytes;

   function Padded (Length : Table_Count) return Natural is
     ((Length + Word_Bytes - 1) / Word_Bytes * Word_Bytes)
   with Post => Padded'Result in Length .. Length + Word_Bytes - 1
                and then Padded'Result mod Word_Bytes = 0;

   --  Little-endian word at Item (First .. First + 7).
   function Word_At (Item : Block; First : Positive) return Unsigned_64
   with Pre => Item'First = 1 and then First <= Item'Last
               and then Item'Last - First >= Word_Bytes - 1;

   --  Append Table and the trailer after Item (1 .. Length). Fits is False
   --  (and nothing changes) when there is no room.
   procedure Append
     (Item   : in out Block;
      Length : in out Block_Length;
      Table  : CuBit.Startup_Tables.Bytes;
      Fits   : out Boolean)
   with Pre  => Item'First = 1 and then Length <= Item'Last
                and then Table'Length <= Maximum_Table_Bytes,
        Post => (if Fits then
                   Length = Length'Old + Padded (Table'Length) + Trailer_Bytes
                   and then Item (1 .. Length'Old) = Item'Old (1 .. Length'Old)
                 else Length = Length'Old and then Item = Item'Old);

   --  Where the payload and table of Item (1 .. Length) are. Present is
   --  False when there is no valid trailer; then the payload is the whole
   --  block and the table is empty.
   procedure Split
     (Item           : Block;
      Payload_Length : out Block_Length;
      Table_First    : out Positive;
      Table_Bytes    : out Table_Count;
      Present        : out Boolean)
   with Pre  => Item'First = 1,
        Post => Payload_Length <= Item'Length
                and then (if Present then
                            Table_First = Payload_Length + 1
                            and then Payload_Length + Padded (Table_Bytes) + Trailer_Bytes
                                     = Item'Length
                          else Payload_Length = Item'Length and then Table_Bytes = 0);

end CuBit.Launch_Envelopes;
