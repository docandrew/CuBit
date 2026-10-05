------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The arithmetic of a producer-owned output stream (CuBit.Streams'
--  layout; docs/c-removal.md): where an entry goes in the ring, the
--  sentinel that skips the end, and how far drop-oldest advances a
--  lagging subscriber.
--
--  @description
--  Indices are free-running 32-bit counts (they wrap). Proved
--  (tests/libc-ada): an entry and its sentinel always lie within the ring,
--  and an entry never straddles its end.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Libc_Stream_Rings with Pure, SPARK_Mode is

   --  CuBit.Streams' ring layout (tests/libc-ada checks these): a header,
   --  its subscriber table at SUBSCRIBER_TABLE_OFF, then entries from
   --  Data_Offset.
   Stream_Magic : constant := 16#5354_5249#;            --  "STRI"
   Stream_Version : constant := 1;
   Header_Size : constant := 128;
   Data_Offset : constant := 128;
   Subscriber_Entry_Size : constant := 12;
   Sentinel_Length : constant := 16#FFFF#;
   HDR_MAGIC            : constant := 16#00#;
   HDR_VERSION          : constant := 16#04#;
   HDR_SUBSCRIBER_COUNT : constant := 16#08#;
   HDR_PRODUCER_IDX     : constant := 16#0C#;
   HDR_CAPACITY         : constant := 16#10#;
   HDR_DEFAULT_TYPE_TAG : constant := 16#14#;
   HDR_OVERFLOW_POLICY  : constant := 16#16#;
   HDR_STREAM_ID        : constant := 16#18#;
   SUBSCRIBER_TABLE_OFF : constant := 16#20#;
   SUB_OFF_PID          : constant := 0;
   SUB_OFF_CURSOR       : constant := 4;
   SUB_OFF_FLAGS        : constant := 8;
   Drop_Oldest : constant := 1;
   --  Entry type tags (CuBit.Streams.TypeTag).
   Text_Line_Tag : constant := 16#0001#;

   Entry_Header_Bytes : constant := 4;      --  length (16 bits), type tag (16)
   Entry_Alignment    : constant := 8;
   Maximum_Capacity   : constant := 2 ** 24;
   subtype Capacity is Positive range Entry_Alignment .. Maximum_Capacity;

   --  An entry's bytes for a payload of Length: header and payload, 8-aligned.
   function Entry_Bytes (Length : Unsigned_32) return Unsigned_32 is
     ((Entry_Header_Bytes + Length + Entry_Alignment - 1) / Entry_Alignment * Entry_Alignment)
   with Pre => Length <= Maximum_Capacity;

   --  Where an entry of Length bytes goes at producer index Producer: its
   --  offset, whether a sentinel is written first (at Sentinel_Offset), and
   --  the producer index the entry starts at.
   procedure Place
     (Producer : Unsigned_32; Size : Capacity; Length : Unsigned_32;
      Offset : out Unsigned_32; Sentinel : out Boolean; Sentinel_Offset : out Unsigned_32;
      Start : out Unsigned_32)
   with Pre => Length in 1 .. Unsigned_32 (Size),
        Post => Offset < Unsigned_32 (Size)
                and then Offset + Length <= Unsigned_32 (Size)
                and then (if Sentinel then Sentinel_Offset + 2 <= Unsigned_32 (Size)
                                           and then Offset = 0);

   --  Drop-oldest: a subscriber at Cursor would lag past the ring once an
   --  entry needs Advance more bytes; its new cursor.
   function Advanced (Producer, Cursor, Advance : Unsigned_32; Size : Capacity)
     return Unsigned_32 is
     (if (Producer - Cursor) > Unsigned_32 (Size)
         or else Advance > Unsigned_32 (Size) - (Producer - Cursor)
      then Cursor + Advance else Cursor);

end CuBit.Libc_Stream_Rings;
