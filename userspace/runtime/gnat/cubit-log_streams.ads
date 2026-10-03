pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Rings;
with CuBit.Log_Protocol;
with CuBit.Log_Records;

--  A reader's log stream: a region the reader lends logstore at Subscribe.
--  Its first page holds the ring's free-running byte indices
--  (CuBit.Channel_Rings), each on its own cache line: logstore writes
--  PRODUCED, the reader writes CONSUMED. The rest is a CuBit.Datagram_Rings
--  ring of entries. logstore produces, the reader consumes; neither trusts
--  the other's index (Channel_Rings' Accept_* checks) or entries (Decode).
--
--  An entry is a 56-byte header (kind, source, node high, node low, observed
--  ms, publisher authority tag, encoded length; little-endian 64-bit words)
--  and, for an event, the encoded record (CuBit.Log_Records wire format).
--  A gap entry says how many events logstore could not keep for this reader
--  (in the observed-ms word) and carries no record.
package CuBit.Log_Streams with Pure, SPARK_Mode is
   package Logs renames CuBit.Log_Records;
   PAGE_BYTES : constant := 4_096;
   CONTROL_BYTES : constant := PAGE_BYTES;
   RING_BYTES : constant CuBit.Channel_Rings.Ring_Size := 65_536;
   STREAM_BYTES : constant := CONTROL_BYTES + 65_536;
   STREAM_PAGES : constant := STREAM_BYTES / PAGE_BYTES;
   PRODUCED_OFFSET : constant := 0;
   CONSUMED_OFFSET : constant := 64;
   --  Page-aligned: it is lent as a grant.
   type Stream_Region is array (Positive range 1 .. STREAM_BYTES) of Unsigned_8
     with Alignment => PAGE_BYTES;

   type Entry_Kind is (Event_Entry, Gap_Entry);
   HEADER_BYTES : constant := 56;
   Maximum_Entry_Bytes : constant := HEADER_BYTES + Natural (Logs.Wire_Count'Last);
   subtype Entry_Length is Natural range HEADER_BYTES .. Maximum_Entry_Bytes;
   subtype Entry_Buffer is CuBit.Channel_Rings.Bytes (0 .. Maximum_Entry_Bytes - 1);
   --  Ring bytes logstore keeps free before taking another event from a
   --  reader's queue: the largest entry, and the pad a wrap may need.
   Room_Needed : constant := 2 * (Maximum_Entry_Bytes + 8);

   procedure Encode_Event
     (Value : CuBit.Log_Protocol.Event; Into : out Entry_Buffer; Length : out Entry_Length);
   procedure Encode_Gap (Lost : Unsigned_64; Into : out Entry_Buffer; Length : out Entry_Length)
     with Post => Length = HEADER_BYTES;
   --  Valid is False for anything malformed; then Value is empty, Lost zero.
   procedure Decode
     (From : Entry_Buffer; Length : Natural; Kind : out Entry_Kind;
      Value : out CuBit.Log_Protocol.Event; Lost : out Unsigned_64; Valid : out Boolean);
end CuBit.Log_Streams;
