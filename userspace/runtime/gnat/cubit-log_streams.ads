pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Channel_Rings;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Protocols;

--  A reader's log stream (docs/data-plane.md): a lossless channel the
--  reader opens to logstore, consuming, before Subscribe binds its filter to
--  it (the channel's number). logstore produces into memory it owns; the
--  reader's index page tells it how far the reader has read. Neither trusts
--  the other's index (CuBit.Channels, through the proved rings) or entries
--  (Decode).
--
--  An entry is a 56-byte header (kind, source, node high, node low, observed
--  ms, publisher authority tag, encoded length; little-endian 64-bit words)
--  and, for an event, the encoded record (CuBit.Log_Records wire format).
--  A gap entry (CuBit.Log_Protocol.Omission) carries no record: its source,
--  and in the node and observed-ms words what was not retained and what
--  this reader lost. A tally entry (CuBit.Log_Protocol.Source_Tally) is the
--  header and five count words.
package CuBit.Log_Streams with Pure, SPARK_Mode is
   package Logs renames CuBit.Log_Records;
   RING_PAGES : constant := 16;
   RING_BYTES : constant CuBit.Channel_Rings.Ring_Size :=
     RING_PAGES * CuBit.Channel_Contracts.Page_Bytes;
   type Entry_Kind is (Event_Entry, Gap_Entry, Tally_Entry);
   HEADER_BYTES : constant := 56;
   TALLY_WORDS : constant := 5;
   TALLY_BYTES : constant := HEADER_BYTES + TALLY_WORDS * 8;
   Maximum_Entry_Bytes : constant := HEADER_BYTES + Natural (Logs.Wire_Count'Last);
   subtype Entry_Length is Natural range HEADER_BYTES .. Maximum_Entry_Bytes;
   subtype Entry_Buffer is CuBit.Channel_Rings.Bytes (0 .. Maximum_Entry_Bytes - 1);

   LOG_EVENT_SCHEMA : constant CuBit.Protocols.Schema_Id := 16#4C4F_4745_5654_0001#;
   CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => LOG_EVENT_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Bounded_Size, Wire_Size => Maximum_Entry_Bytes),
      Kind    => CuBit.Channel_Contracts.Queue,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => RING_PAGES,
      Buffers => 1,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);
   --  Ring bytes logstore keeps free before taking another event from a
   --  reader's queue: the largest entry and the gap entry before it, each
   --  with its length word, and the pad a wrap may need.
   Room_Needed : constant := 3 * (Maximum_Entry_Bytes + 8);

   procedure Encode_Event
     (Value : CuBit.Log_Protocol.Event; Into : out Entry_Buffer; Length : out Entry_Length);
   procedure Encode_Gap
     (Missing : CuBit.Log_Protocol.Omission; Into : out Entry_Buffer; Length : out Entry_Length)
     with Post => Length = HEADER_BYTES;
   procedure Encode_Tally
     (Tally : CuBit.Log_Protocol.Source_Tally; Into : out Entry_Buffer; Length : out Entry_Length)
     with Post => Length = TALLY_BYTES;
   --  Valid is False for anything malformed (a gap that omits nothing among
   --  them); then Value, Missing and Tally are empty. Otherwise the one that
   --  Kind names is set and the others are empty.
   procedure Decode
     (From : Entry_Buffer; Length : Natural; Kind : out Entry_Kind;
      Value : out CuBit.Log_Protocol.Event; Missing : out CuBit.Log_Protocol.Omission;
      Tally : out CuBit.Log_Protocol.Source_Tally; Valid : out Boolean);
end CuBit.Log_Streams;
