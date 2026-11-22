pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Log_Records;
with CuBit.Protocols;
--  A publisher's log channel (docs/logstore-architecture.md, step 1;
--  docs/data-plane.md): the publisher opens it to logstore once, producing,
--  so that publishing a record is a copy into shared memory, not an IPC.
--
--  Shed_Newest: a full ring drops the new record and counts it; logstore
--  reports the count as a gap. logstore publishes the least severe record
--  it keeps in its consumer word Minimum_Word (Log_Records.Severity'Pos),
--  so a publisher skips the rest before encoding. Elements are encoded
--  records (CuBit.Log_Records wire format); logstore stamps each record's
--  source, node and authority from the channel's opener, never from the
--  record, and decodes every one.
package CuBit.Log_Publish_Rings with Pure, SPARK_Mode is

   LOG_RECORD_SCHEMA : constant CuBit.Protocols.Schema_Id := 16#4C4F_4752_4543_0001#;
   Maximum_Entry_Bytes : constant := Natural (CuBit.Log_Records.Wire_Count'Last);
   RING_PAGES : constant := 16;

   CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => LOG_RECORD_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Bounded_Size, Wire_Size => Maximum_Entry_Bytes),
      Kind    => CuBit.Channel_Contracts.Queue,
      Policy  => CuBit.Channel_Contracts.Shed_Newest,
      Pages   => RING_PAGES,
      Buffers => 1,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   --  The consumer word logstore publishes its minimum in.
   Minimum_Word : constant := 0;

end CuBit.Log_Publish_Rings;
