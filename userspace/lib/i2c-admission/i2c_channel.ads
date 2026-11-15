pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Protocols;
with CuBit.Request_Channels;
with I2C_Channel_Wire;
package I2C_Channel is
   package W renames I2C_Channel_Wire;
   Contract_Name : constant String := "com.cubit-os.i2c-transfer";
   -- Draft identity, to be registered with the native channel offer at publication.
   Schema : constant CuBit.Protocols.Schema_Id := 16#12C0_7A4F_0000_0001#;
   Header_Token_Bytes : constant := 8;
   Submission_Bytes : constant := Header_Token_Bytes + W.Header_Bytes;
   Payload_Pages : constant Positive :=
     (Integer (W.Maximum_Payload) + CuBit.Channel_Contracts.Page_Bytes - 1)
       / CuBit.Channel_Contracts.Page_Bytes;
   -- One in-flight transfer: producer writes its own bytes before submitting;
   -- consumer snapshots metadata and reads each payload byte only when needed.
   -- Client snapshots a completed HID report before submitting its next request.
   package RC is new CuBit.Request_Channels
     (W.Raw_Request, W.W.Completion_Words, Request_Bits => 0, Completion_Bits => 0,
      Client_Extra_Pages => Payload_Pages, Server_Extra_Pages => Payload_Pages);
   Element : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => Schema, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Submission_Bytes),
      Kind => CuBit.Channel_Contracts.Duplex,
      Policy => CuBit.Channel_Contracts.Lossless,
      Pages => RC.Client_Pages, Buffers => RC.Server_Pages,
      Rule => CuBit.Channel_Contracts.Copy_Then_Validate);
   -- Copy_Then_Validate applies to the small ring entry. The persistent byte
   -- areas are untrusted streams: they carry neither targets nor speed, and
   -- none of their contents may authorize further memory/register accesses.
   -- Server must quiesce the controller before completing or returning mappings.
   -- A failed quiescence retains the mapping for supervision, even after timeout.
end I2C_Channel;
