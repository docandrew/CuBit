with Interfaces; use Interfaces;
with CuBit.Handles;
--  The wire fixture's driver: the endpoint word Admitted_Word is the
--  admitted render endpoint (stamp 99); any other endpoint is not admitted.
package CuBit.Messages is
   Admitted_Word : constant Unsigned_64 := 2 ** 32 + 63;
   --  The tag's flag byte, as CuBit.Messages has it (CAP-002).
   type Protocol_Flags is mod 2 ** 4 with Size => 4;
   type IPC_Flags is record
      Accepts_Capability, Capability_Transferred, Capability_Refused,
      Capability_Offered : Boolean := False;
   end record with Size => 4;
   for IPC_Flags use record
      Accepts_Capability     at 0 range 0 .. 0;
      Capability_Transferred at 0 range 1 .. 1;
      Capability_Refused     at 0 range 2 .. 2;
      Capability_Offered     at 0 range 3 .. 3;
   end record;
   No_IPC_Flags : constant IPC_Flags := (others => False);
   type MessageTag is record
      label : Unsigned_32;
      length : Unsigned_8;
      protocol : Protocol_Flags := 0;
      ipc : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16;
   end record;
   --  The whole flag byte, as decoders that validate it exactly read it.
   function Flag_Byte (Tag : MessageTag) return Unsigned_8 is
     (Unsigned_8 (Tag.protocol)
      + (if Tag.ipc.Accepts_Capability then 16 else 0)
      + (if Tag.ipc.Capability_Transferred then 32 else 0)
      + (if Tag.ipc.Capability_Refused then 64 else 0)
      + (if Tag.ipc.Capability_Offered then 128 else 0));
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0), 0, [others => 0]);
   Calls : Natural := 0;
   Accounting_Response : MessageWords := [0, 1, 12288, 8192];
   Fault : Natural := 0;
   Memory_Response : MessageWords := [0, 1, 1, 0];
   Map_Response : MessageWords := [0, 1, 1, 7 * 2 ** 32 + 8];
   Map_Request : MessageWords := [others => 0];
   Prepare_Response : MessageWords := [0, 1, 0, 0];
   Close_Response : MessageWords := [0, 1, 16#4750_0000_0000_0001#, 0];
   Retirement_Response : MessageWords := [0, 1, 0, 0];
   Register_Response : MessageWords := [0, 1, 0, 0];
   Update_Response : MessageWords := [0, 1, 1, 0];
   Update_Request : MessageWords := [others => 0];
   Bound_DMA, Last_Allocation_DMA : Unsigned_64 := 0;
   Wait_Forever : constant Unsigned_64 := Unsigned_64'Last;
   --  The runtime's call-deadline surface (Native_GPU_Calls): a mock reply
   --  never times out, so the deadline value is unused.
   REPLY_TIMEOUT : constant Unsigned_32 := 16#FFFF_0001#;
   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64 is (Milliseconds);
   function capCall (Target : CuBit.Handles.Endpoint; Msg : in out Message;
                     Deadline : Unsigned_64) return MessageTag;
end CuBit.Messages;
