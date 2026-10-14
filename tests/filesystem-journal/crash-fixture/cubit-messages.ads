with CuBit.Handles;
with Interfaces; use Interfaces;
package CuBit.Messages is
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
      authorityTag : Unsigned_64 := 0;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0), 0, [others => 0]);
   --  As the runtime's: the caller gives a deadline (ignored here).
   Wait_Forever : constant Unsigned_64 := Unsigned_64'Last;
   function capCall
     (slot : CuBit.Handles.Endpoint; msg : in out Message; Deadline : Unsigned_64)
     return MessageTag;
   procedure debugPrint (value : String);
   type Bytes is array (Natural range <>) of Unsigned_8;
   Grant_Buffer : Bytes (0 .. 4095) := [others => 0];
   procedure Open_Image (Path : String);
   procedure Close_Image;
   --  Device commands so far (for tests that count I/O per operation).
   function Device_Commands return Natural;
   --  Test-only: owned-memory allocation (zero-filled, page aligned), as
   --  the kernel's SYSCALL_ALLOCATE_OWNED_MEMORY. Other calls return Last.
   SYSCALL_ALLOCATE_OWNED_MEMORY : constant Unsigned_64 := 115;
   --  The monotonic ms clock: 0 hosted. The wall clock is the fixture's
   --  CuBit.System_Facts (CUBIT_TEST_WALL_CLOCK).
   SYSCALL_GETTIME : constant Unsigned_64 := 27;
   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0) return Unsigned_64;
end CuBit.Messages;
