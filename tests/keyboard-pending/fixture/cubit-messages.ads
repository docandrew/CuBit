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
      label : Unsigned_32 := 0;
      length : Unsigned_32 := 0;
      protocol : Protocol_Flags := 0;
      ipc : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_32 := 0;
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
      words : MessageWords := (others => 0);
   end record;
   subtype Process_ID is Unsigned_64;
   SYSINFO_REGISTERED_DRIVER : constant Unsigned_64 := 1;
   DRIVER_KEYBOARD : constant Unsigned_64 := 2;
   DRIVER_MOUSE : constant Unsigned_64 := 3;
   SYSCALL_SLEEP : constant Unsigned_64 := 4;
   SYSCALL_GETTIME : constant Unsigned_64 := 5;
   SYSCALL_EXIT : constant Unsigned_64 := 6;
   type Activity_Result is (Work_Available, Deadline_Reached, Unavailable);
   Finished : exception;
   procedure Verify;
   procedure debugPrint (Text : String);
   function syscall (Number : Unsigned_64; Arg : Unsigned_64 := 0) return Unsigned_64;
   function getInfo (Number, Arg : Unsigned_64) return Unsigned_64;
   function portOutp8 (Port : Unsigned_16; Value : Unsigned_8) return Unsigned_64;
   function portInp8 (Port : Unsigned_16) return Unsigned_64;
   function capSend (Slot : Unsigned_64; Msg : Message) return MessageTag;
   function trySendEvent (Dest : Process_ID; Msg : Message) return Boolean;
   function Poll_Event (Msg : out Message) return Boolean;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result;
end CuBit.Messages;
