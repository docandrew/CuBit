with Interfaces; use Interfaces;
with Capability_ABI;
with CuBit.Process_IDs;
with System;
with CuBit.Handles;
package CuBit.Messages is
   subtype Process_ID is CuBit.Process_IDs.Process_ID;
   No_Process : Process_ID renames CuBit.Process_IDs.No_Process;
   function To_Word (Item : Process_ID) return Interfaces.Unsigned_64 renames CuBit.Process_IDs.To_Word;
   DRIVER_KEYBOARD : constant Interfaces.Unsigned_64 := 1;
   DRIVER_MOUSE : constant Interfaces.Unsigned_64 := 10;
   type Protocol_Flags is new Capability_ABI.Protocol_Flags;
   type IPC_Flags is new Capability_ABI.IPC_Flags;
   No_IPC_Flags : constant IPC_Flags := (others => False);

   type MessageTag is record
      label    : Unsigned_32;
      length   : Unsigned_8;
      protocol : Protocol_Flags := 0;
      ipc      : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16; -- Not authenticated; never an authority tag
   end record with Size => 64;

   for MessageTag use record
      label    at 0 range 0 .. 31;
      length   at 4 range 0 .. 7;
      protocol at 5 range 0 .. 3;
      ipc      at 5 range 4 .. 7;
      reserved at 6 range 0 .. 15;
   end record;

   NULL_TAG : constant MessageTag :=
     (label => 0, length => 0, protocol => 0, ipc => No_IPC_Flags, reserved => 0);

   --  The tag's whole flag byte, for decoders that validate it exactly.
   function Flag_Byte (Tag : MessageTag) return Unsigned_8 is
     (Capability_ABI.Flag_Byte (Capability_ABI.Protocol_Flags (Tag.protocol),
                                Capability_ABI.IPC_Flags (Tag.ipc)));

   type MessageWords is array (0 .. 3) of Unsigned_64;

   type Message is record
      tag      : MessageTag;
      --  Kernel-stamped from the capability authorizing this message.
      authorityTag : Unsigned_64 := 0;
      words    : MessageWords;
   end record;

   pragma Compile_Time_Error (Message'Size /= 48 * 8,
                              "IPC message ABI must remain 48 bytes");

   NULL_MESSAGE : constant Message :=
     (tag => NULL_TAG, authorityTag => 0, words => (others => 0));

   NO_COMPLETION_TOKEN : constant Unsigned_64 := Unsigned_64'Last;
   COMPLETION_OK              : constant Unsigned_64 := 0;
   COMPLETION_TARGET_DIED     : constant Unsigned_64 := 1;
   COMPLETION_CANCELLED       : constant Unsigned_64 := 2;
   COMPLETION_QUEUE_OVERFLOW  : constant Unsigned_64 := 3;

   type CompletionEntry is record
      requestId : Unsigned_64;
      token : Unsigned_64;
      msg   : Message;
      from  : Unsigned_64;
      status : Unsigned_64 := COMPLETION_OK;
      valid : Boolean := False;
   end record;
   --  Wire layout shared with kernel Process.CompletionEntry. The kernel
   --  explicitly zeroes reserved tail bytes 81..87 before exporting them.
   for CompletionEntry use record
      requestId at 0 range 0 .. 63;
      token at 8 range 0 .. 63;
      msg at 16 range 0 .. 383;
      from at 64 range 0 .. 63;
      status at 72 range 0 .. 63;
      valid at 80 range 0 .. 7;
   end record;
   for CompletionEntry'Size use 88 * 8;
   for CompletionEntry'Alignment use 8;

   NULL_COMPLETION : constant CompletionEntry :=
     (requestId => 0,
      token     => 0,
      msg       => NULL_MESSAGE,
      from      => 0,
      status    => COMPLETION_OK,
      valid     => False);

   -- Hosted syscall model; excluded from actual-runtime semantic checks.
   SYSCALL_GETTIME : constant Unsigned_64 := 27;
   SYSCALL_SLEEP : constant Unsigned_64 := 28;
   Wait_Forever : constant Unsigned_64 := Unsigned_64'Last;
   type Activity_Result is (Work_Available, Deadline_Reached, Unavailable);
   Clock_Value, Sleep_Status, Reply_Status, Completion_Count : Unsigned_64 := 0;
   Sleep_Argument, Wait_Argument : Unsigned_64 := 0;
   Sleep_Calls, Wait_Calls, Reply_Calls : Natural := 0;
   Wait_Result : Activity_Result := Work_Available;
   Request_Present, Event_Present : Boolean := False;
   Queued_Peer, Reply_Peer : Process_ID := No_Process;
   Queued_Message, Reply_Message : Message := NULL_MESSAGE;
   function syscall (Number : Unsigned_64; Argument : Unsigned_64 := 0) return Unsigned_64;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result;
   function Poll_Completion (Result : System.Address) return Unsigned_64;
   function Poll_Event (Item : out Message; Publisher : out Process_ID) return Boolean;
   procedure Poll_Service_Request (From : out Process_ID; Msg : out Message; Found : out Boolean);
   function reply (ReplyTo : Process_ID; Msg : Message) return Unsigned_64;
   Send_Calls, Submit_Calls : Natural := 0;
   Sent_Message : Message := NULL_MESSAGE;
   Sent_Deadline, Submitted_Token : Unsigned_64 := 0;
   Send_Result : MessageTag := NULL_TAG;
   Submit_Result : Boolean := True;
   function capSend (Target : CuBit.Handles.Endpoint; Msg : Message;
      Deadline : Unsigned_64) return MessageTag;
   function capSubmit (Target : CuBit.Handles.Endpoint; Msg : Message;
      Token : Unsigned_64) return Boolean;
end CuBit.Messages;
