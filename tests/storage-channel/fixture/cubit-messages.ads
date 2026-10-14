with Interfaces; use Interfaces;
with System;
with CuBit.Process_IDs;
with CuBit.Handles;
package CuBit.Messages is
   subtype Process_ID is CuBit.Process_IDs.Process_ID;
   No_Process : Process_ID renames CuBit.Process_IDs.No_Process;
   function "=" (Left, Right : Process_ID) return Boolean
     renames CuBit.Process_IDs."=";
   function To_Word (Process : Process_ID) return Unsigned_64
     renames CuBit.Process_IDs.To_Word;
   function From_Word (Word : Unsigned_64) return Process_ID
     renames CuBit.Process_IDs.From_Word;
   function Is_Process (Process : Process_ID) return Boolean
     renames CuBit.Process_IDs.Is_Process;
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
   NO_COMPLETION_TOKEN : constant Unsigned_64 := Unsigned_64'Last;
   COMPLETION_OK : constant Unsigned_64 := 0;
   COMPLETION_TARGET_DIED : constant Unsigned_64 := 1;
   COMPLETION_CANCELLED : constant Unsigned_64 := 2;
   COMPLETION_QUEUE_OVERFLOW : constant Unsigned_64 := 3;
   type CompletionEntry is record
      requestId, token : Unsigned_64;
      msg : Message;
      from : Unsigned_64;
      status : Unsigned_64 := COMPLETION_OK;
      valid : Boolean := False;
   end record;
   NULL_COMPLETION : constant CompletionEntry :=
     (0, 0, NULL_MESSAGE, 0, COMPLETION_OK, False);
   function capSubmit (slot : CuBit.Handles.Endpoint; msg : Message; token : Unsigned_64) return Boolean;
   Accept_Submission : Boolean := True;
   Last_Request : Message := NULL_MESSAGE;
   Last_Token : Unsigned_64 := 0;
   Last_Endpoint : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   Submissions : Natural := 0;
   Waits : Natural := 0;
   Wait_Reply : Message := NULL_MESSAGE;
   function waitCompletion (entries : System.Address; max, min : Unsigned_64) return Unsigned_64;
end CuBit.Messages;
