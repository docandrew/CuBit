with Interfaces; use Interfaces;
with System;
package CuBit.Messages is
   subtype ProcessID is Unsigned_64;
   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;
   type MessageTag is record
      label : Unsigned_32;
      length, flags : Unsigned_8;
      reserved : Unsigned_16;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64 := 0;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message := ((0, 0, 0, 0), 0, [others => 0]);
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
   function capSubmit (slot : CapabilitySlot; msg : Message; token : Unsigned_64) return Boolean;
   Accept_Submission : Boolean := True;
   Last_Request : Message := NULL_MESSAGE;
   Last_Token : Unsigned_64 := 0;
   Last_Endpoint : CapabilitySlot := 0;
   Submissions : Natural := 0;
   Waits : Natural := 0;
   Wait_Reply : Message := NULL_MESSAGE;
   function waitCompletion (entries : System.Address; max, min : Unsigned_64) return Unsigned_64;
end CuBit.Messages;
