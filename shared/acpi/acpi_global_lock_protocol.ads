pragma Ada_2022;
with Interfaces;
with ACPI_Region_Protocol;
-- Version-one lock control frames. No physical address, caller identity,
-- scope tag, force-release or peer-exit primitive is supplied in the payload.
package ACPI_Global_Lock_Protocol with Pure, SPARK_Mode is
   use Interfaces;
   subtype Packet is ACPI_Region_Protocol.Packet;
   type Operation is (Prepare, Acquire, Check_Access, Release,
     Query_Acquisition, Query_Release, Query_Termination);
   First_Operation : constant Unsigned_32 := 16#290#;
   First_Reply : constant Unsigned_32 := 16#F290#;
   -- Coordinator-reserved role-35 ACPI block; consecutive enum positions
   -- produce distinct operation labels, with replies in a disjoint block.
   pragma Compile_Time_Error
     (First_Operation < 16#0290# or else
      First_Operation + Operation'Pos (Operation'Last) > 16#02BF# or else
      First_Reply < 16#F290# or else
      First_Reply + Operation'Pos (Operation'Last) > 16#F2BF# or else
      First_Operation + Operation'Pos (Operation'Last) >= First_Reply or else
      First_Reply /= First_Operation + 16#F000#,
      "Global Lock labels must be distinct within the reserved ACPI blocks");
   Version : constant Unsigned_64 := 1;
   Maximum_Attempts : constant := 16;
   subtype Attempt_Budget is Positive range 1 .. Maximum_Attempts;
   type Ticket is record Domain, Sequence : Unsigned_64 := 0; end record;
   No_Ticket : constant Ticket := (0, 0);
   function Valid (Item : Ticket) return Boolean is (Item.Domain /= 0 and then Item.Sequence /= 0);
   type Request is record
      Valid : Boolean := False;
      Kind : Operation := Prepare;
      Nonce, Deadline : Unsigned_64 := 0;
      Budget : Attempt_Budget := Attempt_Budget'First;
      Token : Ticket := No_Ticket;
   end record;
   function Owned_Label (Label : Unsigned_32) return Boolean;
   function Decode_Request (Item : Packet) return Request;
   function Prepare_Request (Nonce, Deadline : Unsigned_64; Budget : Attempt_Budget) return Packet
     with Pre => Nonce /= 0 and then Deadline /= 0 and then Deadline /= Unsigned_64'Last;
   function Ticket_Request (Kind : Operation; Token : Ticket) return Packet
     with Pre => Kind /= Prepare and then Valid (Token);
   -- These enumeration positions are frozen for protocol version one.
   -- Recorded/Cached describe a result record, not hardware success.
   type Reply_Kind is (Ready, Recorded, Cached, Checked, No_Result, Denied,
     Busy, Stale_Ticket, Conflict, Invalid_Request, Unavailable, Exhausted, Reentered, Malformed);
   type Lock_Outcome is (Configured, Invalid_Configuration, Already_Configured,
     Acquired, Acquired_After_Deadline, Acquired_Clock_Failed,
     Released, Waiting_For_Firmware, Contended, Expired, Clock_Failed,
     Clock_Reversed, Invalid_Deadline, Unauthorized, Not_Ready, Already_Held,
     Stale_Lease, Lease_Exhausted, Read_Unavailable, Atomic_Refused,
     Atomic_Indeterminate, Signal_Deferred, Signal_Indeterminate, Reentered);
   type Reply is record
      Valid : Boolean := False;
      Kind : Reply_Kind := Malformed;
      Outcome : Lock_Outcome := Not_Ready;
      Token : Ticket := No_Ticket;
   end record;
   function Valid_Result (Kind : Operation; Result : Reply) return Boolean
     with Post => (if Valid_Result'Result and then Kind = Prepare and then Result.Kind = Ready
       then Valid (Result.Token));
   function Encode_Reply (Kind : Operation; Result : Reply) return Packet
     with Pre => Valid_Result (Kind, Result);
   function Decode_Reply (Item : Packet; Kind : Operation) return Reply
     with Post => (if Decode_Reply'Result.Valid then Valid_Result (Kind, Decode_Reply'Result)
       and then (if Kind = Prepare and then Decode_Reply'Result.Kind = Ready
         then Valid (Decode_Reply'Result.Token)));
end ACPI_Global_Lock_Protocol;
