pragma Ada_2022;
with ACPI_Lock_Trace_Protocol; with ACPI_Region_Protocol; with ACPI_Native_Log;
package body ACPI_Native_Lock_Notes is
   package W renames ACPI_Lock_Trace_Protocol;
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   State : Trace.Context;
   procedure Begin_Request (Caller : CuBit.Messages.Process_ID; Scope : Interfaces.Unsigned_64; Kind : G.Operation) is
   begin Trace.Begin_Request (State,Caller,Scope,Kind); end Begin_Request;
   procedure Finish_Request (Reply : G.Reply) is
   begin Trace.Finish_Request (State,Reply); end Finish_Request;
   procedure Begin_Maintenance is begin Trace.Begin_Maintenance (State); end Begin_Maintenance;
   procedure Finish_Maintenance (Outcome : G.Lock_Outcome) is
   begin Trace.Finish_Maintenance (State,Outcome); end Finish_Maintenance;
   procedure Observe_Word (Word : Interfaces.Unsigned_32; Available : Boolean) is
   begin Trace.Observe_Read (State,Word,Available); end Observe_Word;
   procedure Observe_CAS (Expected, Replacement, Observed : Interfaces.Unsigned_32; Known, Attempted, Exchanged : Boolean) is
   begin Trace.Observe_CAS (State,Expected,Replacement,Observed,Known,Attempted,Exchanged); end Observe_CAS;
   procedure Observe_Signal (Result : Trace.Signal_Result; Reads, Writes : Natural) is
   begin Trace.Observe_Signal (State,Result,Reads,Writes); end Observe_Signal;
   procedure Handle_Dump
     (Caller : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Reply : in out CuBit.Messages.Message; Handled : out Boolean) is
      Packet : constant W.Packet := (Request.tag.label, Request.tag.length, CuBit.Messages.Flag_Byte (Request.tag),
        Request.tag.reserved,ACPI_Region_Protocol.Words (Request.words));
      Result : Trace.Dump_Outcome; Status : W.Outcome := W.Malformed; Output : W.Packet;
   begin
      Handled := Request.tag.label = W.Request_Label; if not Handled then return; end if;
      if W.Valid_Request (Packet) then
         Trace.Request_Dump (State,Caller,Request.authorityTag,Result);
         Status := (case Result is when Trace.Accepted => W.Accepted, when Trace.Already_Dumped => W.Already_Dumped,
           when Trace.Unauthorized => W.Unauthorized, when Trace.Malformed => W.Malformed);
      end if;
      Output := W.Reply (Status,(if Status in W.Accepted | W.Already_Dumped then Trace.Length (State) else 0));
      Reply := CuBit.Messages.NULL_MESSAGE;
      Reply.tag := (label => Output.Label,length => Output.Length,protocol => 0,ipc => <>,reserved => 0);
      Reply.words := CuBit.Messages.MessageWords (Output.Data);
   end Handle_Dump;
   function Number (Value : Interfaces.Unsigned_64) return String is
      Text : String (1 .. 20); First : Positive := Text'Last; Remaining : Interfaces.Unsigned_64 := Value;
   begin
      loop Text (First) := Character'Val (Character'Pos ('0')+Natural (Remaining mod 10));
         Remaining := Remaining/10; exit when Remaining = 0; First := First-1; end loop;
      return Text (First .. Text'Last);
   end Number;
   function Hex (Value : Interfaces.Unsigned_32) return String is
      Digits_Text : constant String := "0123456789ABCDEF"; Text : String (1 .. 8);
      Remaining : Interfaces.Unsigned_32 := Value;
   begin
      for I in reverse Text'Range loop Text (I) := Digits_Text (Natural (Remaining mod 16)+1); Remaining := Remaining/16; end loop;
      return Text;
   end Hex;
   function Bit (Value : Boolean) return String is (if Value then "1" else "0");
   function Name (Value : G.Operation) return String is
     (case Value is
        when G.Prepare => "PREPARE",
        when G.Acquire => "ACQUIRE",
        when G.Check_Access => "CHECK_ACCESS",
        when G.Release => "RELEASE",
        when G.Query_Acquisition => "QUERY_ACQUISITION",
        when G.Query_Release => "QUERY_RELEASE",
        when G.Query_Termination => "QUERY_TERMINATION");
   function Name (Value : G.Reply_Kind) return String is
     (case Value is
        when G.Ready => "READY",
        when G.Recorded => "RECORDED",
        when G.Cached => "CACHED",
        when G.Checked => "CHECKED",
        when G.No_Result => "NO_RESULT",
        when G.Denied => "DENIED",
        when G.Busy => "BUSY",
        when G.Stale_Ticket => "STALE_TICKET",
        when G.Conflict => "CONFLICT",
        when G.Invalid_Request => "INVALID_REQUEST",
        when G.Unavailable => "UNAVAILABLE",
        when G.Exhausted => "EXHAUSTED",
        when G.Reentered => "REENTERED",
        when G.Malformed => "MALFORMED");
   function Name (Value : G.Lock_Outcome) return String is
     (case Value is
        when G.Configured => "CONFIGURED",
        when G.Invalid_Configuration => "INVALID_CONFIGURATION",
        when G.Already_Configured => "ALREADY_CONFIGURED",
        when G.Acquired => "ACQUIRED",
        when G.Acquired_After_Deadline => "ACQUIRED_AFTER_DEADLINE",
        when G.Acquired_Clock_Failed => "ACQUIRED_CLOCK_FAILED",
        when G.Released => "RELEASED",
        when G.Waiting_For_Firmware => "WAITING_FOR_FIRMWARE",
        when G.Contended => "CONTENDED",
        when G.Expired => "EXPIRED",
        when G.Clock_Failed => "CLOCK_FAILED",
        when G.Clock_Reversed => "CLOCK_REVERSED",
        when G.Invalid_Deadline => "INVALID_DEADLINE",
        when G.Unauthorized => "UNAUTHORIZED",
        when G.Not_Ready => "NOT_READY",
        when G.Already_Held => "ALREADY_HELD",
        when G.Stale_Lease => "STALE_LEASE",
        when G.Lease_Exhausted => "LEASE_EXHAUSTED",
        when G.Read_Unavailable => "READ_UNAVAILABLE",
        when G.Atomic_Refused => "ATOMIC_REFUSED",
        when G.Atomic_Indeterminate => "ATOMIC_INDETERMINATE",
        when G.Signal_Deferred => "SIGNAL_DEFERRED",
        when G.Signal_Indeterminate => "SIGNAL_INDETERMINATE",
        when G.Reentered => "REENTERED");
   function Name (Value : Trace.Signal_Result) return String is
     (case Value is when Trace.No_Signal => "NONE", when Trace.Signaled => "SIGNALED",
       when Trace.Signal_Not_Sent => "NOT_SENT", when Trace.Signal_Unknown => "UNKNOWN");
   procedure Flush (No_Lease_Held : Boolean) is
      Item : Trace.Snapshot; Found : Boolean;
   begin
      if not No_Lease_Held or else not Trace.Pending (State) then return; end if;
      -- Serialized sole loop: no owner operation can interleave these copies.
      ACPI_Native_Log.Info ("acpi-backend: stage lock-trace ok records=" & Number (Interfaces.Unsigned_64 (Trace.Length (State))) &
        " dropped=" & Number (Trace.Dropped (State)));
      for I in Trace.Index loop
         Trace.Read (State,I,Item,Found); exit when not Found;
         ACPI_Native_Log.Info ("acpi-backend: lock op=" & (if Item.Maintenance then "MAINTENANCE" else Name (Item.Request)) &
           " valid=" & Bit (Item.Reply_Valid) & " reply=" & Name (Item.Reply) & " outcome=" & Name (Item.Outcome) &
           " signal=" & Name (Item.Signal) & " signal_reads=" & Number (Interfaces.Unsigned_64 (Item.Signal_Reads)) &
           " signal_writes=" & Number (Interfaces.Unsigned_64 (Item.Signal_Writes)));
         ACPI_Native_Log.Info ("acpi-backend: lock facs read=" & Bit (Item.Read_Present) & " available=" & Bit (Item.Read_Available) &
           " word=" & Hex (Item.Read_Word) & " cas=" & Bit (Item.CAS_Present) & " known=" & Bit (Item.CAS_Known) &
           " attempted=" & Bit (Item.CAS_Attempted) & " exchanged=" & Bit (Item.CAS_Exchanged) &
           " expected=" & Hex (Item.Expected) & " replacement=" & Hex (Item.Replacement) & " observed=" & Hex (Item.Observed) &
           " attempts=" & Number (Item.CAS_Count));
      end loop;
      Trace.Finish_Dump (State);
   end Flush;
end ACPI_Native_Lock_Notes;
