pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Client;
with ACPI_Locked_NVS_Protocol;
with ACPI_Region_Protocol;
generic
   type Transport is limited private;
   type Peer is private;
   No_Peer : Peer;
   with procedure IPC_Clock (Now : out ACPI_Region_Protocol.Time_Instant; Available : out Boolean);
   with procedure Exchange
     (Channel : in out Transport; Destination : Peer;
      Deadline : ACPI_Region_Protocol.Time_Instant;
      Request : ACPI_Region_Protocol.Packet; Reply : out ACPI_Region_Protocol.Packet;
      Delivered : out ACPI_Region_Protocol.Delivery);
   with package Lease is new ACPI_Global_Lock_Client
     (Transport => Transport, Peer => Peer, No_Peer => No_Peer,
      IPC_Clock => IPC_Clock, Exchange => Exchange, others => <>);
package ACPI_Locked_NVS_Client with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   package P renames ACPI_Locked_NVS_Protocol;
   package W renames ACPI_Region_Protocol;
   type Phase is (Ready, Failed, Uncertain);
   type Outcome is (Read_Complete, Not_Ready, Invalid_Request, Invalid_Deadline,
     Peer_Changed, Clock_Unavailable, Clock_Reversed, Expired, Not_Sent,
     Completion_Unknown, Bad_Reply, Backend_Refused, Late_Completed,
     Exhausted, Reentered, Evidence_Received, No_Evidence, Retired);
   type Context is limited private;
   type Statistics is record
      Stage : Phase;
      Working, Pending_Read : Boolean;
      Remote : P.Read_Outcome;
   end record;
   function Observe (State : Context) return Statistics;
   -- One persistent context per retained peer. The destination comes only
   -- from the matching lease client; this API cannot choose another endpoint.
   -- Every failure stops this field's local lease use; cleanup remains due.
   procedure Read
     (State : in out Context; Held : in out Lease.Context; Channel : in out Transport;
      ID : P.Record_ID; Datum : P.Geometry; IPC_Deadline : W.Time_Instant;
      Value : out Interfaces.Unsigned_64; Result : out Outcome)
     with Post => (if Result /= Read_Complete then Value = 0);
   -- Historical query only. Never returns data for the failed Field and
   -- never resumes it or repeats the read, even if Done is recovered.
   procedure Query_Evidence
     (State : in out Context; Held : in out Lease.Context; Channel : in out Transport;
      IPC_Deadline : W.Time_Instant; Result : out Outcome);
   -- Discard failed-field state only after positive lease cleanup. This says
   -- nothing about missing read history and never reexecutes the old request.
   -- A later explicit field operation uses a new lease and descriptor.
   procedure Retire (State : in out Context; Held : Lease.Context; Result : out Outcome);
private
   type Context is limited record
      Stage : Phase := Ready;
      Working, Bound, Seen_Clock, Clock_Broken, Pending_Read : Boolean := False;
      Destination : Peer := No_Peer;
      Last_Clock : W.Time_Instant := 0;
      Ticket : P.Lease_Ticket := (0, 0);
      Descriptor : P.Descriptor_ID := P.No_Descriptor;
      Sequence : P.Read_Sequence := P.No_Read;
      ID : P.Record_ID := P.No_Record;
      Datum : P.Geometry := P.Empty_Geometry;
      Remote : P.Read_Outcome := P.No_Data;
   end record;
end ACPI_Locked_NVS_Client;
