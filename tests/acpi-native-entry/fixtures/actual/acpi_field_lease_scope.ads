pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Client;
with ACPI_Locked_NVS_Client;
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
   with package NVS is new ACPI_Locked_NVS_Client
     (Transport => Transport, Peer => Peer, No_Peer => No_Peer,
      IPC_Clock => IPC_Clock, Exchange => Exchange, Lease => Lease);
   Maximum_Depth : Positive;
package ACPI_Field_Lease_Scope with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   package W renames ACPI_Region_Protocol;
   package P renames ACPI_Locked_NVS_Protocol;
   type Depth_Count is new Natural range 0 .. Maximum_Depth;
   subtype Cleanup_Outcome is Lease.Outcome;
   type Context is limited private;
   type Statistics is record
      Depth : Depth_Count;
      Initialized, Working, Faulted, Cleanup_Required, Lease_Usable : Boolean;
   end record;
   function Observe (State : Context) return Statistics;
   function Pending (State : Context) return Boolean;
   procedure Initialize (State : in out Context; Destination : Peer; Accepted : out Boolean);
   -- Only the first required lock takes the supplied deadlines. Nested locks
   -- reuse that lease; neither a new ticket nor a longer deadline is issued.
   -- Scope ownership alone grants no address, register or write authority.
   procedure Begin_Field
     (State : in out Context; Channel : in out Transport;
      Requires_Lock, For_Write : Boolean;
      Lease_Deadline : Interfaces.Unsigned_64; IPC_Deadline : W.Time_Instant;
      Budget : Lease.P.Attempt_Budget; Accepted : out Boolean);
   -- Cleanup uses a separate explicit deadline, independent of data expiry.
   -- Every accepted Begin needs End, including after failure. A refused Begin
   -- can retain cleanup duty without opening a scope: call Cleanup at depth0.
   procedure End_Field
     (State : in out Context; Channel : in out Transport;
      Cleanup_Deadline : W.Time_Instant; Accepted : out Boolean);
   procedure Read
     (State : in out Context; Channel : in out Transport;
      ID : P.Record_ID; Datum : P.Geometry; IPC_Deadline : W.Time_Instant;
      Value : out Interfaces.Unsigned_64; Accepted : out Boolean)
     with Post => (if not Accepted then Value = 0);
   procedure Cleanup
     (State : in out Context; Channel : in out Transport;
      Deadline : W.Time_Instant; Result : out Lease.Outcome);
   -- A later explicit operation can resume only after confirmed cleanup and
   -- retirement of failed read state. No old field or read is resumed/replayed.
   procedure Retire (State : in out Context; Accepted : out Boolean);
private
   subtype Frame_Position is Depth_Count range 1 .. Depth_Count'Last;
   type Ownership is array (Frame_Position) of Boolean;
   type Context is limited record
      Initialized, Working, Faulted : Boolean := False;
      Depth : Depth_Count := 0;
      Owners : Ownership := [others => False];
      Held : Lease.Context;
      Data : NVS.Context;
   end record;
end ACPI_Field_Lease_Scope;
