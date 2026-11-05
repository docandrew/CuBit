pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
with ACPI_Locked_NVS_Protocol;
generic
   type Principal is private;
   No_Principal : Principal;
   type Scope_Tag is private;
   No_Scope : Scope_Tag;
   with package Owner is new ACPI_Global_Lock_Owner
     (Principal => Scope_Tag, No_Principal => No_Scope, others => <>);
   with package Sessions is new ACPI_Global_Lock_Scoped_Sessions
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner);
   with package Requests is new ACPI_Global_Lock_Requests
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner, Sessions, others => <>);
   -- Metadata only: exact scoped named NVS record, aligned in-bounds datum
   -- and captured immutable nonzero catalog epoch. No firmware data load.
   with procedure Describe_Datum
     (Scope : Scope_Tag; ID : ACPI_Locked_NVS_Protocol.Record_ID;
      Datum : ACPI_Locked_NVS_Protocol.Geometry;
      Epoch : out Interfaces.Unsigned_64; Accepted : out Boolean);
   -- One synchronous width-aware read through the retained NVS owner.
   -- A false or unrepresentable result closes this data context permanently.
   -- Callbacks return normally, cannot alias/reenter/revoke either context,
   -- and cannot call maintenance while a data operation is in progress.
   with procedure Read_Datum
     (Scope : Scope_Tag; Epoch : Interfaces.Unsigned_64;
      ID : ACPI_Locked_NVS_Protocol.Record_ID; Datum : ACPI_Locked_NVS_Protocol.Geometry;
      Value : out Interfaces.Unsigned_64; Completed : out Boolean);
   Maximum_Descriptors : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
package ACPI_Locked_NVS with SPARK_Mode is
   package P renames ACPI_Locked_NVS_Protocol;
   -- Retain exactly one context for this backend incarnation. Recreating it
   -- under a live peer identity would reuse descriptor IDs and is forbidden.
   type Context is limited private;
   -- Scope and Caller are kernel receive metadata. No payload identity or
   -- physical address is admitted. One descriptor/result slot is bounded;
   -- replacement retires old IDs and never establishes their completion.
   procedure Select_Datum
     (State : in out Context; Lock : in out Requests.Context;
      Scope : Scope_Tag; Caller : Principal; Lease : P.Lease_Ticket;
      ID : P.Record_ID; Datum : P.Geometry; Result : out P.Reply);
   -- A read sequence starts at1 for each server-issued descriptor and must
   -- advance exactly once. Duplicate last sequences return saved results;
   -- older/gapped sequences cannot cause another load. Sequence identity is
   -- bound to descriptor + full lease + full caller + scope.
   procedure Read
     (State : in out Context; Lock : in out Requests.Context;
      Scope : Scope_Tag; Caller : Principal; Lease : P.Lease_Ticket;
      Descriptor : P.Descriptor_ID; Sequence : P.Read_Sequence; Result : out P.Reply);
   procedure Query
     (State : Context; Scope : Scope_Tag; Caller : Principal; Lease : P.Lease_Ticket;
      Descriptor : P.Descriptor_ID; Sequence : P.Read_Sequence; Result : out P.Reply);
   procedure Dispatch
     (State : in out Context; Lock : in out Requests.Context;
      Scope : Scope_Tag; Caller : Principal; Request : P.Packet;
      Response : out P.Packet; Handled : out Boolean);
private
   type Context is limited record
      Working, Stopped, Present, Lease_Closed, Saved : Boolean := False;
      Caller : Principal := No_Principal;
      Scope : Scope_Tag := No_Scope;
      Lease : P.Lease_Ticket := (0, 0);
      ID : P.Record_ID := P.No_Record;
      Datum : P.Geometry;
      Epoch, Issued : Interfaces.Unsigned_64 := 0;
      Descriptor : P.Descriptor_ID := P.No_Descriptor;
      Last_Read : P.Read_Sequence := P.No_Read;
      Outcome : P.Read_Outcome := P.No_Data;
      Value : Interfaces.Unsigned_64 := 0;
   end record;
end ACPI_Locked_NVS;
