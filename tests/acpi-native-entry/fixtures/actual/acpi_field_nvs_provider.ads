pragma Ada_2022;
with ACPI_Capacity_Diagnostics;
with ACPI_Target_Storage;
with AML_Identity;
with ACPI_Locked_NVS_Protocol;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_NVS_Client;
with ACPI_NVS_Protocol;
generic
   type Transport_State is limited private;
   type Peer is private;
   No_Peer : Peer;
   with procedure Read_Clock
     (Now : out ACPI_NVS_Protocol.Wire.Time_Instant; Available : out Boolean);
   with procedure Exchange
     (Channel : in out Transport_State; Destination : Peer;
      Deadline : ACPI_NVS_Protocol.Wire.Time_Instant;
      Item : ACPI_NVS_Protocol.Packet; Reply : out ACPI_NVS_Protocol.Packet;
      Delivered : out ACPI_NVS_Protocol.Wire.Delivery);
   with procedure Read_Deadline (Deadline : out ACPI_NVS_Protocol.Wire.Time_Instant);
   Initial_Target_Capacity : Positive := AML_Field_Access.Default_Maximum_Registers;
   with procedure Record_Capacity_Failure
     (Item : ACPI_Capacity_Diagnostics.Report) is null;
package ACPI_Field_NVS_Provider is
   type Phase is (Fresh, Provisioning, Ready, Failed);
   type Context is limited private;
   procedure Initialize
     (IO : in out Context; Destination : Peer; Symbol : AML_Field_Access.Endpoint_ID;
      Deadline : ACPI_NVS_Protocol.Wire.Time_Instant; Accepted : out Boolean);
   -- Trusted geometry association only. The whole AML region and selected
   -- naturally aligned datum must fit one independently admitted NVS record.
   -- No write, controller admission, _REG readiness or Lock support is added.
   procedure Install
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : out AML_Field_Access.Target; Accepted : out Boolean);
   function Identity (IO : Context) return AML_Identity.Identity;
   function Pending (IO : Context) return Boolean;
   function Uncertain (IO : Context) return AML_Field_Access.Observation;
   function Completed_Fault (IO : Context) return AML_Field_Access.Observation;
   procedure Describe
     (IO : Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   procedure Authorize
     (IO : Context; Target : AML_Field_Access.Target; For_Write : Boolean; Accepted : out Boolean);
   -- Metadata only, selected from this exact live read-only NVS target.
   -- No caller address is accepted; backend lease/record checks remain due.
   procedure Describe_Scoped_Read
     (IO : Context; Target : AML_Field_Access.Target;
      ID : out ACPI_Locked_NVS_Protocol.Record_ID;
      Datum : out ACPI_Locked_NVS_Protocol.Geometry; Accepted : out Boolean);
   procedure Dispatch
     (IO : in out Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response);
   -- No native terminal-evidence protocol exists yet. An uncertain operation
   -- remains captured/stopped; callers must not manufacture reconciliation.
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
private
   package P renames ACPI_NVS_Protocol;
   type Channel_State is limited record
      Inner : Transport_State;
      Delivered : P.Wire.Delivery := P.Wire.Not_Dispatched;
   end record;
   procedure Exchange_Tracked
     (Channel : in out Channel_State; Destination : Peer; Deadline : P.Wire.Time_Instant;
      Item : P.Packet; Reply : out P.Packet; Delivered : out P.Wire.Delivery);
   package Client is new ACPI_NVS_Client (Channel_State, Peer, No_Peer, Read_Clock, Exchange_Tracked);
   subtype Target_Index is Positive;
   type Association is record
      Geometry : AML_Field_Layout.Register_Geometry;
      Record_ID, Offset : Client.Word := 0;
   end record;
   -- Heap storage grows on demand; ordinals remain stable across growth.
   package Storage is new ACPI_Target_Storage
     (Association, Initial_Target_Capacity);
   type Context is limited record
      Stage : Phase := Fresh;
      Issuer : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : AML_Field_Access.Endpoint_ID := AML_Field_Access.No_Endpoint;
      Lower : Client.Context;
      Channel : Channel_State;
      Items : Storage.Context;
      Issued : AML_Field_Access.Counter := 0;
      Working : Boolean := False;
      Captured, Fault : AML_Field_Access.Observation;
   end record;
end ACPI_Field_NVS_Provider;
