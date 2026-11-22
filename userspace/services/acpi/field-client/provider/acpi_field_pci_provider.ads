pragma Ada_2022;
with Interfaces;
with AML_Identity;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_PCI_Protocol;
with ACPI_Target_Storage;
with ACPI_Capacity_Diagnostics;
generic
   type Transport_State is limited private;
   type Peer is private;
   No_Peer : Peer;
   with procedure Read_Clock
     (Now : out ACPI_PCI_Protocol.Wire.Time_Instant; Available : out Boolean);
   with procedure Read_Microseconds
     (Now : out ACPI_PCI_Protocol.Microsecond_Deadline; Available : out Boolean);
   with procedure Exchange
     (Channel : in out Transport_State; Destination : Peer;
      Deadline : ACPI_PCI_Protocol.Wire.Time_Instant;
      Item : ACPI_PCI_Protocol.Packet; Reply : out ACPI_PCI_Protocol.Packet;
      Delivered : out ACPI_PCI_Protocol.Wire.Delivery);
   with procedure Read_Deadline
     (IPC_Deadline : out ACPI_PCI_Protocol.Wire.Time_Instant;
      Data_Deadline : out ACPI_PCI_Protocol.Microsecond_Deadline);
   Initial_Target_Capacity : Positive := AML_Field_Access.Default_Maximum_Registers;
   with procedure Record_Capacity_Failure
     (Item : ACPI_Capacity_Diagnostics.Report) is null;
package ACPI_Field_PCI_Provider is
   package Q renames ACPI_PCI_Protocol;
   type Phase is (Fresh, Provisioning, Ready, Failed);
   type Local_Failure is (No_Failure, Invalid_Deadline, Clock_Unavailable,
     Clock_Reversed, Expired, Not_Sent, Completion_Unknown, Bad_Reply,
     Backend_Refused, Late_Reply, Capacity_Refused);
   type Context is limited private;
   function Status (IO : Context) return Phase;
   function Problem (IO : Context) return Local_Failure;
   function Last_Reply (IO : Context) return Q.Reply;
   function Identity (IO : Context) return AML_Identity.Identity;
   -- Bind a trusted transport without assuming segment zero/bus zero exists.
   -- No bus authority or metadata is acquired here; Install remains local.
   procedure Bind
     (IO : in out Context; Destination : Peer; Symbol : AML_Field_Access.Endpoint_ID;
      Deadline : Q.Wire.Time_Instant; Accepted : out Boolean);
   procedure Initialize
     (IO : in out Context; Destination : Peer; Symbol : AML_Field_Access.Endpoint_ID;
      Deadline : Q.Wire.Time_Instant; Accepted : out Boolean;
      Key : Q.P.Bus_Key := (others => <>));
   -- Explicit bounded setup, never called by Install. Reuses a prepared key
   -- without IPC; otherwise obtains and retains its named record/geometry.
   procedure Prepare_Bus
     (IO : in out Context; Key : Q.P.Bus_Key;
      Deadline : Q.Wire.Time_Instant; Accepted : out Boolean);
   -- Metadata association only. PCI_Config requires a known budgeted VM route;
   -- SystemMemory aliases require the complete region inside the described
   -- named PCI window. Unknown never means function00:00.0 or a guessed base.
   -- Current backend authority is checked separately for every dispatched read.
   procedure Install
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : out AML_Field_Access.Target; Accepted : out Boolean; For_Write : Boolean := False);
   procedure Describe
     (IO : Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   procedure Authorize
     (IO : Context; Target : AML_Field_Access.Target; For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch
     (IO : in out Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response);
   function Pending (IO : Context) return Boolean;
   function Uncertain (IO : Context) return AML_Field_Access.Observation;
   function Completed_Fault (IO : Context) return AML_Field_Access.Observation;
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
private
   type Bus_Binding is record
      Key : Q.P.Bus_Key;
      Epoch, ID, First : Interfaces.Unsigned_64 := 0;
   end record;
   Initial_Bus_Capacity : constant Positive := 1;
   package Bus_Store is new ACPI_Target_Storage (Bus_Binding, Initial_Bus_Capacity);
   type Association is record
      Geometry : AML_Field_Layout.Register_Geometry;
      Config : Q.P.Region;
      Offset : Interfaces.Unsigned_64 := 0;
      Epoch, ID : Interfaces.Unsigned_64 := 0;
   end record;
   package Storage is new ACPI_Target_Storage (Association, Initial_Target_Capacity);
   type Context is limited record
      Stage : Phase := Fresh;
      Failure : Local_Failure := No_Failure;
      Issuer : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : AML_Field_Access.Endpoint_ID := AML_Field_Access.No_Endpoint;
      Destination : Peer := No_Peer;
      Channel : Transport_State;
      Buses : Bus_Store.Context;
      Items : Storage.Context;
      Issued : AML_Field_Access.Counter := 0;
      Working, Seen_IPC, Seen_Data : Boolean := False;
      Last_IPC : Q.Wire.Time_Instant := 0;
      Last_Data : Q.Microsecond_Deadline := 0;
      Last : Q.Reply;
      Captured, Fault : AML_Field_Access.Observation;
   end record;
end ACPI_Field_PCI_Provider;
