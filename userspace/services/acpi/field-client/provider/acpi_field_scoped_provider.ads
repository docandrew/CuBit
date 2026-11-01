pragma Ada_2022;
with Interfaces;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_Field_Provider_View;
with ACPI_Locked_NVS_Protocol;
with ACPI_Field_Lease_Scope;
with ACPI_Global_Lock_Protocol;
with ACPI_Region_Protocol;
generic
   type Transport is limited private;
   type Peer is private;
   No_Peer : Peer;
   type Base_Context is limited private;
   with procedure Initialize_Base
     (IO : in out Base_Context; Destination : Peer; Symbol : AML_Field_Access.Endpoint_ID;
      Deadline : ACPI_Region_Protocol.Time_Instant; Accepted : out Boolean);
   with package Base is new ACPI_Field_Provider_View (Provider_Context => Base_Context, others => <>);
   with procedure Describe_Scoped_Read
     (IO : Base_Context; Target : AML_Field_Access.Target;
      ID : out ACPI_Locked_NVS_Protocol.Record_ID;
      Datum : out ACPI_Locked_NVS_Protocol.Geometry; Accepted : out Boolean);
   with package Scope is new ACPI_Field_Lease_Scope
     (Transport => Transport, Peer => Peer, No_Peer => No_Peer, others => <>);
   with procedure Begin_Window
     (Lease_Deadline : out Interfaces.Unsigned_64;
      IPC_Deadline : out ACPI_Region_Protocol.Time_Instant; Available : out Boolean);
   with procedure Read_Deadline (Deadline : out ACPI_Region_Protocol.Time_Instant);
   with procedure Cleanup_Deadline (Deadline : out ACPI_Region_Protocol.Time_Instant);
   Acquisition_Budget : ACPI_Global_Lock_Protocol.Attempt_Budget;
package ACPI_Field_Scoped_Provider is
   package W renames ACPI_Region_Protocol;
   type Startup_State is (Fresh, Ready, Failed);
   type Context is limited private;
   function Status (IO : Context) return Startup_State;
   function Identity (IO : Context) return AML_Identity.Identity;
   function Pending (IO : Context) return Boolean;
   function Cleanup_Required (IO : Context) return Boolean;
   procedure Initialize
     (IO : in out Context; Destination : Peer; Symbol : AML_Field_Access.Endpoint_ID;
      Deadline : W.Time_Instant; Accepted : out Boolean);
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target
     (IO : Context; Ordinal : Positive; Target : out AML_Field_Access.Target; Accepted : out Boolean);
   procedure Select_Target
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      For_Write : Boolean; Target : out AML_Field_Access.Target; Accepted : out Boolean);
   procedure Resolve_Module_Target
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean);
   procedure Describe
     (IO : Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   procedure Authorize
     (IO : Context; Target : AML_Field_Access.Target; For_Write : Boolean; Accepted : out Boolean);
   procedure Begin_Field
     (IO : in out Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean);
   procedure End_Field (IO : in out Context; Accepted : in out Boolean);
   procedure Dispatch
     (IO : in out Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response);
   function Uncertain (IO : Context) return AML_Field_Access.Observation;
   function Completed_Fault (IO : Context) return AML_Field_Access.Observation;
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
   -- Best-effort explicit close after all hook frames unwind. It does not
   -- rearm this provider, discard uncertainty or suppress backend cleanup.
   procedure Cleanup (IO : in out Context; Completed : out Boolean);
private
   package A renames AML_Field_Access;
   type Context is limited record
      Inner : Base.Context;
      Guard : Scope.Context;
      Channel : Transport;
      Consumed, Working, Failed : Boolean := False;
      Issued : A.Counter := 0;
      Data_Deadline : W.Time_Instant := W.No_Finite_Deadline;
      Captured, Fault : A.Observation;
   end record;
end ACPI_Field_Scoped_Provider;
