pragma Ada_2022;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_Locked_NVS_Protocol;
with ACPI_Field_Inventory_Owner;
with ACPI_Field_NVS_Provider;
with ACPI_Inventory_Protocol;
generic
   type Peer is private;
   with package Scalar is new ACPI_Field_Inventory_Owner (Endpoint => Peer, others => <>);
   with package Memory is new ACPI_Field_NVS_Provider (Peer => Peer, others => <>);
   with procedure Read_Clock
     (Now : out ACPI_Inventory_Protocol.Wire.Time_Instant; Available : out Boolean);
   with procedure Read_Window
     (Now, Deadline : out ACPI_Inventory_Protocol.Wire.Time_Instant);
   Maximum_Targets : Positive := 2 * AML_Field_Access.Default_Maximum_Registers;
package ACPI_Field_Combined_Provider is
   type Startup_State is (Fresh, Initializing, Ready, Failed);
   type Context is limited private;
   function Status (IO : Context) return Startup_State;
   function Identity (IO : Context) return AML_Identity.Identity;
   procedure Initialize
     (IO : in out Context; Destination : Peer; Symbol : AML_Field_Access.Endpoint_ID;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant; Accepted : out Boolean);
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target
     (IO : Context; Ordinal : Positive; Target : out AML_Field_Access.Target; Accepted : out Boolean);
   -- Metadata association only. Explicit named registers take precedence over
   -- a covering NVS record. NVS fallback is read-only and requires the entire
   -- declared region to fit its independently admitted backend record.
   procedure Select_Target
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      For_Write : Boolean; Target : out AML_Field_Access.Target; Accepted : out Boolean);
   -- Optional early-module callback. Resolves metadata only, considering both
   -- named directions without silently choosing between distinct targets.
   -- The executor independently rechecks geometry and each required right.
   procedure Resolve_Module_Target
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean);
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
   function Pending (IO : Context) return Boolean;
   function Uncertain (IO : Context) return AML_Field_Access.Observation;
   function Completed_Fault (IO : Context) return AML_Field_Access.Observation;
   -- There is no native terminal-evidence protocol. Preserve the original
   -- outer request and stop both lanes on uncertainty or submitted failure.
   procedure Reconciled
     (IO : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
private
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   type Lane is (Named_Register, NVS_Data);
   subtype Target_Count is Natural range 0 .. Maximum_Targets;
   subtype Target_Index is Positive range 1 .. Maximum_Targets;
   type Association is record
      Path : Lane := Named_Register;
      Inner : A.Target := A.No_Target;
      Geometry : L.Register_Geometry;
   end record;
   type Associations is array (Target_Index) of Association;
   type Context is limited record
      Stage : Startup_State := Fresh;
      Token : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : A.Endpoint_ID := A.No_Endpoint;
      Registers : Scalar.Context;
      NVS : Memory.Context;
      Count : Target_Count := 0;
      Rows : Associations;
      Issued : A.Counter := 0;
      Working : Boolean := False;
      Captured, Fault : A.Observation;
   end record;
end ACPI_Field_Combined_Provider;
