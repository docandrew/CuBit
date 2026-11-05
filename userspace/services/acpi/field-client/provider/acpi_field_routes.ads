pragma Ada_2022;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_Target_Storage;
with ACPI_Capacity_Diagnostics;
-- Associations only. Callers first resolve a target through its original
-- provider (including original-declaration admission for firmware data).
-- No geometry resolver, publication, physical lock or retry lives here.
generic
   type User_Context is limited private;
   type Lane is (<>);
   with procedure Describe_Inner
     (IO : User_Context; Path : Lane; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   with procedure Authorize_Inner
     (IO : User_Context; Path : Lane; Target : AML_Field_Access.Target;
      For_Write : Boolean; Accepted : out Boolean);
   with procedure Dispatch_Inner
     (IO : in out User_Context; Path : Lane; Item : AML_Field_Access.Request;
      Reply : out AML_Field_Access.Response);
   Initial_Targets : Positive := 1;
package ACPI_Field_Routes is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   type Context is limited private;
   procedure Initialize (Owner : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   function Identity (Owner : Context) return AML_Identity.Identity;
   function Pending (Owner : Context) return Boolean;
   function Ready (Owner : Context) return Boolean;
   function Register_Count (Owner : Context) return Natural;
   function Observe_Capacity (Owner : Context) return ACPI_Capacity_Diagnostics.Report;
   procedure Adopt (Owner : in out Context; IO : User_Context; Path : Lane;
      Inner : A.Target; Geometry : L.Register_Geometry;
      Target : out A.Target; Accepted : out Boolean);
   procedure Export_Target (Owner : Context; Ordinal : Positive;
      Target : out A.Target; Accepted : out Boolean);
   procedure Describe (Owner : Context; IO : User_Context; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Authorize (Owner : Context; IO : User_Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch (Owner : in out Context; IO : in out User_Context;
      Item : A.Request; Reply : out A.Response);
   function Uncertain (Owner : Context) return A.Observation;
   function Completed_Fault (Owner : Context) return A.Observation;
   -- Terminal AML failures remain terminal after lower-owner cleanup.
   procedure Reconciled (Owner : Context; Expected : A.Observation; Accepted : out Boolean);
private
   type Association is record
      Path : Lane;
      Inner : A.Target;
      Geometry : L.Register_Geometry;
   end record;
   package Store is new ACPI_Target_Storage (Association, Initial_Targets);
   type Context is limited record
      Rows : Store.Context;
      Token : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : A.Endpoint_ID := A.No_Endpoint;
      Consumed, Initialized, Working, Stopped : Boolean := False;
      Issued : A.Counter := 0;
      Captured, Fault : A.Observation;
   end record;
end ACPI_Field_Routes;
