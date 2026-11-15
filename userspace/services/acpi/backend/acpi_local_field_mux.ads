pragma Ada_2022;
with ACPI_Local_Register_Provider;
with ACPI_Local_NVS_Provider;
with ACPI_Local_PCI_Provider;
with ACPI_Platform_Field_Provider;
with ACPI_Field_Routes;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with Interfaces;
generic
   type Declaration is private;
   with package Named is new ACPI_Local_Register_Provider (<>);
   with package Pool is new ACPI_Platform_Field_Provider (Declaration => Declaration,others => <>);
   with package Named_NVS is new ACPI_Local_NVS_Provider (<>);
   with package PCI is new ACPI_Local_PCI_Provider (<>);
   -- The ONE retained local Field guard. Must not inspect this mux/cache's
   -- Working flag: those flags are already latched before invoking it.
   with function Access_Allowed (For_Write : Boolean) return Boolean;
   Initial_Targets : Positive := 1;
package ACPI_Local_Field_Mux is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   subtype Word is Interfaces.Unsigned_64;
   type Phase is (Fresh, Initializing, Ready, Failed, Closed);
   type Context is limited private;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   function Is_Ready (IO : Context) return Boolean;
   function Pending (IO : Context) return Boolean;
   function Identity (IO : Context) return AML_Identity.Identity;
   function Pool_Available (IO : Context) return Boolean;
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean);
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean);
   procedure Resolve (IO : in out Context; Item : Declaration; Geometry : L.Register_Geometry;
      Deadline_Microseconds : Word; Target : out A.Target; Accepted : out Boolean);
   -- Named transactions take precedence regardless of requested direction.
   -- Partial overlaps, width aliases and ambiguity reserve those bytes rather
   -- than falling through to the broader read/write pool.
   procedure Resolve_PCI (IO : in out Context; Geometry : L.Register_Geometry;
      Allow_Setup : Boolean; Target : out A.Target; Accepted : out Boolean);
   procedure Close_Setup (IO : in out Context);
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response);
   function Uncertain (IO : Context) return A.Observation;
   function Completed_Fault (IO : Context) return A.Observation;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean);
   procedure Close (IO : in out Context);
private
   type Lane is (Named_Register, Configuration, Named_Data, Platform_Memory);
   type Lower_Context is limited record
      Registers : Named.Context;
      Data : Named_NVS.Context;
      Configuration_Data : PCI.Context;
      Platform : Pool.Context;
   end record;
   procedure Inner_Describe (IO : Lower_Context; Path : Lane; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Inner_Authorize (IO : Lower_Context; Path : Lane; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean);
   procedure Inner_Dispatch (IO : in out Lower_Context; Path : Lane; Item : A.Request; Reply : out A.Response);
   package Routes is new ACPI_Field_Routes (Lower_Context,Lane,Inner_Describe,Inner_Authorize,Inner_Dispatch,Initial_Targets);
   type Context is limited record
      Stage : Phase := Fresh;
      Working, Has_Pool : Boolean := False;
      Lower : Lower_Context;
      Routing : Routes.Context;
   end record;
end ACPI_Local_Field_Mux;
