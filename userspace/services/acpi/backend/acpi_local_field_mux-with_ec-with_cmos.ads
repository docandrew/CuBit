pragma Ada_2022;
with ACPI_Local_CMOS_Provider;
-- Optional SystemCMOS lane over the existing EC/base mux. An absent CMOS
-- grant never disables those lanes. Space5 never falls through to raw I/O.
generic
   with package CMOS is new ACPI_Local_CMOS_Provider (<>);
package ACPI_Local_Field_Mux.With_EC.With_CMOS is
   type Context is limited private;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   function Is_Ready (IO : Context) return Boolean;
   function Pending (IO : Context) return Boolean;
   function Identity (IO : Context) return AML_Identity.Identity;
   function CMOS_Available (IO : Context) return Boolean;
   function Pool_Available (IO : Context) return Boolean;
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean);
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean);
   procedure Resolve (IO : in out Context; Item : Declaration; Geometry : L.Register_Geometry;
      Deadline_Microseconds : Word; Target : out A.Target; Accepted : out Boolean);
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
   type Lane is (Ordinary, CMOS_Controller);
   type Lower_Context is limited record
      Base : ACPI_Local_Field_Mux.With_EC.Context;
      Clock : CMOS.Context;
   end record;
   procedure Describe_Inner (IO : Lower_Context; Path : Lane; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Authorize_Inner (IO : Lower_Context; Path : Lane; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch_Inner (IO : in out Lower_Context; Path : Lane; Item : A.Request; Reply : out A.Response);
   package Links is new ACPI_Field_Routes (Lower_Context,Lane,Describe_Inner,Authorize_Inner,Dispatch_Inner,Initial_Targets);
   type Context is limited record
      Lower : Lower_Context;
      Routing : Links.Context;
      Initialized, Working, Closed, Has_CMOS : Boolean := False;
   end record;
end ACPI_Local_Field_Mux.With_EC.With_CMOS;
