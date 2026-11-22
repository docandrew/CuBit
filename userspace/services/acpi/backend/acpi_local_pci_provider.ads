pragma Ada_2022;
with ACPI_Local_PCI_Results;
with ACPI_PCI_Bus_Geometry;
with ACPI_Target_Storage;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
generic
   with procedure Prepare (Key : ACPI_PCI_Bus_Geometry.P.Bus_Key;
      Deadline_Milliseconds : ACPI_Local_PCI_Results.Word;
      Result : out ACPI_Local_PCI_Results.Window_Description);
   with procedure Inspect (Epoch,ID : ACPI_Local_PCI_Results.Word;
      Result : out ACPI_Local_PCI_Results.Window_Description);
   with procedure Read_Data (Epoch,ID : ACPI_Local_PCI_Results.Word;
      Region : ACPI_PCI_Bus_Geometry.P.Region; Offset : ACPI_Local_PCI_Results.Word;
      Width : ACPI_PCI_Bus_Geometry.P.T.Access_Width;
      Deadline_Microseconds : ACPI_Local_PCI_Results.Word;
      Guard : not null access function return Boolean; Result : out ACPI_Local_PCI_Results.Result);
   with function Backing_Pending return Boolean;
   with function Setup_Allowed return Boolean;
   with function Access_Allowed (For_Write : Boolean) return Boolean;
   with procedure Deadlines (Setup_Milliseconds,Data_Microseconds : out ACPI_Local_PCI_Results.Word;
      Available : out Boolean);
   with procedure Resolve_Alias (Geometry : AML_Field_Layout.Register_Geometry;
      Key : in out ACPI_PCI_Bus_Geometry.P.Bus_Key; Accepted : in out Boolean) is null;
   Initial_Targets : Positive := 1;
package ACPI_Local_PCI_Provider is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package G renames ACPI_PCI_Bus_Geometry;
   package P renames G.P;
   package R renames ACPI_Local_PCI_Results;
   type Phase is (Fresh, Ready, Failed, Closed);
   type Match_Kind is (Absent, Whole_Window, Reserved, Unavailable);
   type Context is limited private;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   function Is_Ready (IO : Context) return Boolean;
   function Pending (IO : Context) return Boolean;
   function Identity (IO : Context) return AML_Identity.Identity;
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean);
   procedure Resolve (IO : in out Context; Geometry : L.Register_Geometry;
      Allow_Setup : Boolean; Target : out A.Target; Match : out Match_Kind; Accepted : out Boolean);
   -- Only typed known PCI routes or complete aliases of retained/prepared
   -- independently acquired windows. Partial/ambiguous aliases reserve bytes.
   -- Setup is one attempt per key, latched before calling downward; never
   -- allowed by selection, after Close_Setup, or when Setup_Allowed is false.
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean);
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response);
   function Uncertain (IO : Context) return A.Observation;
   function Completed_Fault (IO : Context) return A.Observation;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean);
   procedure Close_Setup (IO : in out Context);
   procedure Close (IO : in out Context);
private
   type Bus_Phase is (Requested, Retained, Refused);
   type Bus is record Key : P.Bus_Key; Stage : Bus_Phase := Requested; Window : R.Window_Description; end record;
   package Buses is new ACPI_Target_Storage (Bus,1);
   type Association is record
      Geometry : L.Register_Geometry;
      Window : R.Window_Description;
      Region : P.Region;
      Offset : R.Word;
   end record;
   package Store is new ACPI_Target_Storage (Association,Initial_Targets);
   type Context is limited record
      Stage : Phase := Fresh;
      Working, Setup_Closed : Boolean := False;
      Issuer : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : A.Endpoint_ID := A.No_Endpoint;
      Rows : Store.Context;
      Windows : Buses.Context;
      Sequence : A.Counter := 0;
      Captured, Fault : A.Observation;
   end record;
end ACPI_Local_PCI_Provider;
