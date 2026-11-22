pragma Ada_2022;
with ACPI_Platform_Region_Cache;
with ACPI_Target_Storage;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
generic
   with package Maps is new ACPI_Platform_Region_Cache (<>);
   Memory : in out Maps.Context;
   type Declaration is private;
   -- Actual matches a copied, live Core declaration to this exact geometry
   -- and supplies copied profiling metadata. It never exposes an arena alias.
   with procedure Validate_Declaration (Item : Declaration;
      Geometry : AML_Field_Layout.Register_Geometry;
      Origin : out Maps.Source; Accepted : out Boolean);
   -- Required check over the WHOLE rounded envelope, not just declared bytes.
   -- Refuse overlap with established WB NVS/FACS and PCI mappings. Kernel
   -- pool membership/claims remain the independent authority check.
   with function Compatible_Envelope (First_Page, Pages : Maps.Word) return Boolean;
   Initial_Targets : Positive := 1;
package ACPI_Platform_Field_Provider is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   type Phase is (Fresh, Ready, Failed, Closed);
   type Context is limited private;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   function Identity (IO : Context) return AML_Identity.Identity;
   function Pending (IO : Context) return Boolean;
   function Is_Ready (IO : Context) return Boolean;
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean);
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean);
   procedure Resolve (IO : in out Context; Item : Declaration;
      Geometry : L.Register_Geometry; Deadline_Microseconds : Maps.Word;
      Target : out A.Target; Accepted : out Boolean);
   -- Trusted startup resolution only. Reserve target storage before mapping;
   -- expose nothing until the cache admits the complete declared region.
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response);
   function Uncertain (IO : Context) return A.Observation;
   function Completed_Fault (IO : Context) return A.Observation;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean);
   procedure Close (IO : in out Context);
   function Capacity (IO : Context) return Natural;
private
   type Association is record
      Geometry : L.Register_Geometry;
      Region : Maps.Handle := Maps.No_Region;
      Available : Boolean := False;
   end record;
   package Store is new ACPI_Target_Storage (Association,Initial_Targets);
   type Context is limited record
      Stage : Phase := Fresh;
      Working : Boolean := False;
      Token : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : A.Endpoint_ID := A.No_Endpoint;
      Epoch : A.Counter := 0;
      Ready_Count : Natural := 0;
      Rows : Store.Context;
      Captured, Fault : A.Observation;
   end record;
end ACPI_Platform_Field_Provider;
