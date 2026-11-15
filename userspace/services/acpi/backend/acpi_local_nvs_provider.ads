pragma Ada_2022;
with ACPI_Backend_NVS_Records;
with ACPI_Target_Storage;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_NVS_Writes;
generic
   with package Records is new ACPI_Backend_NVS_Records (<>);
   Backing : in out Records.Context;
   Scope : Records.Word;
   -- Checks the retained physical Field scope, not this provider's Working.
   with function Access_Allowed (For_Write : Boolean) return Boolean;
   Initial_Targets : Positive := 1;
   with procedure Write_Access (Physical,Virtual : Records.Word; Width : Records.T.Access_Width;
      Value : Records.Word; Perform : Boolean; Result : in out ACPI_NVS_Writes.Completion) is null;
package ACPI_Local_NVS_Provider is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   type Phase is (Fresh, Initializing, Ready, Failed, Closed);
   type Match_Kind is (Absent, Whole_Record, Reserved, Unavailable);
   type Context is limited private;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   function Is_Ready (IO : Context) return Boolean;
   function Pending (IO : Context) return Boolean;
   function Identity (IO : Context) return AML_Identity.Identity;
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean);
   -- The complete original region must fit exactly one retained NVS record.
   -- Any partial overlap or ambiguity reserves the bytes, preventing fallback
   -- to a broader read/write provider. No mapping or wire request; default callback permits no write.
   procedure Resolve (IO : in out Context; Geometry : L.Register_Geometry;
      Target : out A.Target; Match : out Match_Kind; Accepted : out Boolean);
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean);
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response);
   function Uncertain (IO : Context) return A.Observation;
   function Completed_Fault (IO : Context) return A.Observation;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean);
   procedure Close (IO : in out Context);
private
   type Association is record
      Geometry : L.Register_Geometry;
      ID : Records.Record_ID;
      Offset : Records.Word;
      Writable : Boolean := False;
   end record;
   package Store is new ACPI_Target_Storage (Association,Initial_Targets);
   type Context is limited record
      Stage : Phase := Fresh;
      Working : Boolean := False;
      Issuer : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : A.Endpoint_ID := A.No_Endpoint;
      Epoch : Records.Epoch_Number := Records.No_Epoch;
      Count : Records.Record_Count := 0;
      Targets : Store.Context;
      Sequence : A.Counter := 0;
      Captured, Fault : A.Observation;
   end record;
end ACPI_Local_NVS_Provider;
