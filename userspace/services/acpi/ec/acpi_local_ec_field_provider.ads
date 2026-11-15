pragma Ada_2022;
with ACPI_Local_EC_Commands;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
generic
   with package Commands is new ACPI_Local_EC_Commands (<>);
   -- Trusted independently admitted single EC domain. Before resolving a
   -- second controller's regions, the native owner must supply a distinct
   -- provider/domain or refuse that platform; geometry is not that proof.
   with function Cell_Allowed (Cell : Commands.T.Cell_Offset; For_Write : Boolean) return Boolean;
   -- Read the CURRENT finite owner window; never create or renew it here.
   with procedure Deadline (Until_Time : out Commands.T.Microseconds; Available : out Boolean);
   -- Same microsecond epoch as Commands.Clock. A bounded downward kernel
   -- wait only; a failed pause requests stop/drain, never resubmission.
   with procedure Pause (Target : Commands.T.Microseconds; Available : out Boolean);
   Work : Commands.T.Work_Budget;
package ACPI_Local_EC_Field_Provider with SPARK_Mode is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package T renames Commands.T;
   type Phase is (Fresh, Ready, Failed, Closed);
   type Context is limited private;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   function Is_Ready (IO : Context) return Boolean;
   function Pending (IO : Context) return Boolean;
   function Identity (IO : Context) return AML_Identity.Identity;
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean);
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
   subtype Cell_Count is Natural range 0 .. T.Cell_Offset'Pos(T.Cell_Offset'Last)+1;
   type Flags is array (T.Cell_Offset) of Boolean;
   type Context is limited record
      Stage : Phase := Fresh;
      Working : Boolean := False;
      Issuer : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : A.Endpoint_ID := A.No_Endpoint;
      Used : Flags := [others => False];
      Count : Cell_Count := 0;
      Captured, Fault : A.Observation;
   end record;
end ACPI_Local_EC_Field_Provider;
