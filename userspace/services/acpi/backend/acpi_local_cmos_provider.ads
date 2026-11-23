pragma Ada_2022;
with ACPI_CMOS_Transactions;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;

generic
   -- One immutable startup receipt plus this owner's already validated FADT.
   -- No AML/config operand can supply or override the receipt.
   with procedure Read_Configuration
     (Profile : out ACPI_CMOS_Transactions.P.Profile;
      Century : out ACPI_CMOS_Transactions.I.Byte;
      Known_NMI, NMI_Disabled, Standard_Granted, Extended_Granted : out Boolean);
   with function Access_Allowed (For_Write : Boolean) return Boolean;
   -- The sole replaceable physical interface. It holds the exact pair's
   -- capability and performs select/data as one serialized, nonreentrant
   -- operation. It must return the actual completion; no AML/IPC callbacks.
   with procedure Transfer_Byte
     (Plan : ACPI_CMOS_Transactions.Transfer;
      Value : out ACPI_CMOS_Transactions.I.Byte;
      Outcome : out ACPI_CMOS_Transactions.Completion);
package ACPI_Local_CMOS_Provider with SPARK_Mode is
   package T renames ACPI_CMOS_Transactions;
   package P renames T.P;
   package I renames T.I;
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   CMOS_Space : constant := 5;
   type Phase is (Fresh, Ready, Failed, Closed);
   type Context is limited private;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean);
   function Is_Ready (IO : Context) return Boolean;
   function Pending (IO : Context) return Boolean;
   function Identity (IO : Context) return AML_Identity.Identity;
   function Register_Count (IO : Context) return Natural;
   procedure Export_Target (IO : Context; Ordinal : Positive;
      Target : out A.Target; Accepted : out Boolean);
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out L.Register_Geometry; Accepted : out Boolean);
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean);
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response);
   function Uncertain (IO : Context) return A.Observation;
   function Completed_Fault (IO : Context) return A.Observation;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean);
   procedure Close (IO : in out Context);
private
   type Cell_Flags is array (I.Byte) of Boolean;
   type Context is limited record
      Stage : Phase := Fresh;
      Working : Boolean := False;
      Issuer : AML_Identity.Identity := AML_Identity.No_Identity;
      Symbol : A.Endpoint_ID := A.No_Endpoint;
      Owner : T.State;
      Last_Cell, Century : I.Byte := 0;
      Used : Cell_Flags := [others => False];
      Count : Natural range 0 .. 256 := 0;
      Captured, Fault : A.Observation;
   end record;
end ACPI_Local_CMOS_Provider;
