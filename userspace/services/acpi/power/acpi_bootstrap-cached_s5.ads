pragma Ada_2022;
with ACPI_Sleep_Objects;
package ACPI_Bootstrap.Cached_S5 with SPARK_Mode is
   -- Fixed stored-value observer. It rejects methods, aliases and references;
   -- it cannot invoke AML, access an OperationRegion or enter a runtime window.
   procedure Observe_Static
     (Service : aliased in out ACPI_Service.State; Item : ACPI_Service.Node_Descriptor;
      Budget : Natural; Result : out ACPI_Service.Values.Result;
      Status : out ACPI_Service.Values.Access_Status);
   package Objects is new ACPI_Sleep_Objects (ACPI_Service, Observe_Static);
   type Copied_Types is record
      Available : Boolean := False;
      Registers : Objects.Register_Model := Objects.No_Sleep_Registers;
      Type_A, Type_B : Objects.Sleep_Type := 0;
   end record;
   Empty_Types : constant Copied_Types := (others => <>);
   type Snapshot is limited private;
   procedure Invalidate (Item : in out Snapshot);
   procedure Retire (Item : in out Snapshot);
   function Current_Types (Session : State; Item : Snapshot) return Copied_Types
     with Pre => Valid (Session);
   procedure Prepare (Session : in out State; Item : in out Snapshot;
      Registers : Objects.Register_Model; Budget : Natural;
      Result : out Objects.Query_Report)
     with Pre => Valid (Session), Post => Valid (Session) and then Result.Charged <= Budget;
private
   type Snapshot is limited record
      Types : Copied_Types;
      Context : ACPI_Service.Context_Key := ACPI_Service.No_Context;
      Retiring : Boolean := False;
   end record;
end ACPI_Bootstrap.Cached_S5;
