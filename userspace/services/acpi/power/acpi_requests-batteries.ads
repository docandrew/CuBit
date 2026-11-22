pragma Ada_2022;
with ACPI_Battery_Cache;
with ACPI_Battery;
with ACPI_Bootstrap.Batteries;
generic
   with package Identity is new ACPI_Battery (Core => ACPI_Service, others => <>);
   with package Store is new ACPI_Battery_Cache
     (Core => ACPI_Service, Dynamic => Identity, others => <>);
package ACPI_Requests.Batteries with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Controller is new ACPI_Bootstrap.Batteries (Identity, Store);
   package Samples renames Store.Samples;
   type Cache is limited private;
   -- Internal owner search through this Server's private Core, not a wire
   -- enumeration or arbitrary invocation endpoint. Cursor is snapshot-local.
   procedure Find
     (Server : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Report : out Controller.Discovery_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old
       and then Report.Charged <= Budget;

   -- Trusted serialized owner, no wire invocation endpoint. Device comes from
   -- this Server's completed namespace. A node number grants no IO authority.
   -- Provisioned collector callbacks retain the existing ownership/charge and
   -- non-reentry contracts. Multiple caches may bind distinct battery devices.
   procedure Bind
     (Data : in out Cache; Server : State;
      Device : ACPI_Service.Namespace.Node_ID; Allowed : out Controller.Admission;
      Result : out Store.Binding_Result)
     with Pre => Valid (Server);
   procedure Forget (Data : in out Cache);
   procedure Invalidate (Data : in out Cache);
   procedure Prepare
     (Data : in out Cache; Server : in out State; Budget : Natural;
      Allowed : out Controller.Admission; Report : out Store.Refresh_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
   -- Observe this same Server's actual retained event at most once per cache.
   -- Does not claim/drain/ACK. Owner fans out to ALL consumers before ACK.
   procedure Observe_Claimed
     (Data : in out Cache; Server : State; Observed : out Boolean)
     with Pre => Valid (Server);
   procedure Publish
     (Data : in out Cache; Server : State; At_Time : Samples.Clock_Reading;
      Events_Healthy : Boolean; Report : out Store.Refresh_Report)
     with Pre => Valid (Server);
   -- Pure owned observation. Queued or unseen staged events suppress the
   -- payload even if the underlying cache previously held a fresh sample.
   function Read
     (Data : Cache; Server : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Samples.View
     with Pre => Valid (Server);
private
   type Cache is limited record
      Value : Store.State;
      Last_Receipt : Receipts.Number := 0;
   end record;
end ACPI_Requests.Batteries;
