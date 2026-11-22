pragma Ada_2022;
with ACPI_Battery;
with ACPI_Battery_Observation;
generic
   with package Identity is new ACPI_Battery (Core => ACPI_Service, others => <>);
   with package Collector is new ACPI_Battery_Observation
     (Core => ACPI_Service, Dynamic => Identity, others => <>);
package ACPI_Bootstrap.Battery_Observations with SPARK_Mode is
   type Discovery_Outcome is (Not_Ready, Found, No_Match, Incomplete, Invalid_Cursor, Failed);
   type Discovery_Report is record
      Outcome : Discovery_Outcome := Not_Ready;
      Scanned, Candidates, Charged : Natural := 0;
      Last_Identity : Identity.Identification_Report;
   end record;
   subtype Observation_Report is Collector.Query_Report;
   -- Startup point observations only, not cached/fresh state. Device startup
   -- must be complete; notification capture/healthy-source claims are not made.
   -- Unlike cache enrollment, this never requires or enables notifications.
   procedure Find (Session : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Report : out Discovery_Report)
     with Pre => Valid (Session), Post => Valid (Session) and then Report.Charged <= Budget;
   -- Fixed collector, same private Core; identity/presence/BIX/BST/confirmation
   -- share Budget. No wire selector, retained handle, event ACK or IO authority.
   -- Owner serializes all AML and stops on cleanup/provider/Field uncertainty.
   procedure Observe (Session : in out State; Device : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Admitted : out Boolean; Report : out Observation_Report)
     with Pre => Valid (Session), Post => Valid (Session) and then Report.Charged <= Budget;
end ACPI_Bootstrap.Battery_Observations;
