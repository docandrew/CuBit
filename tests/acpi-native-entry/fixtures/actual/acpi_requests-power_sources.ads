pragma Ada_2022;
with ACPI_Power_Source;
with ACPI_Bootstrap.Power_Sources;
with ACPI_Bootstrap.Power_Sources.Cached;
generic
   with package Driver is new ACPI_Power_Source (Core => ACPI_Service, others => <>);
package ACPI_Requests.Power_Sources with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Controller is new ACPI_Bootstrap.Power_Sources (Driver);
   package Cached is new Controller.Cached;
   package Samples renames Cached.Samples;
   type Cache is limited private;
   -- Internal serialized owner API, never an observer invocation endpoint.
   -- Keep this Cache with one Server for its entire lifetime. The supplied
   -- evaluator closes over the owner's admitted hardware context. No untrusted
   -- event, method name, node, register address or evaluator is accepted here.
   procedure Bind (Data : in out Cache; Server : in out State; Budget : Natural;
      Report : out Controller.Discovery_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
   procedure Forget (Data : in out Cache);
   procedure Invalidate (Data : in out Cache);
   procedure Prepare (Data : in out Cache; Server : in out State; Budget : Natural;
      Report : out Cached.Refresh_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
   -- Route the SAME Server's actual retained notification to this consumer
   -- before acknowledging its broker receipt. Does not claim, drain or ACK.
   -- Each active receipt is observed at most once by this Cache. The owner
   -- still fans out to every other consumer before acknowledging the event.
   procedure Observe_Claimed (Data : in out Cache; Server : State; Observed : out Boolean)
     with Pre => Valid (Server);
   procedure Publish (Data : in out Cache; Server : State;
      At_Time : Samples.Clock_Reading; Events_Healthy : Boolean;
      Report : out Cached.Refresh_Report)
     with Pre => Valid (Server);
   -- Pure cached copy, no AML execution. An active unseen receipt conservatively
   -- suppresses freshness until owner routing, even when the Core FIFO is empty.
   function Read (Data : Cache; Server : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Samples.View
     with Pre => Valid (Server);
   -- Same private Core and fixed evaluator as this owner's cache.
   procedure Sample_Ready (Server : in out State; Budget : Natural;
      Report : out Controller.Sampling_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old
       and then Report.Charged <= Budget;
private
   type Cache is limited record
      Value : Cached.Cache;
      Last_Receipt : Receipts.Number := 0;
   end record;

end ACPI_Requests.Power_Sources;
