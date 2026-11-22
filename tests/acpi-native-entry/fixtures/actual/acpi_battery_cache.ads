with ACPI_Service_Core;
with ACPI_Battery;
with ACPI_Battery_Information;
with ACPI_Battery_Information_Reader;
with ACPI_Battery_Observation;
with ACPI_Sample_Cache;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Format is new ACPI_Battery_Information (<>);
   with package Dynamic is new ACPI_Battery (Core => Core, others => <>);
   with package Static_Info is new ACPI_Battery_Information_Reader
     (Core => Core, Format => Format, others => <>);
   with package Collector is new ACPI_Battery_Observation
     (Core => Core, Format => Format, Dynamic => Dynamic, Static_Info => Static_Info);
package ACPI_Battery_Cache with SPARK_Mode is
   package Samples is new ACPI_Sample_Cache (Collector.Observation, Collector.Empty_Observation);
   type State (Initial_Revision : Samples.Revision_Number := 0) is limited private;
   type Binding_Result is (Bound, Invalid_Device, Counter_Exhausted);
   procedure Bind (Cache : in out State; Service : Core.State;
      Device : Core.Node_Descriptor; Result : out Binding_Result)
      with Pre => Core.Valid (Service);
   procedure Forget (Cache : in out State);
   procedure Invalidate (Cache : in out State);
   -- Read has no Service or firmware callback; only owned cached values escape.
   function Read (Cache : State; Now : Samples.Clock_Reading;
                  Maximum_Age : Samples.Tick) return Samples.View;
   type Refresh_Result is (Collected, Updated, Already_Pending, Not_Bound, Invalid_Binding, Events_Failed,
                          Publication_Refused, Counter_Exhausted);
   type Refresh_Report is record
      Status : Refresh_Result := Not_Bound;
      Collection : Collector.Query_Report;
      Publication : Samples.Publication_Result := Samples.Not_Pending;
      Events_Drained : Boolean := False;
   end record;
   -- Invalidate before AML; return owned data, without making it readable.
   procedure Prepare_Refresh (Cache : in out State; Service : aliased in out Core.State;
      Budget : Natural; Report : out Refresh_Report)
      with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- Owner routes a bounded batch between Prepare and Publish, without invoking
   -- AML or resetting/reentering Service. Events_Healthy=False fails closed.
   -- The cache's private pending update is consumed once. Only one can be
   -- outstanding per cache. Updated can record absence/failure; inspect the
   -- collection outcome. No firmware invocation, retries or peer callback here.
   procedure Publish_Refresh (Cache : in out State; Service : Core.State;
      At_Time : Samples.Clock_Reading;
      Events_Healthy : Boolean; Report : out Refresh_Report)
      with Pre => Core.Valid (Service);
private
   type Pending_Update is limited record
      Armed : Boolean := False;
      Expected : Samples.Revision_Number := 0;
      Device : Core.Node_Descriptor := Core.Values.No_Node;
      Kind : Samples.Publication := Samples.Acquisition_Failed;
      Value : Collector.Observation := Collector.Empty_Observation;
      Collection : Collector.Query_Report;
   end record;
   type State (Initial_Revision : Samples.Revision_Number := 0) is limited record
      Data : Samples.State (Initial_Revision);
      Device : Core.Node_Descriptor := Core.Values.No_Node;
      Has_Binding : Boolean := False;
      Update : Pending_Update;
   end record;
end ACPI_Battery_Cache;
