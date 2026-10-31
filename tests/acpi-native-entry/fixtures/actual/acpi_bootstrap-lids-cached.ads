with ACPI_Sample_Cache;
with ACPI_Observation_Values;
generic
package ACPI_Bootstrap.Lids.Cached with SPARK_Mode is
   subtype Sample is ACPI_Observation_Values.Lid_Sample;
   Empty_Sample : constant Sample := ACPI_Observation_Values.Empty_Lid_Sample;
   package Samples is new ACPI_Sample_Cache (Sample, Empty_Sample);
   type Cache is limited private;
   procedure Bind (Data : in out Cache; Session : in out State; Budget : Natural;
      Report : out Discovery_Report) with Pre => Valid (Session), Post => Valid (Session);
   procedure Forget (Data : in out Cache);
   procedure Invalidate (Data : in out Cache);
   function Read (Data : Cache; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Samples.View;
   type Refresh_Outcome is (Collected, Updated, Not_Bound, Busy, Not_Ready,
      Invalid_Binding, Routing_Failed, Publication_Refused, Counter_Exhausted);
   type Refresh_Report is record
      Status : Refresh_Outcome := Not_Bound;
      Admission : Query_Admission := Not_Ready;
      Query : Driver.Query_Report;
      Publication : Samples.Publication_Result := Samples.Not_Pending;
   end record;
   -- Explicit owner phases: prepare once; route the SAME Core's actual events
   -- to every local consumer without AML/reset/reentry; then publish once.
   -- Event metadata is descriptive, never accepted from untrusted wire words.
   procedure Prepare (Data : in out Cache; Session : in out State; Budget : Natural;
      Report : out Refresh_Report) with Pre => Valid (Session), Post => Valid (Session);
   procedure Observe_Notification (Data : in out Cache; Session : State;
      Event : ACPI_Service.Notification_Observation) with Pre => Valid (Session);
   procedure Publish (Data : in out Cache; Session : State;
      At_Time : Samples.Clock_Reading; Events_Healthy : Boolean;
      Report : out Refresh_Report) with Pre => Valid (Session);
private
   type Cache is limited record
      Item : Binding := No_Binding;
      Bound, Pending : Boolean := False;
      Values : Samples.State;
      Expected : Samples.Revision_Number := 0;
      Query : Driver.Query_Report;
      Admission : Query_Admission := Not_Ready;
   end record;
end ACPI_Bootstrap.Lids.Cached;
