pragma Ada_2022;
with ACPI_Battery_Cache;
with ACPI_Battery;
with ACPI_Battery_Cache.Notifications;
generic
   with package Identity is new ACPI_Battery (Core => ACPI_Service, others => <>);
   with package Store is new ACPI_Battery_Cache
     (Core => ACPI_Service, Dynamic => Identity, others => <>);
package ACPI_Bootstrap.Batteries with SPARK_Mode is
   package Notifications is new Store.Notifications;
   type Admission is (Not_Ready, Admitted);
   type Discovery_Outcome is (Not_Ready, Found, No_Match, Incomplete, Invalid_Cursor, Failed);
   type Discovery_Report is record
      Outcome : Discovery_Outcome := Not_Ready;
      Scanned, Candidates, Charged : Natural := 0;
      Last_Identity : Identity.Identification_Report;
   end record;
   -- Trusted bounded search after a previously selected node in this SAME
   -- snapshot; Root starts enumeration. No automatic retry/resume of AML.
   -- Whole topology is checked after each identification. A match describes
   -- identity only, not presence, initialization, hardware or source authority.
   procedure Find
     (Session : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Report : out Discovery_Report)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Report.Charged <= Budget;

   -- Trusted owner only. Device is selected from this Session's snapshot, not
   -- a wire selector or hardware authority. Collector rechecks battery identity
   -- and presence on each refresh. Keep cache/session lifetime serialized;
   -- never replace/reset the owning Core while results or updates are pending.
   procedure Bind
     (Data : in out Store.State; Session : State;
      Device : ACPI_Service.Namespace.Node_ID; Allowed : out Admission;
      Result : out Store.Binding_Result)
     with Pre => Valid (Session);
   procedure Prepare
     (Data : in out Store.State; Session : in out State; Budget : Natural;
      Allowed : out Admission; Report : out Store.Refresh_Report)
     with Pre => Valid (Session), Post => Valid (Session);
   -- Must be the same owner's actual event; the Requests child supplies its
   -- retained broker observation. Never route untrusted copied metadata here.
   procedure Observe_Notification
     (Data : in out Store.State; Session : State;
      Event : ACPI_Service.Notification_Observation;
      Result : out Notifications.Routing_Result)
     with Pre => Valid (Session);
   procedure Publish
     (Data : in out Store.State; Session : State;
      At_Time : Store.Samples.Clock_Reading; Events_Healthy : Boolean;
      Report : out Store.Refresh_Report)
     with Pre => Valid (Session);
   function Read
     (Data : Store.State; Session : State; Now : Store.Samples.Clock_Reading;
      Maximum_Age : Store.Samples.Tick) return Store.Samples.View
     with Pre => Valid (Session);
end ACPI_Bootstrap.Batteries;
