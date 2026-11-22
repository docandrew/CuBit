with ACPI_Service_Core;
with ACPI_Battery;
with ACPI_Battery_Status;
with ACPI_Battery_Information;
with ACPI_Battery_Information_Reader;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Format is new ACPI_Battery_Information (<>);
   with package Dynamic is new ACPI_Battery (Core => Core, others => <>);
   with package Static_Info is new ACPI_Battery_Information_Reader
     (Core => Core, Format => Format, others => <>);
package ACPI_Battery_Observation with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type Phase is (Identification, Presence, Information, Dynamic_Status,
                  Confirm_Presence, Finished);
   type Outcome is (Sampled, Not_Battery, Absent, Not_Ready,
      Identification_Failed, Presence_Failed, Information_Failed,
      Status_Failed, Presence_Changed, Budget_Exhausted,
      Cleanup_Failed, Provider_Contract_Error);
   type Observation is record
      Information : Format.Information := Format.Empty_Information;
      Status : ACPI_Battery_Status.Sample := ACPI_Battery_Status.Empty_Sample;
   end record;
   Empty_Observation : constant Observation := (others => <>);
   subtype Attempt_Count is Natural range 0 .. 5;
   type Query_Report is record
      Status : Outcome := Identification_Failed;
      Reached : Phase := Identification;
      Attempts : Attempt_Count := 0;
      Charged : Natural := 0;
      Value : Observation := Empty_Observation;
      Identity : Dynamic.Identification_Report;
      Before_Presence, After_Presence : Dynamic.Presence_Report;
      Static_Data : Static_Info.Query_Report;
      Dynamic_Data : Dynamic.Query_Report;
   end record;
   -- Service-owner API, never an observer's arbitrary AML evaluation endpoint.
   -- Identity -> presence -> information -> status -> presence confirmation.
   -- All stages share Budget. No later query follows absence/failure; prior
   -- AML effects are never replayed. A changed _STA discards the bundle.
   -- This is a sequence of observations, not an atomic hardware snapshot:
   -- the owner must also handle Notify/hot-swap and cache invalidation, and
   -- separately establish initialization and admitted hardware context.
   procedure Query (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Query_Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then Report.Charged <= Budget
         and then (if Report.Status /= Sampled then Report.Value = Empty_Observation)
         and then (Report.Status in Cleanup_Failed | Provider_Contract_Error
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
end ACPI_Battery_Observation;
