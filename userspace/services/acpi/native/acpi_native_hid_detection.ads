pragma Ada_2022;
with ACPI_Requests;
with ACPI_Service;
with ACPI_HID_I2C_Candidate;
with ACPI_HID_I2C_Channels;
-- Fixed, process-lifetime startup observation on the native service's Core.
-- No launch, endpoint discovery, hardware admission or event-source enrollment.
package ACPI_Native_HID_Detection is
   Maximum_AML_Fuel : constant := 100_000;
   Maximum_Resource_Bytes : constant := 65_536;
   Maximum_Candidates : constant := 16;
   Maximum_Source_Span_Bytes : constant := 256;
   package Candidates is new ACPI_HID_I2C_Candidate (Maximum_Source_Span_Bytes);
   package Channels is new ACPI_HID_I2C_Channels (Candidates);
   subtype Fuel_Budget is Natural range 0 .. Maximum_AML_Fuel;
   subtype Resource_Budget is Natural range 0 .. Maximum_Resource_Bytes;
   subtype Candidate_Budget is Positive range 1 .. Maximum_Candidates;
   subtype Candidate_Count is Natural range 0 .. Maximum_Candidates;
   type Policy is record
      Fuel : Fuel_Budget := 10_000;
      Resource_Bytes : Resource_Budget := Maximum_Resource_Bytes;
      Candidates : Candidate_Budget := 4;
   end record;
   type Outcome is (Not_Attempted, Not_Ready, No_Devices, Finished,
      Budget_Exhausted, Search_Failed, Discovery_Failed, Sources_Failed,
      Capacity_Refused, Candidate_Limit, Cleanup_Failed, Owner_Error, Recovery_Required, Channel_Refused);
   subtype Observation_Index is Positive range 1 .. Maximum_Candidates;
   type Observation is record
      Device, I2C_Controller, GPIO_Controller : ACPI_Service.Node_Descriptor :=
        ACPI_Service.Values.No_Node;
      Channel_Data : Channels.Preparation;
   end record;
   type Observation_Array is array (Observation_Index) of Observation;
   type Report is record
      Status : Outcome := Not_Attempted;
      Candidates, Observed : Candidate_Count := 0;
      Charged : Natural := 0;
      Channel_Attempted : Boolean := False;
      Last_Channel_Status : Channels.Outcome := Channels.Invalid_I2C_Target;
      -- Only Items(1..Observed) are complete copied descriptions. They remain
      -- historical: later AML can change resources despite node identity.
      Items : Observation_Array;
      Cleanup_Required : Boolean := False;
      Recovery_Needed : Boolean := False;
   end record;
   -- Called once after snapshot startup, before serving IPC. The process latch
   -- retains every outcome, including refusal, and excludes automatic replay.
   -- Serialize with ALL other AML work. If cleanup fails, the probe and owning
   -- Core remain process-lived; no further AML or reset may use this owner.
   -- Fatal/Field/provider uncertainty also sets Recovery_Needed even when no
   -- retained value is pending. This separately prevents later owner AML.
   -- Unexpected exceptions latch Owner_Error; in-flight calls retain recovery.
   -- Fuel bounds AML operations, not wall time. Resource/path limits are local
   -- policy budgets, not ACPI limits. Failure never admits partial resources.
   -- Prepared rows carry no authority. The launcher must separately admit
   -- backend semantics and the owner must revalidate current resources and all
   -- three identities before a later handoff. No rows are wire-visible here.
   procedure Run (Server : in out ACPI_Requests.State; Result : out Report;
      Limits : Policy := (others => <>));
end ACPI_Native_HID_Detection;
