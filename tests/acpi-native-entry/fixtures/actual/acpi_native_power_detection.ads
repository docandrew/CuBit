pragma Ada_2022;
with ACPI_Requests;
with ACPI_FADT.Sleep_Registers;
package ACPI_Native_Power_Detection is
   Maximum_AML_Fuel : constant := 100_000;
   Maximum_Batteries : constant := 16;
   subtype Fuel_Budget is Natural range 0 .. Maximum_AML_Fuel;
   subtype Battery_Budget is Positive range 1 .. Maximum_Batteries;
   subtype Battery_Count is Natural range 0 .. Maximum_Batteries;
   type Policy is record
      Fuel : Fuel_Budget := 10_000;
      Batteries : Battery_Budget := 4;
   end record;
   type Lid_State is (Unknown, Closed, Open);
   type Source_State is (Unknown, Offline, Online);
   type Outcome is (Not_Attempted, Not_Ready, No_Devices, Finished,
      Budget_Exhausted, Lid_Failed, Source_Failed, Battery_Failed,
      Battery_Limit, Recovery_Required, Owner_Error);
   type Report is record
      Status : Outcome := Not_Attempted;
      Sleep_Registers : ACPI_FADT.Sleep_Registers.Source_Report;
      Lid_Observed, Source_Observed : Boolean := False;
      Lid_Query_Failed, Source_Query_Failed : Boolean := False;
      Lid : Lid_State := Unknown;
      Source : Source_State := Unknown;
      Batteries, Sampled_Batteries, Absent_Batteries : Battery_Count := 0;
      Charged : Natural := 0;
      Recovery_Needed : Boolean := False;
   end record;
   -- One process-lifetime startup pass, serialized after HID cleanup and before
   -- IPC. Same private Core and one total budget. First lid/AC source, bounded
   -- battery enumeration. No notification capture/ACK, live cache publication,
   -- transition policy or hardware authority. Reports are historical ordered
   -- observations, not atomic or fresh state. Every outcome is latched; never
   -- reset/reenter this owner. Recovery_Needed prevents later AML/registration.
   -- Ordinary lid/source refusals preserve their failure flags and permit the
   -- independent later families to run within the remaining total budget.
   -- If all later work finishes, Status reports the first such family failure.
   -- Budget, readiness, battery failure and uncertain effects remain terminal.
   procedure Run (Server : in out ACPI_Requests.State; Result : out Report;
      Limits : Policy := (others => <>));
end ACPI_Native_Power_Detection;
