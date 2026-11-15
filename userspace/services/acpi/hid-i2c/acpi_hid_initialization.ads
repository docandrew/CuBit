pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Device_Initialization;
with ACPI_HID_I2C;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Initialization is new ACPI_Device_Initialization
     (Core => Core, others => <>);
   with package Queries is new ACPI_HID_I2C (Core => Core, others => <>);
package ACPI_HID_Initialization with SPARK_Mode is
   type Outcome is
     (Eligible, Excluded, History_Unknown, Initialization_Incomplete,
      Stale_History, Status_Failed, Budget_Exhausted,
      Recovery_Required);
   function Name (Value : Outcome) return String;
   type Report is record
      Result : Outcome := History_Unknown;
      History : Initialization.Path_Outcome := Initialization.History_Unknown;
      Charged : Natural := 0;
      Last_Node : Core.Node_Descriptor := Core.Values.No_Node;
      Last_Status : Queries.Status_Report;
   end record;
   -- True only when Last_Status holds an actual available observation.
   -- A historical refusal retains the requested Item with unknown status.
   function Status_Known (Item : Report) return Boolean;
   -- Serialized owner only: use the actual initialization Run for this Core.
   -- First validate historical initialization, then observe current ancestor
   -- status root-to-leaf. An absent functioning ancestor permits descent; the
   -- leaf must be present and enabled. No INI replay, hardware authority,
   -- readiness guarantee or atomic status-snapshot claim. Observe_Status owns
   -- result cleanup; Recovery_Required forbids further owner AML until settled.
   procedure Check_Path
     (Run : Initialization.Session; Service : aliased in out Core.State;
      Item : Core.Node_Descriptor; Budget : Natural; Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then Result.Charged <= Budget;
end ACPI_HID_Initialization;
