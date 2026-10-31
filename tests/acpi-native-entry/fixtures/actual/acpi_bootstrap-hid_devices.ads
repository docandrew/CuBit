pragma Ada_2022;
with ACPI_HID_I2C_Discovery;
with ACPI_HID_I2C_Resolved;
generic
   with package Discovery is new ACPI_HID_I2C_Discovery
     (Core => ACPI_Service, others => <>);
   with package Resolver is new ACPI_HID_I2C_Resolved
     (Core => ACPI_Service, others => <>);
package ACPI_Bootstrap.HID_Devices with SPARK_Mode is
   type Admission is (Not_Ready, Admitted);
   type Search_Outcome is (Not_Ready, Found, No_Match, Incomplete, Invalid_Cursor, Failed);
   type Search_Report is record
      Outcome : Search_Outcome := Not_Ready;
      Scanned, Candidates, Charged : Natural := 0;
      Last_Identity : Discovery.Queries.Identification_Report;
   end record;
   -- Trusted owner search in this completed snapshot. Root starts enumeration;
   -- After selects only later nodes. No automatic AML retry, wire selector or
   -- hardware authority. Must not run while ANY owner discovery is Pending.
   procedure Find
     (Session : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Result : out Search_Report)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Result.Charged <= Budget;

   type Probe is limited private;
   type Report is record
      Firmware : Discovery.Report;
      Sources_Attempted : Boolean := False;
      Sources : Resolver.Report;
   end record;
   function Pending (Run : Probe) return Boolean;
   function Observe (Run : Probe) return Report;
   function Current (Run : Probe; Session : State) return Boolean
     with Pre => Valid (Session);
   -- Trusted owner only. Device is selected from this completed snapshot,
   -- not a wire node selector. Fixed callbacks close over admitted providers.
   -- Serialize this Probe with its Session; while Pending exclude other AML,
   -- catalog mutation and reset, as required by Discovery's retained _CRS.
   procedure Begin_Probe
     (Run : in out Probe; Session : in out State;
      Device : ACPI_Service.Namespace.Node_ID; Budget : Natural;
      Allowed : out Admission; Action : out Discovery.Action_Result)
     with Pre => Valid (Session), Post => Valid (Session);
   -- Only an actual completed discovery can reach Resolver. It receives the
   -- SAME device, exact copied prefix and observed descriptor register before
   -- any other owner work. Capacity retry preserves _CRS and does not repeat
   -- AML. Resolved descriptions do not grant controller/source authority.
   procedure Copy_And_Resolve
     (Run : in out Probe; Session : in out State; Data : out Firmware_Tables.Bytes;
      Allowed : out Admission; Action : out Discovery.Action_Result)
     with Pre => Valid (Session), Post => Valid (Session);
   -- Cleanup does not require readiness. Foreign-context rejection preserves
   -- the original retained owner; no implicit retry/replay or Core replacement.
   procedure Cancel
     (Run : in out Probe; Session : in out State;
      Action : out Discovery.Action_Result)
     with Pre => Valid (Session), Post => Valid (Session);
   -- Serialized trusted-owner selection and probe start. Search and all later
   -- probe work share Budget: Begin_Probe receives only Budget - Search.Charged.
   -- While this Run is Pending, return Busy without searching or replacing it.
   -- Other pending probes still require owner-wide exclusion. A Found search
   -- is not discovery success; inspect Action and Observe (Run). Copy retries
   -- retain the same _CRS via Copy_And_Resolve, never rerun this operation for size.
   procedure Begin_Next_Probe
     (Run : in out Probe; Session : in out State;
      After : ACPI_Service.Namespace.Node_ID; Budget : Natural;
      Device : out ACPI_Service.Namespace.Node_ID; Search : out Search_Report;
      Allowed : out Admission; Action : out Discovery.Action_Result)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Search.Charged <= Budget;
private
   type Probe is limited record
      Work : Discovery.Session;
      Device : ACPI_Service.Node_Descriptor := ACPI_Service.Values.No_Node;
      Tried : Boolean := False;
      Resolution : Resolver.Report;
   end record;
end ACPI_Bootstrap.HID_Devices;
