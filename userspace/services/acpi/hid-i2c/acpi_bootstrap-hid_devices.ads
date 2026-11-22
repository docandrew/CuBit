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
   -- Process-local owner policy, invoked before any HID identity AML. This
   -- gate grants no authority. Recovery_Required preserves owner cleanup debt;
   -- Excluded skips a non-candidate; Node_Error skips a known ordinary AML
   -- failure, counts it and continues. Neither grants device authority.
   type Policy_Outcome is
     (Eligible, Excluded, Policy_Incomplete, Policy_Failed, Recovery_Required, Node_Error);
   type Policy_Report is record
      Outcome : Policy_Outcome := Policy_Incomplete;
      Charged : Natural := 0;
   end record;
   Maximum_Captured_Search_Errors : constant Positive := 8;
   subtype Search_Error_Count is Natural range 0 .. Maximum_Captured_Search_Errors;
   subtype Search_Error_Index is Positive range 1 .. Maximum_Captured_Search_Errors;
   type Search_Error_Kind is (Policy_Error, Identity_Error);
   type Search_Error is record
      Kind : Search_Error_Kind := Policy_Error;
      Device : ACPI_Service.Node_Descriptor := ACPI_Service.Values.No_Node;
      Policy : Policy_Report;
      Identity : Discovery.Queries.Identification_Report;
   end record;
   type Search_Error_Array is array (Search_Error_Index) of Search_Error;
   type Search_Report is record
      Outcome : Search_Outcome := Not_Ready;
      Scanned, Candidates, Excluded_Count, Charged : Natural := 0;
      Node_Errors : Natural := 0;
      -- Detached observations in scan order, including errors preceding a
      -- later successful candidate. No retained values or Core references.
      Captured_Errors : Search_Error_Count := 0;
      Omitted_Errors : Natural := 0;
      Errors : Search_Error_Array;
      Last_Policy : Policy_Report;
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

   -- The fixed owner callback receives this Session's exact private Core.
   -- It must preserve Core validity, use only Budget, retain any pending result
   -- itself, and report Recovery_Required until that obligation is settled.
   -- It must not reset/rebind the owner or recursively enter discovery.
   generic
      with procedure Check_Candidate
        (Service : aliased in out ACPI_Service.State;
         Item : ACPI_Service.Node_Descriptor; Budget : Natural;
         Result : out Policy_Report);
   procedure Find_With_Policy
     (Session : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Result : out Search_Report)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Result.Charged <= Budget;

   generic
      with procedure Check_Candidate
        (Service : aliased in out ACPI_Service.State;
         Item : ACPI_Service.Node_Descriptor; Budget : Natural;
         Result : out Policy_Report);
   procedure Check_With_Policy
     (Session : in out State; Item : ACPI_Service.Node_Descriptor;
      Budget : Natural; Result : out Policy_Report)
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
