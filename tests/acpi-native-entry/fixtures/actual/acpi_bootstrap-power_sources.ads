pragma Ada_2022;
with ACPI_Power_Source;
generic
   with package Driver is new ACPI_Power_Source (Core => ACPI_Service, others => <>);
package ACPI_Bootstrap.Power_Sources with SPARK_Mode is
   type Binding is private;
   No_Binding : constant Binding;
   type Discovery_Outcome is (Not_Ready, Found, No_Match, Incomplete, Failed);
   type Discovery_Report is record
      Outcome : Discovery_Outcome := Not_Ready;
      Scanned, Candidates, Errors, Charged : Natural := 0;
      Last_Identity : Driver.Identification_Report;
   end record;
   -- Trusted controller API. Searches for the first matching power source only, after
   -- successful device initialization. It never exposes the Core arena or
   -- accepts a wire-supplied namespace node. Find may evaluate firmware _HID.
   -- Other power sources and device-status policy remain separate enumeration work.
   procedure Find (Session : in out State; Budget : Natural;
      Item : out Binding; Report : out Discovery_Report)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Report.Charged <= Budget;
   type Query_Admission is (Admitted, Not_Ready, Invalid_Binding);
   procedure Query (Session : in out State; Item : Binding; Budget : Natural;
      Admission : out Query_Admission; Report : out Driver.Query_Report)
     with Pre => Valid (Session), Post => Valid (Session);
   type Sampling_Outcome is (Not_Attempted, Not_Ready, No_Match, Search_Failed,
      Budget_Exhausted, Status_Failed, Device_Unavailable, Query_Failed,
      Sampled, Provider_Contract_Error);
   type Sampling_Report is record
      Outcome : Sampling_Outcome := Not_Attempted;
      Search : Discovery_Report;
      Device_Status : Driver.Device_Status.Status_Report;
      Query : Driver.Query_Report;
      Charged : Natural := 0;
   end record;
   -- Fixed first-match startup observation, not a live/fresh cache or authority.
   -- Search, direct _STA and member query share one budget. Status failure or
   -- absent/disabled/nonfunctioning device prevents the member query. Returned
   -- objects are cleaned by the fixed evaluators; cleanup failures are retained
   -- in the reports and must stop subsequent owner AML/reset. No automatic retry.
   procedure Sample_Ready (Session : in out State; Budget : Natural;
      Report : out Sampling_Report)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Report.Charged <= Budget;
private
   type Binding is record
      Device : ACPI_Service.Node_Descriptor := ACPI_Service.Values.No_Node;
   end record;
   No_Binding : constant Binding := (others => <>);
end ACPI_Bootstrap.Power_Sources;
