pragma Ada_2022;
with ACPI_Lid;
generic
   with package Driver is new ACPI_Lid (Core => ACPI_Service, others => <>);
package ACPI_Bootstrap.Lids with SPARK_Mode is
   type Binding is private;
   No_Binding : constant Binding;
   type Discovery_Outcome is (Not_Ready, Found, No_Match, Incomplete, Failed);
   type Discovery_Report is record
      Outcome : Discovery_Outcome := Not_Ready;
      Scanned, Candidates, Errors, Charged : Natural := 0;
      Last_Identity : Driver.Identification_Report;
   end record;
   -- Trusted controller API. Searches for the first matching lid only, after
   -- successful device initialization. It never exposes the Core arena or
   -- accepts a wire-supplied namespace node. Find may evaluate firmware _HID.
   -- Other lids and device-status policy remain separate enumeration work.
   procedure Find (Session : in out State; Budget : Natural;
      Item : out Binding; Report : out Discovery_Report)
     with Pre => Valid (Session), Post => Valid (Session)
       and then Report.Charged <= Budget;
   type Query_Admission is (Admitted, Not_Ready, Invalid_Binding);
   procedure Query (Session : in out State; Item : Binding; Budget : Natural;
      Admission : out Query_Admission; Report : out Driver.Query_Report)
     with Pre => Valid (Session), Post => Valid (Session);
private
   type Binding is record
      Device : ACPI_Service.Node_Descriptor := ACPI_Service.Values.No_Node;
   end record;
   No_Binding : constant Binding := (others => <>);
end ACPI_Bootstrap.Lids;
