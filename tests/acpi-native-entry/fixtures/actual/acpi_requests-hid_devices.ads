pragma Ada_2022;
with ACPI_HID_I2C_Discovery;
with ACPI_HID_I2C_Resolved;
with ACPI_Bootstrap.HID_Devices;
generic
   with package Discovery is new ACPI_HID_I2C_Discovery
     (Core => ACPI_Service, others => <>);
   with package Resolver is new ACPI_HID_I2C_Resolved
     (Core => ACPI_Service, others => <>);
package ACPI_Requests.HID_Devices with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Controller is new ACPI_Bootstrap.HID_Devices (Discovery, Resolver);
   -- Trusted owner enumeration, same pending-session exclusion as Controller.
   procedure Find
     (Server : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Result : out Controller.Search_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old
       and then Result.Charged <= Budget;

   function Current (Run : Controller.Probe; Server : State) return Boolean
     with Pre => Valid (Server);
   -- Internal fixed owner path to this Server's SAME private Core. No wire
   -- execution endpoint or hardware authority. Observe the Controller's
   -- serialization and pending-session exclusion requirements.
   procedure Begin_Probe
     (Run : in out Controller.Probe; Server : in out State;
      Device : ACPI_Service.Namespace.Node_ID; Budget : Natural;
      Allowed : out Controller.Admission; Action : out Discovery.Action_Result)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
   procedure Copy_And_Resolve
     (Run : in out Controller.Probe; Server : in out State;
      Data : out Firmware_Tables.Bytes; Allowed : out Controller.Admission;
      Action : out Discovery.Action_Result)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
   procedure Cancel
     (Run : in out Controller.Probe; Server : in out State;
      Action : out Discovery.Action_Result)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
   -- Serialized trusted-owner selection and probe start. Search and all later
   -- probe work share Budget: Begin_Probe receives only Budget - Search.Charged.
   -- While this Run is Pending, return Busy without searching or replacing it.
   -- Other pending probes still require owner-wide exclusion. A Found search
   -- is not discovery success; inspect Action and Observe (Run). Copy retries
   -- retain the same _CRS via Copy_And_Resolve, never rerun this operation for size.
   procedure Begin_Next_Probe
     (Run : in out Controller.Probe; Server : in out State;
      After : ACPI_Service.Namespace.Node_ID; Budget : Natural;
      Device : out ACPI_Service.Namespace.Node_ID; Search : out Controller.Search_Report;
      Allowed : out Controller.Admission; Action : out Discovery.Action_Result)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old
       and then Search.Charged <= Budget;
end ACPI_Requests.HID_Devices;
