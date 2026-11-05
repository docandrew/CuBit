pragma Ada_2022;
package body ACPI_Requests.HID_Devices with SPARK_Mode is
   procedure Find
     (Server : in out State; After : ACPI_Service.Namespace.Node_ID;
      Budget : Natural; Device : out ACPI_Service.Namespace.Node_ID;
      Result : out Controller.Search_Report) is
   begin Controller.Find (Server.Boot, After, Budget, Device, Result); end Find;
   function Current (Run : Controller.Probe; Server : State) return Boolean is
     (Controller.Current (Run, Server.Boot));
   procedure Begin_Probe
     (Run : in out Controller.Probe; Server : in out State;
      Device : ACPI_Service.Namespace.Node_ID; Budget : Natural;
      Allowed : out Controller.Admission; Action : out Discovery.Action_Result) is
   begin Controller.Begin_Probe (Run, Server.Boot, Device, Budget, Allowed, Action); end Begin_Probe;
   procedure Begin_Next_Probe
     (Run : in out Controller.Probe; Server : in out State;
      After : ACPI_Service.Namespace.Node_ID; Budget : Natural;
      Device : out ACPI_Service.Namespace.Node_ID; Search : out Controller.Search_Report;
      Allowed : out Controller.Admission; Action : out Discovery.Action_Result) is
   begin
      Controller.Begin_Next_Probe
        (Run, Server.Boot, After, Budget, Device, Search, Allowed, Action);
   end Begin_Next_Probe;
   procedure Copy_And_Resolve
     (Run : in out Controller.Probe; Server : in out State;
      Data : out Firmware_Tables.Bytes; Allowed : out Controller.Admission;
      Action : out Discovery.Action_Result) is
   begin Controller.Copy_And_Resolve (Run, Server.Boot, Data, Allowed, Action); end Copy_And_Resolve;
   procedure Cancel
     (Run : in out Controller.Probe; Server : in out State;
      Action : out Discovery.Action_Result) is
   begin Controller.Cancel (Run, Server.Boot, Action); end Cancel;
end ACPI_Requests.HID_Devices;
