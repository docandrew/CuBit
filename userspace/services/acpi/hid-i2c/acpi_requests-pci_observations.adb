pragma Ada_2022;
package body ACPI_Requests.PCI_Observations with SPARK_Mode is
   procedure Read (Server : in out State;
      Device : ACPI_Service.Node_Descriptor; Selected : Reader.Member;
      Budget : Natural; Allowed : out Controller.Admission; Result : out Reader.Report) is
   begin
      Controller.Read (Server.Boot, Device, Selected, Budget, Allowed, Result);
   end Read;
   procedure Identify_Root (Server : in out State;
      Device : ACPI_Service.Node_Descriptor; Budget : Natural;
      Allowed : out Controller.Admission; Result : out Reader.Root_Report) is
   begin
      Controller.Identify_Root (Server.Boot, Device, Budget, Allowed, Result);
   end Identify_Root;
   procedure Observe_Controller (Server : in out State;
      Device : ACPI_Service.Node_Descriptor; Budget : Natural;
      Allowed : out Controller.Admission; Result : out Reader.Topology_Report) is
   begin
      Controller.Observe_Controller (Server.Boot, Device, Budget, Allowed, Result);
   end Observe_Controller;
end ACPI_Requests.PCI_Observations;
