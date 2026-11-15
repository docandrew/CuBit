pragma Ada_2022;
with ACPI_PCI_Observation_Reader;
with ACPI_Bootstrap.PCI_Observations;
generic
   with package Reader is new ACPI_PCI_Observation_Reader
     (Core => ACPI_Service, others => <>);
package ACPI_Requests.PCI_Observations with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Controller is new ACPI_Bootstrap.PCI_Observations (Reader);
   -- Local owner operation, absent from Handle's public wire protocol.
   -- Revision/receipts stay intact; the selected AML method can have effects.
   procedure Read (Server : in out State;
      Device : ACPI_Service.Node_Descriptor; Selected : Reader.Member;
      Budget : Natural; Allowed : out Controller.Admission; Result : out Reader.Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
   procedure Identify_Root (Server : in out State;
      Device : ACPI_Service.Node_Descriptor; Budget : Natural;
      Allowed : out Controller.Admission; Result : out Reader.Root_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
   procedure Observe_Controller (Server : in out State;
      Device : ACPI_Service.Node_Descriptor; Budget : Natural;
      Allowed : out Controller.Admission; Result : out Reader.Topology_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old;
end ACPI_Requests.PCI_Observations;
