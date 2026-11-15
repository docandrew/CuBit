pragma Ada_2022;
with ACPI_PCI_Observation_Reader;
generic
   with package Reader is new ACPI_PCI_Observation_Reader
     (Core => ACPI_Service, others => <>);
package ACPI_Bootstrap.PCI_Observations with SPARK_Mode is
   type Admission is (Not_Ready, Owner_Busy, Admitted);
   -- Fixed trusted-owner path to this Bootstrap's SAME Core. No new wire
   -- selector or evaluation endpoint. Complete device initialization required;
   -- any retained result excludes more AML. The owner additionally excludes
   -- all other pending/recovery/provider work, including between local calls.
   procedure Read (Session : in out State;
      Device : ACPI_Service.Node_Descriptor; Selected : Reader.Member;
      Budget : Natural; Allowed : out Admission; Result : out Reader.Report)
     with Pre => Valid (Session), Post => Valid (Session);
   procedure Identify_Root (Session : in out State;
      Device : ACPI_Service.Node_Descriptor; Budget : Natural;
      Allowed : out Admission; Result : out Reader.Root_Report)
     with Pre => Valid (Session), Post => Valid (Session);
   procedure Observe_Controller (Session : in out State;
      Device : ACPI_Service.Node_Descriptor; Budget : Natural;
      Allowed : out Admission; Result : out Reader.Topology_Report)
     with Pre => Valid (Session), Post => Valid (Session);
end ACPI_Bootstrap.PCI_Observations;
