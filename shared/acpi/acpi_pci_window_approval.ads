pragma Ada_2022;
--  Launch approval for the PCI configuration broker (acpi-pci-windows):
--  only the one ACPI service (ACPI-ONE-001), started by the trusted plan
--  with both (acpi-hardware ...) and (acpi-pci-config ...) approved. The
--  identity is read from the executable manifest by the launcher, never an
--  RPC claim.
package ACPI_PCI_Window_Approval with Pure, SPARK_Mode is
   function May_Issue
     (System_Startup, Hardware_Approved, PCI_Config_Approved : Boolean;
      Validated_Manifest_Identity : String) return Boolean is
     (System_Startup and then Hardware_Approved and then PCI_Config_Approved
      and then Validated_Manifest_Identity = "com.cubit.acpi");
end ACPI_PCI_Window_Approval;
