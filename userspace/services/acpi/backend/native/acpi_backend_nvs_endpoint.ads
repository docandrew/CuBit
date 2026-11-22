pragma Ada_2022;
with CuBit.Messages;
with ACPI_Backend_NVS_Records;
with ACPI_Backend_NVS_Dispatch;
generic
   with package Records is new ACPI_Backend_NVS_Records (<>);
package ACPI_Backend_NVS_Endpoint is
   -- Authenticated authorityTag comes from the kernel receive adapter.
   -- The payload contains only the NVS protocol's record/offset/width data.
   procedure Dispatch
     (State : in out Records.Context; Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
private
   package Driver is new ACPI_Backend_NVS_Dispatch (Records);
end ACPI_Backend_NVS_Endpoint;
