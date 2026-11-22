with CuBit.Handles.Testing;with CuBit.Handles;with CuBit.Handles.Startup;
with CuBit.ACPI_Windows;with CuBit.Firmware_Snapshot;
-- Hosted launch boundary. No real authority or FADT admission is established.
package Startup is
 function Hardware return CuBit.ACPI_Windows.Grant;
 function Tables return CuBit.Firmware_Snapshot.Authority is (CuBit.Firmware_Snapshot.Held (CuBit.Handles.Testing.Snapshot));
 function Hardware_Data return CuBit.Handles.Startup.Optional_Endpoint is ((Present=>False));
 function Pci_Windows return CuBit.Handles.Startup.Optional_Endpoint is ((Present=>False));
 function Registers return CuBit.Handles.Startup.Optional_Endpoint is ((Present=>False));
 function Registration return CuBit.Handles.Notification is (CuBit.Handles.No_Notification);
end Startup;
