-- Explicit hosted launch-parameter boundary, fixed Neutral profile.
package Parameters is
 type ACPI_Profile_Value is (Neutral,Known_Windows);
 type ACPI_Firmware_Profile_Value is record kind:ACPI_Profile_Value:=Neutral;end record;
 function Value return ACPI_Firmware_Profile_Value is ((kind=>Neutral));
end Parameters;
