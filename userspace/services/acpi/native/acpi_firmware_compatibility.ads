pragma Ada_2022;
with CCL.Setting_Codecs.ACPI_Firmware_Profile;
with AML_Interface_Policy;
-- Explicit service policy, separate from the reusable AML library default.
-- This package neither reads settings nor selects a live service profile.
package ACPI_Firmware_Compatibility with Pure, SPARK_Mode is
   package Declared renames CCL.Setting_Codecs.ACPI_Firmware_Profile;
   subtype Profile is Declared.ACPI_Profile_Value;
   use type Profile;
   Known_Windows_Interface_Count : constant Positive := 23;
   function Policy_For (Item : Profile) return AML_Interface_Policy.Policy
     with Post => AML_Interface_Policy.Count (Policy_For'Result) =
       (if Item = Declared.Neutral then 0 else Known_Windows_Interface_Count);
   function Name (Item : Profile) return String;
end ACPI_Firmware_Compatibility;
