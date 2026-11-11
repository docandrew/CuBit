pragma Ada_2022;
with ACPI_Firmware_Compatibility; with AML_Interface_Policy; with AML_Decode;
-- Select once from the generated immutable launch parameters.
-- No Config RPC or live policy reload.
package ACPI_Native_Firmware_Profile with SPARK_Mode => Off is
   procedure Log_Selected;
   procedure Observe_Query (Name : AML_Decode.Bytes; Supported, Truncated : Boolean);
   function Selected return ACPI_Firmware_Compatibility.Profile;
   function Interfaces return AML_Interface_Policy.Policy;
end ACPI_Native_Firmware_Profile;
