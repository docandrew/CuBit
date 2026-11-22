pragma Ada_2022;
-- Copied diagnostic text only. Use the established publisher ring even while
-- hardware is uncertain; channel opening is only permitted at a quiet boundary.
package ACPI_Native_Diagnostic_Output is
   procedure Write (Text : String; Peer_Quiet : Boolean; Success : Boolean := False);
end ACPI_Native_Diagnostic_Output;
