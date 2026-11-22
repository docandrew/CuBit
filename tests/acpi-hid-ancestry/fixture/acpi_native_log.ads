-- Hosted log boundary only; no channel or hardware.
package ACPI_Native_Log is
 procedure Info (Text : String);
 procedure Warning (Text : String);
 function Error_Rows return Natural;
 function Error_Summary return String;
 function Exception_Reports return Natural;
 function Compact_Result_Length return Natural;
end ACPI_Native_Log;
