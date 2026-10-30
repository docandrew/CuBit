-- Process-lifetime typed logger, serialized with the ACPI owner loop.
package ACPI_Native_Log is
   procedure Info (Text : String);
   procedure Warning (Text : String);
   procedure Critical (Text : String);
end ACPI_Native_Log;
