-- Hosted adapter only; does not validate native logstore/runtime delivery.
package ACPI_Native_Log is
   procedure Info (Text : String);
   procedure Warning (Text : String);
   function Records return Natural;
   function Warnings return Natural;
end ACPI_Native_Log;
