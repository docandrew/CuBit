with Owner_Test_Control;
package body ACPI_Native_Log is
   procedure Info (Text : String) is
   begin Owner_Test_Control.Write (Owner_Test_Control.Information, Text); end Info;
   procedure Warning (Text : String) is
   begin Owner_Test_Control.Write (Owner_Test_Control.Warning, Text); end Warning;
   procedure Critical (Text : String) is
   begin Owner_Test_Control.Write (Owner_Test_Control.Critical, Text); end Critical;
end ACPI_Native_Log;
