with Ada.Strings.Unbounded;
package body ACPI_Native_Log is
 use Ada.Strings.Unbounded;
 Rows : Natural := 0;
 Summary : Unbounded_String;
 Exceptions, Result_Length : Natural := 0;
 procedure Info (Text : String) is pragma Unreferenced (Text); begin null; end Info;
 procedure Warning (Text : String) is
   Row_Prefix : constant String := "acpi: stage hid-node fail ";
   Summary_Prefix : constant String := "acpi: stage hid-node-summary ";
   Result_Prefix : constant String := "acpi: stage hid-discovery-result fail ";
   Exception_Prefix : constant String := Result_Prefix & "OWNER_EXCEPTION";
 begin
   if Text'Length >= Row_Prefix'Length
     and then Text (Text'First .. Text'First + Row_Prefix'Length - 1) = Row_Prefix then
      Rows := Rows + 1;
   elsif Text'Length >= Summary_Prefix'Length
     and then Text (Text'First .. Text'First + Summary_Prefix'Length - 1) = Summary_Prefix then
      Summary := To_Unbounded_String (Text);
   end if;
   if Text'Length >= Result_Prefix'Length
     and then Text (Text'First .. Text'First + Result_Prefix'Length - 1) = Result_Prefix then
      Result_Length := Text'Length;
   end if;
   if Text'Length >= Exception_Prefix'Length
     and then Text (Text'First .. Text'First + Exception_Prefix'Length - 1) = Exception_Prefix then
      Exceptions := Exceptions + 1;
   end if;
 end Warning;
 function Error_Rows return Natural is (Rows);
 function Error_Summary return String is (To_String (Summary));
 function Exception_Reports return Natural is (Exceptions);
 function Compact_Result_Length return Natural is (Result_Length);
end ACPI_Native_Log;
