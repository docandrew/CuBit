with Ada.Text_IO;
package body ACPI_Native_Log is
   Record_Count, Warning_Count : Natural := 0;
   procedure Info (Text : String) is
   begin
      Record_Count := Record_Count + 1;
      Ada.Text_IO.Put_Line (Text);
   end Info;
   procedure Warning (Text : String) is
   begin
      Record_Count := Record_Count + 1;
      Warning_Count := Warning_Count + 1;
      Ada.Text_IO.Put_Line (Text);
   end Warning;
   function Records return Natural is (Record_Count);
   function Warnings return Natural is (Warning_Count);
end ACPI_Native_Log;
