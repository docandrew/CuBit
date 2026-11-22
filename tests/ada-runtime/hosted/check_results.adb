--  Linux body of Check_Results: the same lines, on standard output.
with Ada.Text_IO;

package body Check_Results is

   Prefix : constant String := "ada-runtime-check: ";

   Failure_Count : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Ada.Text_IO.Put_Line (Prefix & Name & " PASS");
      else
         Failure_Count := Failure_Count + 1;
         Ada.Text_IO.Put_Line (Prefix & Name & " FAIL");
      end if;
   end Check;

   procedure Note (Text : String) is
   begin
      Ada.Text_IO.Put_Line (Prefix & Text);
   end Note;

   function Failures return Natural is (Failure_Count);

end Check_Results;
