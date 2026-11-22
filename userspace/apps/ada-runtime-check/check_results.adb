pragma Ada_2022;
with CuBit.Messages;

package body Check_Results is

   Prefix : constant String := "ada-runtime-check: ";

   Failure_Count : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         CuBit.Messages.debugPrint (Prefix & Name & " PASS" & ASCII.LF);
      else
         Failure_Count := Failure_Count + 1;
         CuBit.Messages.debugPrint (Prefix & Name & " FAIL" & ASCII.LF);
      end if;
   end Check;

   procedure Note (Text : String) is
   begin
      CuBit.Messages.debugPrint (Prefix & Text & ASCII.LF);
   end Note;

   function Failures return Natural is (Failure_Count);

end Check_Results;
