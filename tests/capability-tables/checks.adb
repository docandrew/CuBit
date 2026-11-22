with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body Checks is
   Failed_N : Natural := 0;
   Passed_N : Natural := 0;
   Current  : access String := null;
   Max_Reported : constant := 50;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Passed_N := Passed_N + 1;
      else
         Failed_N := Failed_N + 1;
         if Failed_N <= Max_Reported then
            Put_Line ("FAIL [" & (if Current = null then "" else Current.all)
                      & "] " & What);
         end if;
      end if;
   end Check;

   procedure Section (Name : String) is
   begin
      Current := new String'(Name);
      Put_Line ("-- " & Name);
   end Section;

   function Failures return Natural is (Failed_N);
   function Passed return Natural is (Passed_N);

   procedure Finish is
   begin
      Put_Line ("checks passed:" & Passed_N'Image & ", failed:" & Failed_N'Image);
      if Failed_N = 0 then
         Put_Line ("PASS");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
      else
         Put_Line ("FAILED");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Finish;
end Checks;
