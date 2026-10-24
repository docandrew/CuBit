with Ada.Text_IO; use Ada.Text_IO;
package body Test_Support is
   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failed := Failed + 1;
         Put_Line ("FAIL " & Name);
      end if;
   end Check;
end Test_Support;
