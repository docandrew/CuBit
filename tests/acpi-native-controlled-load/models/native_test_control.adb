with Ada.Command_Line;
package body Native_Test_Control is
   function Mode return String is (Ada.Command_Line.Argument (1));
end Native_Test_Control;
