with Ada.Text_IO; use Ada.Text_IO;
with AML_Namespace;
procedure Namespace_Tests is
   package NS is new AML_Namespace (Capacity => 128);
   use NS;
   Tree : State := Empty;
   Node : Node_ID;
   Result : Insert_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then
         raise Program_Error with Checks'Image;
      end if;
   end Check;
begin
   Check (NS.Count (Tree) = 0 and then Parent (Tree, Root) = Root);
   Check (Child (Tree, Root, "_SB_") = Root);
   Insert (Tree, Root, "_SB_", Node, Result);
   Check (Result = Inserted and then Node = 1);
   Insert (Tree, Root, "_SB_", Node, Result);
   Check (Result = Duplicate and then Node = Root and then NS.Count (Tree) = 1);
   Insert (Tree, 1, "_SB_", Node, Result);
   Check (Result = Inserted and then Node = 2);
   Check (Child (Tree, Root, "_SB_") = 1 and then Child (Tree, 1, "_SB_") = 2);
   Insert (Tree, Root, "0BAD", Node, Result);
   Check (Result = Invalid_Name and then NS.Count (Tree) = 2);
   for I in 3 .. 128 loop
      Insert (Tree, I - 1, "NODE", Node, Result);
      Check (Result = Inserted and then Node = I and then Parent (Tree, Node) = I - 1);
      for J in 3 .. I loop
         Check (Child (Tree, J - 1, "NODE") = J);
      end loop;
   end loop;
   Insert (Tree, Root, "FULL", Node, Result);
   Check (Result = Full and then Node = Root and then NS.Count (Tree) = 128);
   Insert (Tree, Root, "_SB_", Node, Result);
   Check (Result = Duplicate and then Node = Root);
   Put_Line ("AML-NAMESPACE-CHECK: PASS" & Checks'Image);
end Namespace_Tests;
