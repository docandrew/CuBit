with Ada.Text_IO;
with AML_Decode;
with AML_Names;
with Namespace_Instance;
procedure Resolve_Tests is
   package NS renames Namespace_Instance;
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
   procedure Add (Scope : Node_ID; Part : String) is
   begin
      Insert (Tree, Scope, Part, Node, Result);
      Check (Result = Inserted);
   end Add;
   procedure Lookup
     (Scope : Node_ID; Data : AML_Decode.Bytes;
      Status : Lookup_Status; Expected : Node_ID := 0)
   is
      R : constant Lookup_Result := Resolve (Tree, Scope, AML_Names.Read_Name (Data));
   begin
      Check (R.Status = Status and then
             (if Status = Found then R.Node = Expected));
   end Lookup;
begin
   Add (0, "AAAA"); -- 1
   Add (1, "BBBB"); -- 2
   Add (0, "TEST"); -- 3
   Add (1, "TEST"); -- 4: nearest ancestor shadows root
   Add (3, "LEAF"); -- 5
   Lookup (2, [16#54#,16#45#,16#53#,16#54#], Found, 4);
   Lookup (2, [16#5C#,16#54#,16#45#,16#53#,16#54#], Found, 3);
   Lookup (2, [16#5E#,16#54#,16#45#,16#53#,16#54#], Found, 4);
   Lookup (2, [16#5E#,16#5E#,16#54#,16#45#,16#53#,16#54#], Found, 3);
   Lookup (2, [16#5E#,16#5E#,16#5E#,0], Above_Root);
   Lookup (2, [16#2E#,16#54#,16#45#,16#53#,16#54#,
                        16#4C#,16#45#,16#41#,16#46#], Not_Found);
   Lookup (0, [16#2E#,16#54#,16#45#,16#53#,16#54#,
                        16#4C#,16#45#,16#41#,16#46#], Found, 5);
   Lookup (2, [0], Found, 2);
   Lookup (2, [16#5C#,0], Found, 0);
   Lookup (2, [16#5E#,0], Found, 1);
   Lookup (2, [16#2F#,0], Invalid_Path);
   Add (2, "TEST"); -- 6 shadows both ancestors
   Lookup (2, [16#54#,16#45#,16#53#,16#54#], Found, 6);
   --  Explicit parent never searches farther upwards.
   Lookup (2, [16#5E#,16#4C#,16#45#,16#41#,16#46#], Not_Found);
   Add (0, "ONLY"); -- 7
   for I in 8 .. 128 loop
      Add (I - 1, "DEEP");
      Lookup (I, [16#4F#,16#4E#,16#4C#,16#59#], Found, 7);
      Lookup (I, [16#4D#,16#49#,16#53#,16#53#], Not_Found);
   end loop;
   Ada.Text_IO.Put_Line ("AML-RESOLVE-CHECK: PASS" & Checks'Image);
end Resolve_Tests;
