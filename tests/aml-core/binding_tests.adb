with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with Namespace_Instance;
procedure Binding_Tests is
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.State;
   use type Byte;
   use type Integer_Value;
   Checks : Natural := 0;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Result;
   end Enc;
   function Method_Data (Body_Data : Bytes) return Bytes is
     ([16#14#, Byte (6 + Body_Data'Length)] & Enc ("READ") & [0] & Body_Data);
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Case_Test
     (Body_Data : Bytes; Expected : Execution_Status; Value : Integer_Value := 0)
   is
      Contents : constant Bytes := [8] & Enc ("VAL0") & [16#0A#,44] & Method_Data (Body_Data);
      Data : constant Bytes := [8] & Enc ("VAL0") & [16#0A#,33] &
        [8] & Enc ("STR0") & [16#0D#,65,0] &
        [16#5B#,16#82#,Byte (5 + Contents'Length)] & Enc ("DEV0") & Contents;
      Tree : NS.State;
      Before : NS.State;
      Loaded : NS.Load_Status;
      Node : NS.Node_ID;
      R : Execution_Result;
   begin
      for W in Integer_Width loop
         Tree := NS.Empty;
         NS.Load_Names (Tree, Data, W, Loaded);
         Check (Loaded = NS.Loaded);
         Node := NS.Child (Tree, NS.Child (Tree, NS.Root, "DEV0"), "READ");
         Before := Tree;
         R := NS.Invoke (Tree, Node, [others => 0], 0, 30);
         Check (R.Status = Expected and then
                (if Expected = Returned then R.Value = Value));
         Check (Tree = Before);
         R := NS.Invoke (Tree, Node, [others => 0], 0, 1);
         Check (R.Status = Budget_Exceeded);
      end loop;
   end Case_Test;
begin
   Case_Test ([16#A4#] & Enc ("VAL0"), Returned, 44);
   Case_Test ([16#A4#,16#5E#] & Enc ("VAL0"), Returned, 44);
   Case_Test ([16#A4#,16#5E#,16#5E#] & Enc ("VAL0"), Returned, 33);
   Case_Test ([16#A4#,16#5C#] & Enc ("VAL0"), Returned, 33);
   Case_Test ([16#A4#,16#5C#,16#2E#] & Enc ("DEV0VAL0"), Returned, 44);
   Case_Test ([16#A4#,16#5C#,16#2F#,2] & Enc ("DEV0VAL0"), Returned, 44);
   Case_Test ([16#A4#,16#72#] & Enc ("VAL0") & [1,0], Returned, 45);
   Case_Test ([16#70#] & Enc ("VAL0") & [16#60#,16#A4#,16#60#], Returned, 44);
   Case_Test ([16#A4#,16#2E#] & Enc ("DEV0VAL0"), Unknown_Name);
   Case_Test ([16#A4#] & Enc ("MISS"), Unknown_Name);
   Case_Test ([16#A4#] & Enc ("STR0"), Object_Returned);
   Case_Test ([16#A4#] & Enc ("READ"), Budget_Exceeded);
   Case_Test ([16#A4#,16#5E#,16#5E#,16#5E#] & Enc ("VAL0"), Unknown_Name);
   Case_Test ([16#A4#] & Enc ("V"), Truncated);
   Case_Test ([16#A4#] & Enc ("VaL0"), Bad_Name);
   Ada.Text_IO.Put_Line ("AML-BINDING-CHECK: PASS" & Checks'Image);
end Binding_Tests;
