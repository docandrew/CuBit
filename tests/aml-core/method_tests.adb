with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with Namespace_Instance;
procedure Method_Tests is
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.Object_Kind;
   use type NS.State;
   use type Integer_Value;
   Tree : NS.State := NS.Empty;
   Before : NS.State;
   Loaded : NS.Load_Status;
   R : Execution_Result;
   Args : constant Arguments := [others => 16#1234_5678_9ABC_DEF0#];
   Data : Bytes := [16#14#,8,16#54#,16#45#,16#53#,16#54#,1,16#A4#,16#68#];
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for W in Integer_Width loop
      Tree := NS.Empty;
      NS.Load_Names (Tree, Data, W, Loaded);
      Check (Loaded = NS.Loaded and then NS.Kind (Tree, 1) = NS.Method_Object);
      R := NS.Invoke (Tree, 1, Args, 1, 2);
      Check (R.Status = Returned and then R.Value =
        Args (0));
      R := NS.Invoke (Tree, 1, Args, 0, 2);
      Check (R.Status = Argument_Mismatch and then R.Charged = 0);
      R := NS.Invoke (Tree, 0, Args, 0, 2);
      Check (R.Status = Invalid_Method);
      R := NS.Invoke (Tree, 1, Args, 1, 1);
      Check (R.Status = Budget_Exceeded);
   end loop;
   for Flags in 0 .. 255 loop
      Data (7) := Byte (Flags);
      Tree := NS.Empty;
      NS.Load_Names (Tree, Data, Bits_64, Loaded);
      Check (Loaded = NS.Loaded);
      R := NS.Invoke (Tree, 1, Args, Flags mod 8, 10);
      Check ((if Flags mod 8 = 0 then R.Status = Missing_Argument
              else R.Status = Returned));
   end loop;
   Data (7) := 1;
   for Cut in 1 .. Data'Length - 1 loop
      Tree := NS.Empty;
      NS.Load_Names (Tree, Data (1 .. Cut), Bits_64, Loaded);
      Check (Loaded /= NS.Loaded and then Tree = NS.Empty);
   end loop;
   Tree := NS.Empty;
   NS.Load_Names (Tree, Data & [8,16#56#,16#41#,16#4C#,16#30#,1], Bits_64, Loaded);
   Check (Loaded = NS.Loaded and then NS.Kind (Tree, 2) = NS.Integer_Object);
   Data (9) := 0; --  Caller mutation cannot change the owned method body.
   R := NS.Invoke (Tree, 1, Args, 1, 10);
   Check (R.Status = Returned and then R.Value = Args (0));
   Before := Tree;
   NS.Load_Names (Tree, [16#14#,5,16#42#,16#41#,16#44#,16#30#], Bits_64, Loaded);
   Check (Loaded = NS.Bad_Method and then Tree = Before);
   Ada.Text_IO.Put_Line ("AML-METHOD-CHECK: PASS" & Checks'Image);
end Method_Tests;
