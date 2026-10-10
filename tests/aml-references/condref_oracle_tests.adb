with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Condref_Oracle_Tests is
   package NS is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type Integer_Value;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   OK : Boolean;
   L : Load_Status;
   R : Execution_Result;
   Prior : NS.State;
   Node : Node_ID;
   Bound_Status : Bind_Status;
   Checks : Natural := 0;
   Obj : constant Bytes := [79,66,74,48];
   Pkg : constant Bytes := [80,75,71,48];
   Names : constant array (1 .. 5) of Bytes (1 .. 4) :=
     [[77,84,72,48], [77,84,72,49], [68,69,86,48], [82,69,71,48], [70,76,68,48]];
   function Method_Data (Name : Bytes; Code : Bytes; Flags : Byte := 0) return Bytes is
     (Bytes'(16#14#, Byte (Code'Length + 6)) & Name & Bytes'(1 => Flags) & Code);
   procedure Check (B : Boolean) is
   begin Checks := Checks + 1; if not B then raise Program_Error with Checks'Image & " " & R.Status'Image; end if; end Check;
   procedure Prepare (Code : Bytes; W : Integer_Width) is
      Prefix : constant Bytes := Bytes'(1=>16#08#) & Obj & Bytes'(16#0A#,3,16#08#,71,76,79,66,0)
        & Method_Data (Names (1), [16#70#,1,71,76,79,66,16#A4#,1])
        & Method_Data (Names (2), [16#70#,1,71,76,79,66,16#A4#,16#68#], 1)
        & Bytes'(16#5B#,16#82#,5,68,69,86,48);
   begin
      Reset (A, OK); Check (OK);
      Load (A, Prefix & Method_Data ([84,69,83,84], Code), W, L); Check (L = Loaded);
      Bind_Table_Region (A, Root, "REG0", (1,1), Node, Bound_Status); Check (Bound_Status = Bound and Node = 7);
      Bind_Table_Field (A, Root, "FLD0", ((1,1),0,8), Node, Bound_Status); Check (Bound_Status = Bound and Node = 8);
      Load (A, Bytes'(1=>16#08#) & Pkg & Bytes'(16#12#,5,3,0,0,0), W, L); Check(L=Loaded);
   end Prepare;
   procedure Run (Code : Bytes; W : Integer_Width; Expected : Execution_Status;
                  Value : Integer_Value := 0; Budget : Natural := 100) is
   begin
      Prepare (Code,W); Prior := Snapshot (A);
      Invoke (A, Input, 6, [others => (Integer_Datum,0,Ordinary_Integer)],0,Budget,R);
      Check (R.Status = Expected);
      if Expected = Returned then Check (R.Value = Value and R.Origin = Ordinary_Integer); end if;
      Check (Cleanup_Frame (Snapshot (A),Prior));
      Check (R.Charged <= Budget);
   end Run;
begin
   Run ([164,91,18,77,84,72,48,0], Bits_32, Returned, 4294967295);
   Run ([91,18,77,84,72,48,96,164,142,96], Bits_32, Returned, 8);
   Run ([164,91,18,77,84,72,49,0], Bits_32, Returned, 4294967295);
   Run ([91,18,77,84,72,49,96,164,142,96], Bits_32, Returned, 8);
   Run ([164,91,18,68,69,86,48,0], Bits_32, Returned, 4294967295);
   Run ([91,18,68,69,86,48,96,164,142,96], Bits_32, Returned, 6);
   Run ([164,91,18,82,69,71,48,0], Bits_32, Returned, 4294967295);
   Run ([91,18,82,69,71,48,96,164,142,96], Bits_32, Returned, 10);
   Run ([164,91,18,70,76,68,48,0], Bits_32, Returned, 4294967295);
   Run ([91,18,70,76,68,48,96,164,142,96], Bits_32, Returned, 5);
   Run ([164,91,18,77,84,72,48,0], Bits_64, Returned, 18446744073709551615);
   Run ([91,18,77,84,72,48,96,164,142,96], Bits_64, Returned, 8);
   Run ([164,91,18,77,84,72,49,0], Bits_64, Returned, 18446744073709551615);
   Run ([91,18,77,84,72,49,96,164,142,96], Bits_64, Returned, 8);
   Run ([164,91,18,68,69,86,48,0], Bits_64, Returned, 18446744073709551615);
   Run ([91,18,68,69,86,48,96,164,142,96], Bits_64, Returned, 6);
   Run ([164,91,18,82,69,71,48,0], Bits_64, Returned, 18446744073709551615);
   Run ([91,18,82,69,71,48,96,164,142,96], Bits_64, Returned, 10);
   Run ([164,91,18,70,76,68,48,0], Bits_64, Returned, 18446744073709551615);
   Run ([91,18,70,76,68,48,96,164,142,96], Bits_64, Returned, 5);
   Ada.Text_IO.Put_Line("CondRefOf extracted oracle checks" & Checks'Image);
end Condref_Oracle_Tests;
