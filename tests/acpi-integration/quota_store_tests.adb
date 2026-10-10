with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with AML_Table_Backing;
with Test_Namespace; use Test_Namespace;
procedure Quota_Store_Tests is
   use Owned;
   use type AML_Objects.Allocation_Status;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   OK : Boolean;
   Loaded_Status : Load_Status;
   Allocation : AML_Objects.Allocation_Status;
   ID : AML_Objects.Object_ID;
   R : Execution_Result;
   Prior : State;
   Args : constant Value_Arguments := [others => (Integer_Datum, 42, AML_Decode.Ordinary_Integer)];
   Checks : Natural := 0;
   Names : constant Bytes :=
     [16#08#,65,65,65,65,16#0D#,97,98,99,0,
      16#08#,66,66,66,66,16#0D#,120,121,0];
   procedure Check (C : Boolean) is
   begin Checks := Checks + 1; if not C then raise Program_Error with Checks'Image; end if; end Check;
   procedure Run_Case (W : Integer_Width; Destination : Bytes) is
      -- Named source evaluation allocates nothing. Slot initialization is scalar.
      Body_Code : constant Bytes :=
        (if Destination'Length = 1 then [16#70#,16#0A#,42,Destination (Destination'First)] else Bytes'(1 .. 0 => 0))
        & Bytes'(16#70#,65,65,65,65) & Destination & Bytes'(16#A4#,0);
      Method_Code : constant Bytes :=
        Bytes'(16#14#, Byte (6 + Body_Code'Length),84,69,83,84,7) & Body_Code;
   begin
      Reset (A, OK); Check (OK);
      Load (A, Names & Method_Code, W, Loaded_Status); Check (Loaded_Status = Loaded);
      Append (A, [1 .. AML_Objects.Max_Bytes - Values_Used (A).Bytes => 0], ID, Allocation);
      Check (Allocation = AML_Objects.Allocated);
      Prior := Snapshot (A);
      Invoke (A, Input, 3, Args, 7, 100, R);
      Check (R.Status = Value_Limit);
      -- Only this fixture's allocation-free earlier expressions permit exact equality.
      Check (Snapshot (A) = Prior);
      Check (String_Data (Snapshot (A), 1) = "abc");
      Check (String_Data (Snapshot (A), 2) = "xy");
      Check (Args = Value_Arguments'(others => (Integer_Datum, 42, AML_Decode.Ordinary_Integer)));
      -- Locals and callee argument replacements are invocation-private and cannot
      -- be observed after failure; checked Write_Target contracts cover their frame.
   end Run_Case;
begin
   for W in Integer_Width loop
      Run_Case (W, [66,66,66,66]);
      for Slot in Byte range 16#60# .. 16#6E# loop
         Run_Case (W, [Slot]);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("QUOTA-STORE PASS" & Checks'Image);
end Quota_Store_Tests;
