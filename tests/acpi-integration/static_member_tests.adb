with AML_Delays;
with Ada.Text_IO;
with AML_Namespace;
with AML_Decode; use AML_Decode;
with AML_Objects;
with AML_Objects.Package_References;
with AML_Execute; use AML_Execute;
with AML_Table_Backing;
procedure Static_Member_Tests is
   package NS is new AML_Namespace (32, AML_Delays.Unavailable_Provider, Max_Pending_Members => 4, Max_Pending_Segments => 4);
   use NS; use NS.Owned;
   use type AML_Objects.Package_References.Result_Status;
   use type Integer_Value;
   A : Arena;
   Before : State;
   OK : Boolean;
   Loaded_Result : Load_Status;
   Report : Initialization_Report;
   Ref : Reference;
   Ref_Status : AML_Objects.Package_References.Result_Status;
   ID : AML_Objects.Object_ID;
   R : Execution_Result;
   Input : aliased AML_Table_Backing.State (1, 1);
   Checks : Natural := 0;
   -- Name(PKG0,Package(2){SRC0,SRC0}); Method(TEST,0){Return(1)}
   Pkg : constant Bytes := [16#08#,16#50#,16#4B#,16#47#,16#30#,16#12#,10,2,
     16#53#,16#52#,16#43#,16#30#,16#53#,16#52#,16#43#,16#30#];
   Source : constant Bytes := [16#08#,16#53#,16#52#,16#43#,16#30#,16#0A#,7];
   Method : constant Bytes := [16#14#,8,16#54#,16#45#,16#53#,16#54#,0,16#A4#,1];
   procedure Check (C : Boolean) is
   begin Checks := Checks + 1; if not C then raise Program_Error with Checks'Image; end if; end Check;
   procedure Member_Is (Expected : AML_Objects.Object_ID) is
   begin
      for I in 0 .. 1 loop
         Make_Element (A, Data_Object (Snapshot (A), 1), Integer_Value (I), Ref, Ref_Status);
         Check (Ref_Status = AML_Objects.Package_References.Ready);
         Read_Element (A, Ref, ID, OK); Check (OK and then ID = Expected);
      end loop;
   end Member_Is;
begin
   Reset (A, OK); Check (OK);
   Load (A, Pkg & Method, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Check (Pending_Members (Snapshot (A)) = 2);
   Invoke (A, Input, 2, [others => (Integer_Datum, 0, AML_Decode.Ordinary_Integer)], 0, 20, R);
   Check (R.Status = Uninitialized and then R.Charged = 0);
   Member_Is (0);
   Before := Snapshot (A);
   R := NS.Invoke (Before, 2, [others => 0], 0, 20);
   Check (R.Status = Uninitialized and then R.Charged = 0);
   NS.Invoke_Mutable (Before, 2, [others => 0], 0, 20, R);
   Check (R.Status = Uninitialized and then Before = Snapshot (A));
   NS.Invoke_With_Tables (Before, Input, 2, [others => 0], 0, 20, R);
   Check (R.Status = Uninitialized and then Before = Snapshot (A));
   Load (A, Source & [16#FF#], Bits_64, Loaded_Result);
   Check (Loaded_Result /= Loaded and then Snapshot (A) = Before);
   Load (A, Source, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Check (Pending_Members (Snapshot (A)) = 2); Member_Is (0);
   Initialize_Members (A, Report);
   Check (Report = (2,0,0)); Check (Pending_Members (Snapshot (A)) = 0);
   Member_Is (Data_Object (Snapshot (A), 3));
   Invoke (A, Input, 2, [others => (Integer_Datum, 0, AML_Decode.Ordinary_Integer)], 0, 20, R);
   Check (R.Status = Returned and then R.Value = 1);
   Before := Snapshot (A); Initialize_Members (A, Report);
   Check (Report = (0,0,0) and then Snapshot (A) = Before);
   Reset (A, OK); Check (OK);
   Load (A, Pkg, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Initialize_Members (A, Report); Check (Report = (0,2,0)); Member_Is (0);
   Load (A, Source, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Initialize_Members (A, Report); Check (Report = (0,0,0)); Member_Is (0);
   Reset (A, OK); Check (OK);
   -- Unsupported named method is distinct from missing.
   declare M : Bytes := Method; P : constant Bytes := Pkg; begin
      M (3 .. 6) := [16#53#,16#52#,16#43#,16#30#];
      Load (A, P & M, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   end;
   Initialize_Members (A, Report); Check (Report = (0,0,2)); Member_Is (0);
   Reset (A, OK); Check (OK);
   Load (A, Pkg, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Before := Snapshot (A);
   declare P : Bytes := Pkg; begin
      P (5) := 16#31#;
      -- Three references exceed remaining two journal slots.
      P (7) := 14; P (8) := 3;
      Load (A, P & [16#53#,16#52#,16#43#,16#30#], Bits_64, Loaded_Result);
      Check (Loaded_Result = Value_Limit and then Snapshot (A) = Before);
   end;
   Ada.Text_IO.Put_Line ("STATIC-MEMBERS: PASS" & Checks'Image);
end Static_Member_Tests;
