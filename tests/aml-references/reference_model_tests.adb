with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with AML_References;
with AML_Frame_Handles;
procedure Reference_Model_Tests is
   use type AML_References.Reference;
   use type Integer_Value;
   use type AML_References.Node_Incarnation;
   use type AML_Frame_Handles.Invocation_Domain;
   use type AML_Frame_Handles.Invocation_Serial;
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package NS is new AML_Namespace
     (Perform_Delay => AML_Delays.Unavailable_Provider, Capacity => 8, Read_Microseconds => Capture, Max_Node_Incarnation => 4, Max_Invocations => 3);
   use NS;
   use NS.Owned;
   A, B : Arena;
   package Issuer_NS is new AML_Namespace (Perform_Delay => AML_Delays.Unavailable_Provider, Capacity => 8, Max_Invocations => 2);
   package Issuer renames Issuer_NS.Owned;
   use type Issuer_NS.State;
   use type Issuer_NS.Load_Status;
   I : Issuer.Arena;
   I_Prior : Issuer_NS.State;
   I_Loaded : Issuer_NS.Load_Status;
   Input : aliased AML_Table_Backing.State (1, 32);
   Saved, Current, Bad : Reference := AML_References.No_Reference;
   Captures : Natural := 0;
   Checks : Natural := 0;
   OK : Boolean;
   Loaded_Result : Load_Status;
   R : Execution_Result;
   V : Datum;
   S : Execution_Status;
   Invocation_Result : Invocation_Status;
   Domain, Old_Domain : AML_Frame_Handles.Invocation_Domain;
   Prior : NS.State;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
   begin
      Captures := Captures + 1;
      Ada.Text_IO.Put_Line ("CAPTURE nodes" & Node_Count (A)'Image);
      Check (Node_Count (A) = 3);
      Make_Named_Reference (A, 3, Current, OK); Check (OK and then Matches (A, Current));
      if Captures = 1 then Saved := Current;
      else Check (not Matches (A, Saved)); Check (Current /= Saved); end if;
      Resolve_Value (A, Current, V, S); Check (S = Returned and then V.Number = 35);
      Value := 0; Available := True;
   end Capture;
   Fixture : constant Bytes :=
     [16#14#,17,77,65,75,69,0,16#08#,84,69,77,80,16#0A#,35,16#5B#,16#33#,16#A4#,0,
      16#14#,17,78,69,88,84,0,16#08#,79,84,72,82,16#0A#,35,16#5B#,16#33#,16#A4#,0];
   Named : constant Bytes := [16#08#,65,65,65,65,16#0A#,35];
begin
   -- Exercise the tiny serial quota independently from live method execution.
   Issuer.Begin_Invocation (I, Domain, Invocation_Result);
   Check (Invocation_Result = Unsupported_Context and then Domain = AML_Frame_Handles.No_Domain);
   Issuer.Reset (I, OK); Check (OK);
   Issuer.Load (I, Named, Bits_64, I_Loaded); Check (I_Loaded = Issuer_NS.Loaded);
   I_Prior := Issuer.Snapshot (I);
   Issuer.Begin_Invocation (I, Domain, Invocation_Result);
   Check (Invocation_Result = Available and then Issuer.Invocation_Count (I) = 1 and then Issuer.Snapshot (I) = I_Prior);
   Old_Domain := Domain;
   Issuer.Load (I, [16#08#], Bits_64, I_Loaded);
   Check (I_Loaded /= Issuer_NS.Loaded and then Issuer.Snapshot (I) = I_Prior and then Issuer.Invocation_Count (I) = 1);
   Issuer.Begin_Invocation (I, Domain, Invocation_Result);
   Check (Invocation_Result = Available and then Domain /= Old_Domain and then Issuer.Invocation_Count (I) = 2);
   Issuer.Begin_Invocation (I, Domain, Invocation_Result);
   Check (Invocation_Result = Exhausted and then Domain = AML_Frame_Handles.No_Domain
          and then Issuer.Invocation_Count (I) = 2 and then Issuer.Snapshot (I) = I_Prior);
   Issuer.Reset (I, OK); Check (OK and then Issuer.Invocation_Count (I) = 0);
   Issuer.Begin_Invocation (I, Domain, Invocation_Result);
   Check (Invocation_Result = Available and then Domain /= Old_Domain and then Issuer.Invocation_Count (I) = 1);

   Reset (A, OK); Check (OK); Reset (B, OK); Check (OK);
   Load (A, Fixture, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Check (Last_Incarnation (Snapshot (A)) = 2);
   Prior := Snapshot (A);
   Load (A, [16#08#], Bits_64, Loaded_Result);
   Check (Loaded_Result /= Loaded and then Snapshot (A) = Prior and then Invocation_Count (A) = 0);
   Invoke (A, Input, 1, [others => (Integer_Datum, 0, AML_Decode.Ordinary_Integer)], 0, 100, R);
   Check (R.Status = Returned and then Captures = 1 and then Node_Count (A) = 2);
   Check (Invocation_Count (A) = 1);
   Check (not Matches (A, Saved) and then Last_Incarnation (Snapshot (A)) = 3);
   Resolve_Value (A, Saved, V, S); Check (S = Unsupported_Value);
   Invoke (A, Input, 2, [others => (Integer_Datum, 0, AML_Decode.Ordinary_Integer)], 0, 100, R);
   Check (R.Status = Returned and then Captures = 2 and then Last_Incarnation (Snapshot (A)) = 4);
   Check (Invocation_Count (A) = 2);
   Prior := Snapshot (A);
   Invoke (A, Input, 1, [others => (Integer_Datum, 0, AML_Decode.Ordinary_Integer)], 0, 100, R);
   Check (R.Status = Namespace_Limit and then Captures = 2 and then Snapshot (A) = Prior);
   Check (Invocation_Count (A) = 3);
   Make_Named_Reference (A, Natural'Last, Bad, OK); Check (not OK and then Bad = AML_References.No_Reference);
   Reset (A, OK); Check (OK and then Invocation_Count (A) = 0);
   Load (A, Named, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Make_Named_Reference (A, 1, Saved, OK); Check (OK);
   Load (B, Named, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Check (not Matches (B, Saved));
   Begin_Invocation (A, Old_Domain, Invocation_Result); Check (Invocation_Result = Available);
   Reset (A, OK); Check (OK);
   Load (A, Named, Bits_64, Loaded_Result); Check (Loaded_Result = Loaded);
   Make_Named_Reference (A, 1, Current, OK); Check (OK and then Current /= Saved and then not Matches (A, Saved));
   Begin_Invocation (A, Domain, Invocation_Result);
   Check (Invocation_Result = Available and then Domain /= Old_Domain and then Invocation_Count (A) = 1);
   Ada.Text_IO.Put_Line ("REFERENCE-MODEL PASS" & Checks'Image);
end Reference_Model_Tests;
