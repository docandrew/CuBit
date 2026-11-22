with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Identity.Issuer;
with AML_Namespace;
with AML_References;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Mutex_Identity_Tests is
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   package E renames AML_Execute;
   procedure Read_Time (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace
     (Capacity => 16, Perform_Delay => AML_Delays.Unavailable_Provider,
      Read_Microseconds => Read_Time);
   package O renames N.Owned;
   use type E.Execution_Status;
   use type N.Load_Status;
   use type N.State;
   A, Foreign : O.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Success : Boolean;
   Loaded : N.Load_Status;
   Init : N.Initialization_Report;
   Raw : E.Execution_Result;
   Current, Stale, Other : E.Datum := (E.Integer_Datum, 0, Ordinary_Integer);
   Old_Method, Old_Name : AML_References.Reference;
   Checks : Natural := 0;
   Reentry_Probes : Natural := 0;
   Code : constant Bytes :=
     Method ("MAKE", [16#5B#, 1] & Enc ("TEMP") & [0, 16#A4#] & Enc ("TEMP"))
     & Block ([16#14#], Enc ("POLL") & [1, 16#5B#, 16#23#, 16#68#, 0, 0, 16#5B#, 16#27#, 16#68#, 16#A4#, 0])
     & Method ("RENT", [16#5B#, 1] & Enc ("TEMP")
         & [0, 16#5B#, 16#23#] & Enc ("TEMP") & [0, 0]
         & [16#70#, 16#5B#, 16#33#, 16#60#, 16#5B#, 16#27#]
         & Enc ("TEMP") & [16#A4#, 0])
     & [16#5B#, 1] & Enc ("RAW0") & [0];
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
   procedure Read_Time (Value : out Integer_Value; Available : out Boolean) is
      Before : constant N.State := O.Snapshot (A);
      Before_Frames : constant Natural := O.Frame_Root_Count (A);
      Before_Issuer : constant Natural := AML_Identity.Issuer.Issued_Count;
      Node : constant N.Node_ID := N.Child (Before, N.Root, "MAKE");
      Nested : E.Execution_Result;
   begin
      -- Bypass Collecting's In_Progress guard and exercise Owned admission.
      O.Invoke (A, Input, Node, [others => (E.Integer_Datum, 0, Ordinary_Integer)],
        0, 1000, Nested);
      Check (Nested.Status = E.Unsupported_Value, "direct Owned reentry refused");
      Check (Nested.Charged = 0, "refusal executes no AML");
      Check (O.Snapshot (A) = Before, "refusal preserves outer tree and objects");
      Check (O.Frame_Root_Count (A) = Before_Frames, "refusal preserves outer frames");
      Check (AML_Identity.Issuer.Issued_Count = Before_Issuer,
        "refusal issues no owner identity");
      Reentry_Probes := Reentry_Probes + 1;
      Value := 0; Available := True;
   end Read_Time;
   procedure Load (Owner : in out O.Arena; Width : Integer_Width) is
   begin
      O.Reset (Owner, Success); Check (Success, "reset");
      O.Load (Owner, Code, Width, Loaded); Check (Loaded = N.Loaded, "load");
      O.Initialize_Members (Owner, Init);
   end Load;
   procedure Make (Owner : in out O.Arena; Value : out E.Datum) is
      Node : constant N.Node_ID := N.Child (O.Snapshot (Owner), N.Root, "MAKE");
   begin
      O.Invoke (Owner, Input, Node, [others => (E.Integer_Datum, 0, Ordinary_Integer)], 0, 1000, Raw);
      Check (Raw.Status = E.Object_Returned, "real returned mutex");
      Value := (E.Object_Datum, Raw.Object);
      Check (O.Has_Source (Owner, Value.Object.Source), "issued object authority");
   end Make;
   procedure Poll (Value : E.Datum; Expected : E.Execution_Status) is
      Node : constant N.Node_ID := N.Child (O.Snapshot (A), N.Root, "POLL");
      Before : constant N.State := O.Snapshot (A);
   begin
      O.Invoke (A, Input, Node, [0 => Value, others => (E.Integer_Datum, 0, Ordinary_Integer)], 1, 1000, Raw);
      Check (Raw.Status = Expected, "authenticated poll status " & Raw.Status'Image);
      if Expected = E.Returned then Check (Raw.Value = 0, "poll acquired and released"); end if;
      Check (O.Snapshot (A) = Before, "poll preserves namespace and objects");
      Check (O.Frame_Root_Count (A) = 0, "frame cleanup");
   end Poll;
begin
   for Width in Integer_Width loop
      Load (A, Width); Load (Foreign, Width);
      declare
         Node : constant N.Node_ID := N.Child (O.Snapshot (A), N.Root, "RENT");
      begin
         O.Invoke (A, Input, Node, [others => (E.Integer_Datum, 0, Ordinary_Integer)],
           0, 1000, Raw);
         Check (Raw.Status = E.Returned, "outer lock survives rejected Owned reentry");
         Check (Raw.Value = 0, "outer release succeeds after reentry refusal");
         Check (O.Frame_Root_Count (A) = 0, "outer frames retire after reentry");
         Check (N.Child (O.Snapshot (A), Node, "TEMP") = N.Root,
           "outer method local retires after reentry");
      end;
      Make (A, Current); Make (Foreign, Other);
      Poll (Other, E.Unsupported_Value); Poll (Current, E.Returned);
      O.Make_Named_Identity (A, Natural (N.Child (O.Snapshot (A), N.Root, "RAW0")), Old_Name, Success);
      Check (Success, "real metadata-only named identity");
      Poll ((E.Reference_Datum, Old_Name), E.Unsupported_Value);
      O.Make_Named_Identity (A, Natural (N.Child (O.Snapshot (A), N.Root, "POLL")), Old_Method, Success);
      Check (Success, "real method identity");
      Stale := Current;
      Load (A, Width);
      Check (not O.Has_Source (A, Stale.Object.Source), "old object owner rejected after reset");
      Check (not O.Named_Identity_Matches (A, Old_Method), "old method incarnation/owner rejected");
      Check (not O.Named_Identity_Matches (A, Old_Name), "old named identity rejected");
      Poll (Stale, E.Unsupported_Value);
      Poll ((E.Reference_Datum, Old_Method), E.Unsupported_Value);
      Poll ((E.Reference_Datum, Old_Name), E.Unsupported_Value);
      Make (A, Current); Poll (Current, E.Returned);
   end loop;
   Check (Reentry_Probes = 2, "direct Owned refusal exercised in both widths");
   Ada.Text_IO.Put_Line ("Mutex identity checks" & Checks'Image);
end Mutex_Identity_Tests;
