with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with AML_Objects.Package_References;
with AML_References;
with Test_Namespace;
procedure Package_Store_Atomicity_Tests is
   package NS renames Test_Namespace;
   package Owner renames NS.Owned;
   use type NS.State;
   use type NS.Load_Status;
   use type AML_Objects.Package_References.Result_Status;
   A, Foreign : Owner.Arena;
   R, Foreign_R, Named, Stale : Owner.Reference;
   Loaded : NS.Load_Status;
   Made : AML_Objects.Package_References.Result_Status;
   Status : Execution_Status;
   OK : Boolean;
   Checks : Natural := 0;
   Fixture : constant Bytes :=
     [16#08#,16#50#,16#4B#,16#47#,16#30#,16#12#,4,1,16#0A#,7];
   procedure Check (Value : Boolean) is
   begin
      if not Value then raise Program_Error with Checks'Image; end if;
      Checks := Checks + 1;
   end Check;
   procedure Rejected (Target : Owner.Reference; Item : Datum;
                       Expected : Execution_Status := Unsupported_Value) is
      Before : constant NS.State := Owner.Snapshot (A);
   begin
      Owner.Store_Reference_Value (A, Target, Bits_64, Item, Status);
      Check (Status = Expected);
      Check (Owner.Snapshot (A) = Before);
   end Rejected;
   procedure Setup (Arena : in out Owner.Arena; Ref : out Owner.Reference) is
   begin
      Owner.Reset (Arena, OK); Check (OK);
      Owner.Load (Arena, Fixture, Bits_64, Loaded); Check (Loaded = NS.Loaded);
      Owner.Make_Element (Arena, NS.Data_Object (Owner.Snapshot (Arena), 1),
                          0, Ref, Made);
      Check (Made = AML_Objects.Package_References.Ready);
   end Setup;
   Item : Datum;
   Stale_Source, Live_Source : AML_References.Object_Handle;
   Exhausted : Boolean := False;
begin
   Setup (A, R); Setup (Foreign, Foreign_R);
   Rejected (AML_References.No_Reference, (Integer_Datum, 9, AML_Decode.Ordinary_Integer));
   Rejected (Foreign_R, (Integer_Datum, 9, AML_Decode.Ordinary_Integer));
   Rejected (R, (Reference_Datum, AML_References.No_Reference));
   Rejected (R, (Object_Datum, (others => <>)));
   Owner.Resolve_Value (Foreign, Foreign_R, Item, Status);
   Check (Status = Returned);
   -- Foreign package source: its owner-bound handle cannot authorize local copy.
   declare
      Source : AML_References.Object_Handle;
   begin
      Owner.Make_Source (Foreign, NS.Data_Object (Owner.Snapshot (Foreign), 1), Source, OK);
      Check (OK);
      Rejected (R, (Object_Datum, (Source => Source, ID => 1,
                                 Type_Code => 4, Size => 1, others => <>)));
   end;
   Owner.Make_Named_Reference (A, 1, Named, OK); Check (OK);
   Owner.Make_Source (A, NS.Data_Object (Owner.Snapshot (A), 1), Stale_Source, OK);
   Check (OK);
   Stale := R;
   Setup (A, R);
   Rejected (Stale, (Integer_Datum, 9, AML_Decode.Ordinary_Integer));
   Rejected (Named, (Integer_Datum, 9, AML_Decode.Ordinary_Integer));
   Rejected (R, (Object_Datum, (Source => Stale_Source, ID => 1,
                               Type_Code => 4, Size => 1, others => <>)));
   Owner.Make_Source (A, NS.Data_Object (Owner.Snapshot (A), 1), Live_Source, OK);
   Check (OK);
   -- Repeated independent integer allocations exhaust the fixed object quota.
   for I in 1 .. AML_Objects.Max_Objects + 1 loop
      declare
         Before : constant NS.State := Owner.Snapshot (A);
      begin
         Owner.Store_Reference_Value (A, R, Bits_64, (Integer_Datum, 9, AML_Decode.Ordinary_Integer), Status);
         if Status = Value_Limit then
            Check (Owner.Snapshot (A) = Before); Exhausted := True; exit;
         end if;
         Check (Status = Returned);
      end;
   end loop;
   Check (Exhausted);
   Rejected (R, (Integer_Datum, 9, AML_Decode.Ordinary_Integer), Value_Limit);
   Rejected (R, (Object_Datum, (Source => Live_Source, ID => 1,
                               Type_Code => 4, Size => 1, others => <>)), Value_Limit);
   Ada.Text_IO.Put_Line ("PACKAGE STORE ATOMICITY" & Checks'Image);
end Package_Store_Atomicity_Tests;
