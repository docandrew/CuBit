with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects.Root_Snapshots;
with AML_References;
procedure Auth_Snapshot_Tests is
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   package O renames AML_Objects;
   package R renames O.Root_Snapshots;
   use N; use N.Owned;
   use type O.Allocation_Status;
   use type Object_Root_Set;
   use type R.Result_Status;
   use type R.Snapshot_Phase;
   A, B : Arena;
   Roots : R.Snapshot;
   Expected : Object_Root_Set := [others => False];
   Build : Snapshot_Build_Status;
   Exec : Execution_Status;
   Alloc : O.Allocation_Status;
   ID, Other : O.Object_ID;
   Source, Foreign_Source : AML_References.Object_Handle;
   Value, Broken : Datum;
   Ref, Foreign_Ref : Reference;
   OK : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Check_Seeds (Good : Boolean) with Ghost;
   procedure Check_Seeds (Good : Boolean) is
      Keep : Object_Root_Set := [others => True];
      Resolved : R.Result_Status;
   begin
      R.Resolve (Roots, Generation (A), Value_Store (Snapshot (A)), Keep, Resolved);
      pragma Assert (if Good then Resolved = R.Ready and then Keep = Expected
                     else Resolved /= R.Ready and then Keep = Object_Root_Set'(others => False));
   end Check_Seeds;
   procedure Build_And_Check (Values : Root_Values; Want : Snapshot_Build_Status := Snapshot_Built) is
      Before : constant N.State := Snapshot (A);
   begin
      Build_Expression_Snapshot (A, Values, Roots, Build);
      Check (Build = Want and then Snapshot (A) = Before);
      if Want = Snapshot_Built then
         Check (R.Phase (Roots) = R.Published);
         Check_Seeds (True);
         pragma Assert (Expression_Snapshot_Matches (A, Values, Roots));
      else
         Check (R.Phase (Roots) = R.Rejected);
         if Initialized (A) then
            Check_Seeds (False);
         end if;
      end if;
   end Build_And_Check;
begin
   Build_And_Check ([], Snapshot_Uninitialized_Owner);
   Reset (A, OK); Check (OK); Reset (B, OK); Check (OK);
   Build_And_Check ([]); Build_And_Check ([(Integer_Datum, 5, Ordinary_Integer)]);
   Append (A, [1,2], ID, Alloc); Check (Alloc = O.Allocated);
   Append (A, [3], Other, Alloc); Check (Alloc = O.Allocated);
   Append (B, [9], Other, Alloc); Check (Alloc = O.Allocated);
   Make_Source (A, ID, Source, OK); Check (OK);
   Make_Source (B, Other, Foreign_Source, OK); Check (OK);
   Read_Source (A, Source, Value, Exec); Check (Exec = Returned);
   Expected (ID) := True; Build_And_Check ([Value]);
   Build_And_Check ([Value, (Integer_Datum, 7, Ordinary_Integer), Value]);
   Broken := Value; Broken.Object.ID := ID + 1;
   Build_And_Check ([Broken], Snapshot_Invalid_Value);
   Build_And_Check ([Value, Broken], Snapshot_Invalid_Value);
   Broken := Value; Broken.Object.Source := Foreign_Source;
   Build_And_Check ([Broken], Snapshot_Invalid_Value);
   Build_And_Check ([(Reference_Datum, AML_References.No_Reference)], Snapshot_Invalid_Value);
   Make_Index (A, Source, 0, Ref, Exec); Check (Exec = Returned);
   Build_And_Check ([(Reference_Datum, Ref)]);
   Make_Index (B, Foreign_Source, 0, Foreign_Ref, Exec); Check (Exec = Returned);
   Expected := [others => False]; Build_And_Check ([(Reference_Datum, Foreign_Ref)]);
   Build_And_Check ([]); -- replaces previous construction, never accumulates
   Reset (A, OK); Check (OK);
   Build_And_Check ([Value], Snapshot_Invalid_Value);
   Build_And_Check ([(Reference_Datum, Ref)]);
   Append (A, [4], Other, Alloc); Check (Alloc = O.Allocated and then Other = ID);
   Build_And_Check ([Value], Snapshot_Invalid_Value);
   Build_And_Check ([(Reference_Datum, Ref)]);
   Ada.Text_IO.Put_Line ("AUTH SNAPSHOTS" & Checks'Image);
end Auth_Snapshot_Tests;
