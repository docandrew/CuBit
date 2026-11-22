with AML_Execute.Scalar_Transport;
with AML_Scalar_Ledger;
package body AML_Execute.Scalar_Transport_Test_Support is
   procedure Run (Domain : AML_Frame_Handles.Invocation_Domain; Checks : out Natural) is
      procedure Check (Good : Boolean) is
      begin
         Checks := Checks + 1;
         if not Good then raise Program_Error with "scalar transport check" & Checks'Image; end if;
      end Check;
      Full : constant Datum := (Integer_Datum, AML_Decode.Integer_Value'Last, AML_Decode.Ordinary_Integer);
      Small : constant Datum := (Integer_Datum, 17, AML_Decode.AML_Constant);
   begin
      Checks := 0;
      declare
         package T is new AML_Execute.Scalar_Transport (2, 1, 32);
         use type T.State;
         use type T.Outcome;
         use type T.Identity_Kind;
         S : T.State := T.Empty (Domain);
         Before : T.State;
         Status : T.Outcome;
      begin
         T.Initialize (S, Domain);
         Check (S = T.Empty (Domain));
         Check (T.Valid (S) and then T.Temporary_Roots (S) = 0);
         T.Put (S, 1, Full, Status);
         Check (Status = T.Ready and then T.Kind (S, 1) = T.Temporary_Scalar);
         Check (T.Snapshot (S, 1) = Full and then T.Temporary_Owners (S, 1) = 1);
         Before := S;
         for I in 1 .. 8 loop Check (T.Root_Projection (S, 1) = Full and then S = Before); end loop;
         T.Share (S, 1, 1, Status); Check (Status = T.Ready and then S = Before);
         T.Move (S, 1, 1, Status); Check (Status = T.Ready and then S = Before);
         T.Share (S, 1, 2, Status);
         Check (Status = T.Ready and then T.Same_Identity (S, 1, 2)
           and then T.Temporary_Roots (S) = 2 and then T.Temporary_Owners (S, 1) = 2);
         Before := S; T.Put (S, 1, Small, Status);
         Check (Status = T.Capacity_Limit and then S = Before);
         T.Clear (S, 2); Check (T.Temporary_Owners (S, 1) = 1);
         T.Put (S, 1, Small, Status);
         Check (Status = T.Ready and then T.Snapshot (S, 1) = Small and then T.Temporary_Roots (S) = 1);
         Before := S; T.Put (S, 2, Full, Status);
         Check (Status = T.Capacity_Limit and then S = Before);
         T.Move (S, 1, 2, Status);
         Check (Status = T.Ready and then not T.Occupied (S, 1)
           and then T.Snapshot (S, 2) = Small and then T.Temporary_Roots (S) = 1);
         Before := S; T.Share (S, 1, 2, Status);
         Check (Status = T.Invalid_Source and then S = Before);
         T.Move (S, 1, 2, Status); Check (Status = T.Invalid_Source and then S = Before);
         T.Clear_All (S); Check (T.Valid (S) and then T.Temporary_Roots (S) = 0);
      end;
      declare
         package T is new AML_Execute.Scalar_Transport (2, 2, 2);
         use type T.State;
         use type T.Outcome;
         S : T.State := T.Empty (Domain);
         Before : T.State;
         Status : T.Outcome;
      begin
         T.Put (S, 1, Full, Status); Check (Status = T.Ready);
         T.Share (S, 1, 2, Status); Check (Status = T.Ready);
         Before := S; T.Share (S, 1, 2, Status);
         Check (Status = T.Generation_Exhausted and then S = Before);
         T.Move (S, 1, 2, Status);
         Check (Status = T.Generation_Exhausted and then S = Before);
         T.Put (S, 1, Small, Status);
         Check (Status = T.Generation_Exhausted and then S = Before);
         T.Clear_All (S); Check (T.Valid (S) and then T.Temporary_Roots (S) = 0);
      end;
      declare
         package T is new AML_Execute.Scalar_Transport (1, 1, 8);
         use type T.Outcome;
         S : T.State := T.Empty (Domain);
         Status : T.Outcome;
      begin
         T.Put (S, 1, Full, Status); Check (Status = T.Ready);
         -- Physical staging object/root are separate from one usable slot.
         T.Put (S, 1, Small, Status);
         Check (Status = T.Ready and then T.Snapshot (S, 1) = Small and then T.Temporary_Roots (S) = 1);
         T.Clear_All (S); Check (T.Valid (S) and then T.Temporary_Roots (S) = 0);
      end;
      declare
         package T is new AML_Execute.Scalar_Transport (4, 4, 32);
         use type T.State;
         use type T.Outcome;
         S : T.State := T.Empty (Domain);
         Before : T.State;
         Status : T.Outcome;
      begin
         T.Put (S, 1, Full, Status); Check (Status = T.Ready);
         T.Share (S, 1, 2, Status); Check (Status = T.Ready);
         T.Put (S, 4, Small, Status); Check (Status = T.Ready);
         Before := S; T.Clear_Range (S, 4, 1); Check (S = Before);
         T.Clear_Range (S, 1, 1);
         Check (not T.Occupied (S, 1) and then T.Temporary_Owners (S, 2) = 1
           and then T.Snapshot (S, 2) = Full and then T.Snapshot (S, 4) = Small);
         Before := S; T.Clear_Range (S, 3, 3); Check (S = Before);
         T.Clear_Range (S, 4, 4);
         Check (not T.Occupied (S, 4) and then T.Snapshot (S, 2) = Full);
         T.Clear_Range (S, 1, 4);
         Check (T.Valid (S) and then T.Temporary_Roots (S) = 0);
      end;
      declare
         package L is new AML_Scalar_Ledger (3, 4, 32);
         use type L.State;
         use type L.Status;
         use type L.Reading;
         S : L.State := L.Empty (Domain);
         Before : L.State;
         A, B, C, Stale : L.Handle;
         Status : L.Status;
      begin
         L.New_Value (S, 17, L.Ordinary, A, Status); Check (Status = L.Ready);
         L.Share (S, A, B, Status); Check (Status = L.Ready);
         L.New_Value (S, 99, L.AML_Constant, C, Status); Check (Status = L.Ready);
         Before := S; L.Drop_Many (S, [], Status); Check (Status = L.Ready and then S = Before);
         L.Drop_Many (S, [A, A], Status); Check (Status = L.Invalid_Handle and then S = Before);
         L.Drop_Many (S, [A, L.No_Handle], Status); Check (Status = L.Invalid_Handle and then S = Before);
         Stale := B; L.Drop (S, B, Status); Check (Status = L.Ready);
         Before := S; L.Drop_Many (S, [A, Stale], Status);
         Check (Status = L.Invalid_Handle and then S = Before);
         L.Drop_Many (S, [A], Status);
         Check (Status = L.Ready and then not L.Live (S, A) and then L.Live (S, C)
           and then L.Read (S, C) = L.Read (Before, C) and then L.Owners (S, C) = 1);
         L.New_Value (S, 55, L.Ordinary, B, Status); Check (Status = L.Ready);
         Check (not L.Live (S, A) and then not L.Live (S, Stale));
         L.Drop_Many (S, [B, C], Status);
         Check (Status = L.Ready and then L.Roots (S) = 0 and then L.Objects (S) = 0);
      end;
   end Run;
   procedure Run_Identities
     (Domain, Other_Domain : AML_Frame_Handles.Invocation_Domain;
      Owned_Handle, Other_Handle : AML_References.Object_Handle; Checks : out Natural)
   is
      Full : constant Datum := (Integer_Datum, 99, AML_Decode.Ordinary_Integer);
      Small : constant Datum := (Integer_Datum, 17, AML_Decode.AML_Constant);
      Ref : constant Datum := (Reference_Datum, AML_References.No_Reference);
      procedure Check (Good : Boolean) is
      begin
         Checks := Checks + 1;
         if not Good then raise Program_Error with "canonical slot check" & Checks'Image; end if;
      end Check;
   begin
      Checks := 0;
      declare
         package T is new Scalar_Transport (3, 3, 100, 100);
         use type T.State;
         use type T.Outcome;
         use type T.Slot_Identity;
         S : T.State := T.Empty (Domain);
         Other : T.State := T.Empty (Other_Domain);
         Before : T.State;
         First, Second, Third, Fresh : T.Slot_Identity;
         Status : T.Outcome;
      begin
         Check (T.Position (T.No_Slot_Identity) = 0);
         Check (not T.Matches_Slot (S, T.No_Slot_Identity));
         Check (T.Current_Slot_Identity (S, 1) = T.No_Slot_Identity);
         T.Put (S, 1, Ref, Status); Check (Status = T.Ready);
         First := T.Current_Slot_Identity (S, 1);
         Check (T.Position (First) = 1 and then T.Matches_Slot (S, First));
         Before := S;
         Check (T.Current_Slot_Identity (S, 1) = First and then S = Before);
         T.Put (Other, 1, Ref, Status); Check (Status = T.Ready);
         Check (not T.Matches_Slot (Other, First)
           and then not T.Matches_Slot (S, T.Current_Slot_Identity (Other, 1)));
         T.Share (S, 1, 2, Status); Check (Status = T.Ready);
         Second := T.Current_Slot_Identity (S, 2);
         Check (Second /= First and then T.Position (Second) = 2
           and then T.Matches_Slot (S, First) and then T.Matches_Slot (S, Second));
         T.Put (S, 1, Ref, Status); Check (Status = T.Ready);
         Fresh := T.Current_Slot_Identity (S, 1);
         Check (Fresh /= First and then not T.Matches_Slot (S, First)
           and then T.Matches_Slot (S, Second));
         T.Clear (S, 1);
         Check (not T.Matches_Slot (S, Fresh) and then T.Current_Slot_Identity (S, 1) = T.No_Slot_Identity);
         T.Put (S, 1, Full, Status); Check (Status = T.Ready);
         Third := T.Current_Slot_Identity (S, 1);
         Check (Third /= First and then Third /= Fresh);
         T.Move (S, 1, 3, Status); Check (Status = T.Ready);
         Fresh := T.Current_Slot_Identity (S, 3);
         Check (T.Position (Fresh) = 3 and then not T.Matches_Slot (S, Third)
           and then T.Matches_Slot (S, Fresh) and then T.Matches_Slot (S, Second));
         Before := S;
         T.Move (S, 3, 3, Status); Check (Status = T.Ready and then S = Before);
         T.Share (S, 3, 3, Status); Check (Status = T.Ready and then S = Before);
         T.Clear_Range (S, 3, 3);
         Check (not T.Matches_Slot (S, Fresh) and then T.Matches_Slot (S, Second));
         T.Put_Owned (S, 1, 17, AML_Decode.Ordinary_Integer, Owned_Handle, Status);
         Check (Status = T.Ready);
         Third := T.Current_Slot_Identity (S, 1);
         Check (T.Matches_Slot (S, Third));
         T.Clear_All (S); Check (not T.Matches_Slot (S, Third) and then not T.Matches_Slot (S, Second));
         T.Put_Owned (S, 1, 17, AML_Decode.Ordinary_Integer, Owned_Handle, Status);
         Check (Status = T.Ready and then T.Current_Slot_Identity (S, 1) /= Third);
         Check (not T.Matches_Slot (S, Third) and then not T.Matches_Slot (S, First));
      end;
      declare
         package T is new Scalar_Transport (3, 3, 100, 3);
         use type T.State;
         use type T.Outcome;
         use type T.Slot_Identity;
         S : T.State := T.Empty (Domain);
         Before : T.State;
         First, Second, Third : T.Slot_Identity;
         Status : T.Outcome;
      begin
         T.Put (S, 1, Full, Status); Check (Status = T.Ready);
         T.Put (S, 2, Ref, Status); Check (Status = T.Ready);
         T.Put_Owned (S, 3, 9, AML_Decode.Ordinary_Integer, Owned_Handle, Status); Check (Status = T.Ready);
         First := T.Current_Slot_Identity (S, 1);
         Second := T.Current_Slot_Identity (S, 2);
         Third := T.Current_Slot_Identity (S, 3);
         Before := S;
         T.Put (S, 1, Small, Status); Check (Status = T.Generation_Exhausted and then S = Before);
         T.Put (S, 2, Ref, Status); Check (Status = T.Generation_Exhausted and then S = Before);
         T.Put_Owned (S, 3, 18, AML_Decode.AML_Constant, Owned_Handle, Status);
         Check (Status = T.Generation_Exhausted and then S = Before);
         T.Share (S, 1, 2, Status); Check (Status = T.Generation_Exhausted and then S = Before);
         T.Move (S, 1, 2, Status); Check (Status = T.Generation_Exhausted and then S = Before);
         for Index in T.Slot loop
            T.Share (S, Index, Index, Status); Check (Status = T.Ready and then S = Before);
            T.Move (S, Index, Index, Status); Check (Status = T.Ready and then S = Before);
         end loop;
         Check (T.Matches_Slot (S, First) and then T.Matches_Slot (S, Second) and then T.Matches_Slot (S, Third));
         T.Clear_All (S); Before := S;
         T.Put (S, 1, Ref, Status); Check (Status = T.Generation_Exhausted and then S = Before);
         Check (not T.Matches_Slot (S, First) and then not T.Matches_Slot (S, Second) and then not T.Matches_Slot (S, Third));
         T.Initialize (S, Other_Domain);
         T.Put (S, 1, Full, Status); Check (Status = T.Ready);
         Check (not T.Matches_Slot (S, First) and then T.Current_Slot_Identity (S, 1) /= First);
      end;
      declare
         package T is new Scalar_Transport (2, 2, 10, 10);
         use type T.Outcome;
         use type T.Slot_Identity;
         A : T.State := T.Empty (AML_Frame_Handles.No_Domain);
         B : T.State := T.Empty (AML_Frame_Handles.No_Domain);
         Status : T.Outcome;
      begin
         T.Put (A, 1, Ref, Status); Check (Status = T.Ready);
         T.Put (B, 1, Ref, Status); Check (Status = T.Ready);
         T.Put_Owned (A, 2, 9, AML_Decode.Ordinary_Integer, Owned_Handle, Status); Check (Status = T.Ready);
         Check (T.Current_Slot_Identity (A, 1) = T.No_Slot_Identity
           and then T.Current_Slot_Identity (B, 1) = T.No_Slot_Identity
           and then T.Current_Slot_Identity (A, 2) = T.No_Slot_Identity);
         Check (not T.Matches_Slot (A, T.Current_Slot_Identity (B, 1))
           and then not T.Matches_Slot (B, T.Current_Slot_Identity (A, 1)));
         Check (T.Valid (A) and then T.Valid (B));
      end;
      declare
         package T is new Scalar_Transport (3, 3, 100, 100);
         use type T.Outcome;
         use type T.Slot_Identity;
         S : T.State := T.Empty (Domain);
         Status : T.Outcome;
         Saved : T.Slot_Identity;
         Changed : constant Datum := (Integer_Datum, 34, AML_Decode.Ordinary_Integer);
      begin
         T.Put_Owned (S, 1, 17, AML_Decode.Ordinary_Integer, Owned_Handle, Status);
         Check (Status = T.Ready);
         T.Share (S, 1, 2, Status); Check (Status = T.Ready);
         T.Put (S, 3, Small, Status); Check (Status = T.Ready);
         Saved := T.Current_Slot_Identity (S, 1);
         T.Refresh_Owned_Cache (S, 1, 34, AML_Decode.Ordinary_Integer);
         Check (T.Valid (S) and then T.Current_Slot_Identity (S, 1) = Saved);
         Check (T.Snapshot (S, 1) = Changed and then T.Same_Identity (S, 1, 2));
         Check (T.Snapshot (S, 2) = (Integer_Datum, 17, AML_Decode.Ordinary_Integer));
         Check (T.Snapshot (S, 3) = Small and then T.Temporary_Roots (S) = 1);
         Check (T.Root_Projection (S, 1) = T.Root_Projection (S, 2));
         T.Clear_All (S); Check (not T.Matches_Slot (S, Saved));
      end;
      declare
         package T is new Scalar_Transport (3, 3, 100, 100);
         use type T.Outcome;
         use type T.State;
         S : T.State := T.Empty (Domain);
         Before : T.State;
         Status : T.Outcome;
         Shared : Boolean;
         Saved, Stale : T.Slot_Identity;
      begin
         T.Other_Owned_Lease (S, T.No_Slot_Identity, Shared, Status);
         Check (Status = T.Invalid_Source and then not Shared);
         T.Put_Owned (S, 1, 17, AML_Decode.Ordinary_Integer, Owned_Handle, Status);
         Check (Status = T.Ready);
         Saved := T.Current_Slot_Identity (S, 1);
         Before := S;
         T.Other_Owned_Lease (S, Saved, Shared, Status);
         Check (Status = T.Ready and then not Shared and then S = Before);
         T.Put_Owned (S, 2, 17, AML_Decode.Ordinary_Integer, Other_Handle, Status);
         Check (Status = T.Ready);
         T.Other_Owned_Lease (S, Saved, Shared, Status);
         Check (Status = T.Ready and then not Shared);
         T.Clear (S, 2);
         -- Publication and stale cached bits do not add/remove semantic leases.
         for Index in 1 .. 3 loop
            Check (T.Root_Projection (S, 1).Value_Kind = Object_Datum);
         end loop;
         T.Share (S, 1, 2, Status); Check (Status = T.Ready);
         T.Refresh_Owned_Cache (S, 2, 34, AML_Decode.AML_Constant);
         Before := S;
         T.Other_Owned_Lease (S, Saved, Shared, Status);
         Check (Status = T.Ready and then Shared and then S = Before);
         T.Clear (S, 2);
         T.Put (S, 2, Full, Status); Check (Status = T.Ready);
         T.Put (S, 3, Ref, Status); Check (Status = T.Ready);
         T.Other_Owned_Lease (S, Saved, Shared, Status);
         Check (Status = T.Ready and then not Shared);
         T.Other_Owned_Lease (S, T.Current_Slot_Identity (S, 2), Shared, Status);
         Check (Status = T.Invalid_Source and then not Shared);
         T.Other_Owned_Lease (S, T.Current_Slot_Identity (S, 3), Shared, Status);
         Check (Status = T.Invalid_Source and then not Shared);
         Stale := Saved;
         T.Move (S, 1, 3, Status); Check (Status = T.Ready);
         T.Other_Owned_Lease (S, Stale, Shared, Status);
         Check (Status = T.Invalid_Source and then not Shared);
         Saved := T.Current_Slot_Identity (S, 3);
         T.Other_Owned_Lease (S, Saved, Shared, Status);
         Check (Status = T.Ready and then not Shared);
         T.Initialize (S, Other_Domain);
         T.Put_Owned (S, 3, 17, AML_Decode.Ordinary_Integer, Owned_Handle, Status);
         Check (Status = T.Ready);
         T.Other_Owned_Lease (S, Saved, Shared, Status);
         Check (Status = T.Invalid_Source and then not Shared);
      end;
      declare
         package T is new Scalar_Transport (3, 3, 100, 100);
         use type T.State;
         use type T.Outcome;
         use type T.Slot_Identity;
         Buffer_Type_Code : constant Natural := 3;
         Package_Type_Code : constant Natural := 4;
         S : T.State := T.Empty (Domain);
         Before : T.State;
         Status : T.Outcome;
         First, Second : T.Slot_Identity;
         Initial : constant Object_Value :=
           (Source => Owned_Handle, ID => AML_References.Source (Owned_Handle),
            Type_Code => Buffer_Type_Code, Size => 1, others => <>);
         Changed : constant Object_Value := (Initial with delta Size => 2);
         Foreign_Value : constant Object_Value :=
           (Changed with delta Source => Other_Handle, ID => AML_References.Source (Other_Handle));
      begin
         T.Put (S, 1, (Object_Datum, Initial), Status); Check (Status = T.Ready);
         T.Share (S, 1, 2, Status); Check (Status = T.Ready);
         T.Put (S, 3, Full, Status); Check (Status = T.Ready);
         First := T.Current_Slot_Identity (S, 1); Second := T.Current_Slot_Identity (S, 2);
         Check (T.Compound_Identity_Matches (S, 1, Changed));
         T.Refresh_Compound_Cache (S, 1, Changed, Status);
         Check (Status = T.Ready and then T.Snapshot (S, 1) = (Object_Datum, Changed));
         Check (T.Current_Slot_Identity (S, 1) = First and then T.Current_Slot_Identity (S, 2) = Second);
         Check (T.Snapshot (S, 2) = (Object_Datum, Initial) and then T.Snapshot (S, 3) = Full);
         Check (T.Temporary_Roots (S) = 1 and then T.Temporary_Owners (S, 3) = 1);
         Before := S;
         T.Refresh_Compound_Cache (S, 1, Foreign_Value, Status);
         Check (Status = T.Invalid_Source and then S = Before);
         T.Refresh_Compound_Cache (S, 1, (Changed with delta Type_Code => Package_Type_Code), Status);
         Check (Status = T.Invalid_Source and then S = Before);
         T.Refresh_Compound_Cache (S, 3, Changed, Status);
         Check (Status = T.Invalid_Source and then S = Before);
         T.Clear (S, 1); Before := S;
         T.Refresh_Compound_Cache (S, 1, Changed, Status);
         Check (Status = T.Invalid_Source and then S = Before);
      end;
      declare
         package T is new Scalar_Transport (6, 6, 100, 100);
         use type T.State;
         use type T.Outcome;
         S : T.State := T.Empty (Domain);
         Before : T.State;
         Status : T.Outcome;
         Source, Stale : T.Slot_Identity;
         Seen : Natural := 0;
         Reject : Boolean := False;
         -- These two canonical cells represent independently live frame/operand
         -- leases. Their borrowed projections below must add no visits.
         procedure Visit (Value : Datum; Accepted : out Boolean) is
            use type AML_References.Reference;
         begin
            Seen := Seen + 1;
            Check (Value.Value_Kind = Reference_Datum and then Value.Ref = Ref.Ref);
            Accepted := not Reject;
         end Visit;
         procedure Enumerate is new T.Visit_Compound_Owners (Visit);
      begin
         T.Put_Owned (S, 1, 17, AML_Decode.Ordinary_Integer, Owned_Handle, Status);
         Check (Status = T.Ready);
         Source := T.Current_Slot_Identity (S, 1);
         T.Put (S, 2, Ref, Status); Check (Status = T.Ready);
         T.Share (S, 2, 3, Status); Check (Status = T.Ready);
         T.Put (S, 4, Full, Status); Check (Status = T.Ready);
         T.Put_Owned (S, 5, 17, AML_Decode.Ordinary_Integer, Other_Handle, Status);
         Check (Status = T.Ready);
         declare
            Borrowed : constant Datum := T.Root_Projection (S, 2);
            Scalar_Projection : constant Datum := T.Root_Projection (S, 1);
         begin
            Check (Borrowed = Ref and then Scalar_Projection.Value_Kind = Object_Datum);
            Before := S;
            Enumerate (S, Source, Status);
            Check (Status = T.Ready and then Seen = 2 and then S = Before);
         end;
         Seen := 0; Reject := True;
         Enumerate (S, Source, Status);
         Check (Status = T.Invalid_Source and then Seen = 1 and then S = Before);
         Reject := False; Seen := 0; Stale := Source;
         T.Move (S, 1, 6, Status); Check (Status = T.Ready);
         Enumerate (S, Stale, Status); Check (Status = T.Invalid_Source and then Seen = 0);
         Enumerate (S, T.Current_Slot_Identity (S, 2), Status);
         Check (Status = T.Invalid_Source and then Seen = 0);
         T.Clear (S, 2); Source := T.Current_Slot_Identity (S, 6);
         Enumerate (S, Source, Status); Check (Status = T.Ready and then Seen = 1);
      end;
   end Run_Identities;
end AML_Execute.Scalar_Transport_Test_Support;
