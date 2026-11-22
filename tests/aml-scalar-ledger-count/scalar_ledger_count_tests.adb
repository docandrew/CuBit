pragma Ada_2022;
with Ada.Text_IO;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Frame_Handles;
with AML_Scalar_Ledger;
with AML_Scalar_Ledger.Test_Inspection;
procedure Scalar_Ledger_Count_Tests is
   Owner : AML_Identity.Identity;
   Issued : Boolean;
   Domain : AML_Frame_Handles.Invocation_Domain;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "count check" & Checks'Image; end if;
   end Check;
   package L is new AML_Scalar_Ledger (3, 6, 50);
   package Scan is new L.Test_Inspection;
   use type L.State;
   use type L.Status;
   use type L.Handle;
   use type L.Number;
   S, Before, Bad : L.State;
   A, B, C, D, E, Stale, Foreign : L.Handle;
   Status : L.Status;
   procedure Count_Is (N : Natural) is
   begin
      Check (L.Valid (S));
      Check (L.Objects (S) = N and then Scan.Scanned_Objects (S) = N);
   end Count_Is;
begin
   AML_Identity.Issuer.Issue (Owner, Issued); Check (Issued);
   Domain := AML_Frame_Handles.Bind_Domain (Owner, 1);
   L.Initialize (S, Domain); Check (S = L.Empty (Domain)); Count_Is (0);
   Bad := S; Scan.Set_Cached_Count (Bad, 1); Check (not L.Valid (Bad));
   L.New_Value (S, L.Number'Last, L.Ordinary, A, Status); Check (Status = L.Ready); Count_Is (1);
   Bad := S; Scan.Set_Cached_Count (Bad, 0); Check (not L.Valid (Bad));
   Bad := S; Scan.Set_Cached_Count (Bad, 2); Check (not L.Valid (Bad));
   L.Share (S, A, B, Status); Check (Status = L.Ready); Count_Is (1);
   Stale := B; L.Move (S, B, C, Status);
   Check (Status = L.Ready and then B = L.No_Handle and then not L.Live (S, Stale)); Count_Is (1);
   L.Copy (S, A, D, Status); Check (Status = L.Ready and then not L.Same_Object (S, A, D)); Count_Is (2);
   L.Set_Result_Width (S, A, L.Bits_32, Status);
   Check (Status = L.Ready and then L.Read (S, C).Value = 16#FFFF_FFFF#
     and then L.Read (S, D).Value = L.Number'Last); Count_Is (2);
   L.Set_Result_Width (S, C, L.Bits_64, Status); Check (Status = L.Ready); Count_Is (2);
   L.Drop (S, A, Status); Check (Status = L.Ready); Count_Is (2);
   L.Share (S, D, E, Status); Check (Status = L.Ready); Count_Is (2);
   -- Retire C's object while D's object retains E: mixed final/nonfinal batch.
   L.Drop_Many (S, [C, D], Status); Check (Status = L.Ready); Count_Is (1);
   L.New_Value (S, 17, L.AML_Constant, A, Status); Check (Status = L.Ready); Count_Is (2);
   L.Share (S, A, B, Status); Check (Status = L.Ready); Count_Is (2);
   -- Both leases of one object retire it exactly once.
   L.Drop_Many (S, [A, B], Status); Check (Status = L.Ready); Count_Is (1);
   Before := S; L.Drop_Many (S, L.Handle_Array'(3 .. 2 => L.No_Handle), Status);
   Check (Status = L.Ready and then S = Before); Count_Is (1);
   L.Drop_Many (S, [E, E], Status); Check (Status = L.Invalid_Handle and then S = Before);
   L.Drop_Many (S, [E, Stale], Status); Check (Status = L.Invalid_Handle and then S = Before);
   L.Drop_Many (S, L.Handle_Array'(1 .. 7 => E), Status);
   Check (Status = L.Invalid_Handle and then S = Before); Count_Is (1);
   Bad := L.Empty (AML_Frame_Handles.Bind_Domain (Owner, 2));
   L.New_Value (Bad, 23, L.Ordinary, Foreign, Status); Check (Status = L.Ready);
   L.Drop_Many (S, [E, Foreign], Status); Check (Status = L.Invalid_Handle and then S = Before);
   L.Copy (S, Foreign, A, Status); Check (Status = L.Invalid_Handle and then S = Before);
   L.Set_Result_Width (S, Foreign, L.Bits_32, Status); Check (Status = L.Invalid_Handle and then S = Before);
   L.Drop (S, E, Status); Check (Status = L.Ready); Count_Is (0);
   L.New_Value (S, 29, L.Ordinary, A, Status); Check (Status = L.Ready); Count_Is (1);
   Before := S; Stale := A; L.Clear_All (S); Count_Is (0);
   Check (Scan.Issued (S) = Scan.Issued (Before) and then not L.Live (S, Stale));
   L.New_Value (S, 31, L.Ordinary, A, Status);
   Check (Status = L.Ready and then not L.Live (S, Stale)); Count_Is (1);
   S := L.Empty (AML_Frame_Handles.No_Domain); Before := S;
   L.New_Value (S, 1, L.Ordinary, A, Status);
   Check (Status = L.Invalid_Domain and then S = Before and then A = L.No_Handle);
   L.Drop_Many (S, [], Status); Check (Status = L.Ready and then S = Before); Count_Is (0);
   declare
      package F is new AML_Scalar_Ledger (1, 2, 10);
      package I is new F.Test_Inspection;
      use type F.State; use type F.Status; use type F.Handle;
      T : F.State := F.Empty (Domain); Prior : F.State;
      X, Y, Z : F.Handle; R : F.Status;
   begin
      F.New_Value (T, 1, F.Ordinary, X, R); Check (R = F.Ready);
      Prior := T; F.New_Value (T, 2, F.Ordinary, Y, R);
      Check (R = F.Object_Limit and then T = Prior and then Y = F.No_Handle);
      F.Copy (T, X, Y, R); Check (R = F.Object_Limit and then T = Prior);
      F.Share (T, X, Y, R); Check (R = F.Ready);
      Prior := T; F.New_Value (T, 3, F.Ordinary, Z, R);
      Check (R = F.Root_Limit and then T = Prior and then Z = F.No_Handle);
      F.Share (T, X, Z, R); Check (R = F.Root_Limit and then T = Prior);
      Check (F.Valid (T) and then F.Objects (T) = 1 and then I.Scanned_Objects (T) = 1);
   end;
   declare
      package F is new AML_Scalar_Ledger (1, 1, 1);
      package I is new F.Test_Inspection;
      use type F.State; use type F.Status; use type F.Handle;
      T : F.State := F.Empty (Domain); Prior : F.State;
      X, Y, Old : F.Handle; R : F.Status;
   begin
      F.New_Value (T, 1, F.Ordinary, X, R); Check (R = F.Ready);
      Prior := T; Old := X;
      F.New_Value (T, 2, F.Ordinary, Y, R);
      Check (R = F.Generation_Exhausted and then T = Prior and then Y = F.No_Handle);
      F.Share (T, X, Y, R); Check (R = F.Generation_Exhausted and then T = Prior);
      F.Move (T, X, Y, R);
      Check (R = F.Generation_Exhausted and then T = Prior and then X = Old);
      F.Copy (T, X, Y, R); Check (R = F.Generation_Exhausted and then T = Prior);
      F.Clear_All (T); Prior := T;
      Check (I.Issued (T) = 1 and then I.Scanned_Objects (T) = 0);
      F.New_Value (T, 3, F.Ordinary, Y, R);
      Check (R = F.Generation_Exhausted and then T = Prior and then F.Objects (T) = 0);
   end;
   Ada.Text_IO.Put_Line ("Scalar ledger count checks" & Checks'Image);
end Scalar_Ledger_Count_Tests;
