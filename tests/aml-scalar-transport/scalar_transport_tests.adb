pragma Ada_2022;
with Ada.Text_IO;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Frame_Handles;
with AML_Scalar_Ledger;
with AML_References;
with AML_Object_Identifiers;
with AML_Execute.Scalar_Transport_Test_Support;
procedure Scalar_Transport_Tests is
   Owner : AML_Identity.Identity;
   Issued : Boolean;
   Domain : AML_Frame_Handles.Invocation_Domain;
   Checks : Natural;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "lease census check" & Checks'Image; end if;
   end Check;
begin
   AML_Identity.Issuer.Issue (Owner, Issued);
   if not Issued then raise Program_Error with "issuer exhausted"; end if;
   Domain := AML_Frame_Handles.Bind_Domain (Owner, 1);
   AML_Execute.Scalar_Transport_Test_Support.Run (Domain, Checks);
   declare
      package L is new AML_Scalar_Ledger (2, 3, 16);
      use type L.Status;
      S : L.State := L.Empty (Domain);
      A, B, Stale, C : L.Handle;
      Status : L.Status;
      Position : Natural;
   begin
      Check (L.Lease_Position (S, L.No_Handle) = 0);
      L.New_Value (S, 1, L.Ordinary, A, Status); Check (Status = L.Ready);
      Position := L.Lease_Position (S, A); Check (Position in 1 .. 3);
      L.Share (S, A, B, Status);
      Check (Status = L.Ready and then L.Lease_Position (S, B) /= Position);
      Stale := A; L.Move (S, A, C, Status);
      Check (Status = L.Ready and then L.Lease_Position (S, Stale) = 0
        and then L.Lease_Position (S, C) = Position);
      L.Drop (S, C, Status); Check (Status = L.Ready and then L.Lease_Position (S, C) = 0);
      L.New_Value (S, 2, L.Ordinary, A, Status);
      Check (Status = L.Ready and then L.Lease_Position (S, A) = Position
        and then L.Lease_Position (S, Stale) = 0);
      L.Drop (S, A, Status); Check (Status = L.Ready);
      L.Drop (S, B, Status); Check (Status = L.Ready and then L.Roots (S) = 0);
   end;
   declare
      package L is new AML_Scalar_Ledger (1, 1, 2);
      use type L.Status;
      S : L.State := L.Empty (Domain);
      A, Stale : L.Handle;
      Status : L.Status;
   begin
      L.New_Value (S, 1, L.Ordinary, A, Status); Check (Status = L.Ready);
      Stale := A; L.Clear_All (S);
      Check (L.Roots (S) = 0 and then L.Objects (S) = 0 and then not L.Live (S, Stale));
      L.New_Value (S, 2, L.Ordinary, A, Status);
      Check (Status = L.Ready and then not L.Live (S, Stale));
      L.Clear_All (S); L.New_Value (S, 3, L.Ordinary, A, Status);
      Check (Status = L.Generation_Exhausted and then L.Roots (S) = 0);
   end;
   declare
      package L is new AML_Scalar_Ledger (2, 2, 8);
      use type L.State;
      use type L.Status;
      S : L.State := L.Empty (Domain);
      Foreign_State : L.State := L.Empty (AML_Frame_Handles.Bind_Domain (Owner, 2));
      Before : L.State;
      A, Foreign_Handle : L.Handle;
      Status : L.Status;
   begin
      L.New_Value (S, 1, L.Ordinary, A, Status); Check (Status = L.Ready);
      L.New_Value (Foreign_State, 2, L.Ordinary, Foreign_Handle, Status); Check (Status = L.Ready);
      Before := S; L.Drop_Many (S, [A, Foreign_Handle], Status);
      Check (Status = L.Invalid_Handle and then S = Before);
      -- A distinct fresh invocation domain must not authenticate prior tokens.
      L.Initialize (S, AML_Frame_Handles.Bind_Domain (Owner, 3));
      Check (S = L.Empty (AML_Frame_Handles.Bind_Domain (Owner, 3)));
      Check (not L.Live (S, A) and then not L.Live (S, Foreign_Handle));
      L.New_Value (S, 9, L.Ordinary, A, Status); Check (Status = L.Ready);
   end;
   declare
      More : Natural;
      -- The transport carries this opaque credential; it does not authenticate
      -- the owner/object. Arena authentication is outside this pure helper test.
      Source : constant AML_References.Object_Handle := AML_References.Bind_Object
        (Owner, AML_Object_Identifiers.Make_Address (1, AML_Object_Identifiers.First_Incarnation));
   begin
      AML_Execute.Scalar_Transport_Test_Support.Run_Identities
        (Domain, AML_Frame_Handles.Bind_Domain (Owner, 4), Source, AML_References.Bind_Object
          (Owner, AML_Object_Identifiers.Make_Address (2, AML_Object_Identifiers.First_Incarnation)), More);
      Checks := Checks + More;
   end;
   Ada.Text_IO.Put_Line ("Scalar transport checks" & Checks'Image);
end Scalar_Transport_Tests;
