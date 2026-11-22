pragma Ada_2022;
with Ada.Text_IO;
with AML_Declaration_Contexts;
with AML_Frame_Handles;
with AML_Identity.Issuer;
procedure Module_Scope_Tests is
   package L renames AML_Declaration_Contexts;
   package F renames AML_Frame_Handles;
   use type L.State, L.Lease, L.Scope_Identity, L.Scope_Incarnation;
   use type F.Frame_Handle;
   Store : L.State := L.Empty;
   Owner : AML_Identity.Identity;
   Good, Allowed : Boolean;
   Parent, Child, Method, Item : L.Lease;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
begin
   AML_Identity.Issuer.Issue (Owner, Good); Check (Good);
   declare
      Invocation : constant F.Invocation_Serial := 1000;
      Domain : constant F.Invocation_Domain := F.Bind_Domain (Owner, Invocation);
      PF : constant F.Frame_Handle := F.Bind_Frame (Domain, 1, 1);
      CF : constant F.Frame_Handle := F.Bind_Frame (Domain, 2, 1);
      MF : constant F.Frame_Handle := F.Bind_Frame (Domain, 3, 1);
      Selected : constant L.Scope_Identity := L.Make_Scope (7, 9);
      Nested : constant L.Scope_Identity := L.Make_Scope (8, 11);
   begin
      L.Open_Module (Store, PF, Selected, Invocation, Parent, Allowed);
      Check (Allowed and then L.Valid (Store) and then L.Is_Module (Parent));
      Check (L.Scope_Of (Parent) = 0 and then L.Stamp_Of (Parent) = 1000
        and then L.Stamp_Of (L.Entry_Identity (Parent)) = 9);
      Check (L.Frame_Of (Parent) = PF and then L.Entry_Identity (Parent) = Selected
        and then L.Lexical_Top (Store, Parent) = Selected and then L.Lexical_Count (Store, Parent) = 0);
      L.Push_Scope (Store, Parent, Selected, Nested, Allowed); Check (Allowed);
      declare Before : constant L.State := Store; begin
         L.Open_Module (Store, CF, L.No_Scope, Invocation, Item, Allowed);
         Check (not Allowed and then Item = L.No_Lease and then Store = Before);
         L.Open_Module (Store, F.No_Frame, Selected, Invocation, Item, Allowed);
         Check (not Allowed and then Store = Before);
         L.Open_Module (Store, PF, L.Root_Scope, Invocation, Item, Allowed);
         Check (not Allowed and then Store = Before);
         L.Open (Store, L.Module_Request, CF, 7, 1000, Item, Allowed);
         Check (not Allowed and then Store = Before);
      end;
      L.Open (Store, L.Method_Request, MF, 10, 17, Method, Allowed);
      Check (Allowed and then L.Scope_Of (Method) = 10 and then L.Entry_Identity (Method) = L.Make_Scope (10, 17));
      L.Open_Module (Store, CF, L.Make_Scope (13, 21), Invocation, Child, Allowed);
      Check (Allowed and then L.Scope_Of (Child) = 0 and then L.Stamp_Of (Child) = 1000
        and then L.Entry_Identity (Child) = L.Make_Scope (13, 21));
      Check (L.Is_Live (Store, Parent) and then L.Is_Live (Store, Method)
        and then L.Lexical_Top (Store, Parent) = Nested and then L.Lexical_Count (Store, Parent) = 1);
      L.Close (Store, Child);
      Check (L.Is_Live (Store, Parent) and then L.Lexical_Top (Store, Parent) = Nested);
      L.Pop_Scope (Store, Parent, Nested, Allowed);
      Check (Allowed and then L.Lexical_Top (Store, Parent) = Selected);
      L.Close (Store, Method); L.Close (Store, Parent); Check (Store = L.Empty);
      L.Open_Module (Store, F.Bind_Frame (Domain, 1, 2), L.Make_Scope (7, 10), Invocation, Item, Allowed);
      Check (Allowed and then not L.Is_Live (Store, Parent) and then L.Entry_Identity (Item) = L.Make_Scope (7, 10));
      declare Before : constant L.State := Store; begin
         L.Push_Scope (Store, Parent, Selected, Nested, Allowed);
         Check (not Allowed and then Store = Before);
      end;
      L.Close (Store, Item);
      L.Open_Module (Store, PF, L.Root_Scope, Invocation, Item, Allowed);
      Check (Allowed and then L.Scope_Of (Item) = 0 and then L.Entry_Identity (Item) = L.Root_Scope
        and then L.Stamp_Of (Item) = 1000);
      L.Close (Store, Item);
      declare
         type Receipts is array (Positive range 1 .. L.Max_Active_Executions) of L.Lease;
         Saved : Receipts;
      begin
         for I in Saved'Range loop
            L.Open_Module (Store, F.Bind_Frame (Domain, F.Frame_Index (I), 3),
              L.Make_Scope (L.Scope_Node (I + 10), L.Scope_Incarnation (I + 20)), Invocation, Saved (I), Allowed);
            Check (Allowed and then L.Is_Live (Store, Saved (I)) and then L.Scope_Of (Saved (I)) = 0);
         end loop;
         declare Before : constant L.State := Store; begin
            L.Open_Module (Store, F.Bind_Frame (Domain, 1, 4), Selected, Invocation, Item, Allowed);
            Check (not Allowed and then Item = L.No_Lease and then Store = Before);
         end;
         for I in reverse Saved'Range loop L.Close (Store, Saved (I)); end loop;
      end;
      Check (L.Valid (Store) and then Store = L.Empty);
   end;
   Ada.Text_IO.Put_Line ("Module scope checks" & Checks'Image);
end Module_Scope_Tests;
