pragma Ada_2022;
with Ada.Text_IO;
with AML_Module_Frames;
with AML_Frame_Handles;
with AML_Declaration_Contexts;
with AML_Identity.Issuer;
procedure Module_Frames_Tests is
   package M renames AML_Module_Frames;
   package F renames AML_Frame_Handles;
   package L renames AML_Declaration_Contexts;
   use type F.Frame_Handle, M.State, L.Scope_Identity;
   A, B : AML_Identity.Identity;
   Success, Accepted : Boolean;
   Stack : M.State := M.Empty;
   Leases : L.State := L.Empty;
   Parent, Child, Method : L.Lease;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   procedure Refused (Frame : F.Frame_Handle; Domain : F.Invocation_Domain) is
      Before : constant M.State := Stack;
   begin
      M.Push (Stack, Frame, Domain, Accepted);
      Check (not Accepted and then Stack = Before and then M.Valid (Stack));
   end Refused;
begin
   AML_Identity.Issuer.Issue (A, Success); Check (Success);
   AML_Identity.Issuer.Issue (B, Success); Check (Success);
   declare
      Domain : constant F.Invocation_Domain := F.Bind_Domain (A, 1);
      Foreign_Domain : constant F.Invocation_Domain := F.Bind_Domain (B, 1);
      Later_Domain : constant F.Invocation_Domain := F.Bind_Domain (A, 2);
      PF : constant F.Frame_Handle := F.Bind_Frame (Domain, 1, 1);
      MF : constant F.Frame_Handle := F.Bind_Frame (Domain, 2, 1);
      CF : constant F.Frame_Handle := F.Bind_Frame (Domain, 3, 1);
      Parent_Scope : constant L.Scope_Identity := L.Make_Scope (7, 8);
   begin
      Check (M.Valid (Stack) and then M.Count (Stack) = 0 and then M.Top (Stack) = F.No_Frame);
      M.Pop (Stack, PF, Accepted); Check (not Accepted and then M.Count (Stack) = 0);
      Refused (F.No_Frame, Domain); Refused (PF, F.No_Domain);
      L.Open (Leases, L.Module_Request, PF, 0, 1, Parent, Accepted); Check (Accepted);
      L.Push_Scope (Leases, Parent, L.Root_Scope, Parent_Scope, Accepted); Check (Accepted);
      M.Push (Stack, PF, Domain, Accepted); Check (Accepted and then M.Top (Stack) = PF);
      Refused (PF, Domain);
      Refused (F.Bind_Frame (Foreign_Domain, 4, 1), Domain);
      Refused (F.Bind_Frame (Foreign_Domain, 4, 1), Foreign_Domain);
      Refused (F.Bind_Frame (Later_Domain, 4, 1), Later_Domain);
      L.Open (Leases, L.Method_Request, MF, 9, 4, Method, Accepted); Check (Accepted);
      L.Open (Leases, L.Module_Request, CF, 0, 1, Child, Accepted); Check (Accepted);
      M.Push (Stack, CF, Domain, Accepted);
      Check (Accepted and then M.Count (Stack) = 2 and then M.Top (Stack) = CF
        and then M.Contains (Stack, PF) and then M.Contains (Stack, CF));
      Check (L.Is_Live (Leases, Parent) and then L.Is_Live (Leases, Method)
        and then L.Lexical_Top (Leases, Parent) = Parent_Scope and then L.Lexical_Count (Leases, Parent) = 1);
      declare Before : constant M.State := Stack; begin
         M.Pop (Stack, PF, Accepted); Check (not Accepted and then Stack = Before);
      end;
      M.Pop (Stack, CF, Accepted); Check (Accepted and then M.Top (Stack) = PF and then not M.Contains (Stack, CF));
      L.Close (Leases, Child);
      Check (L.Is_Live (Leases, Parent) and then L.Lexical_Top (Leases, Parent) = Parent_Scope);
      L.Close (Leases, Method);
      M.Pop (Stack, PF, Accepted); Check (Accepted and then M.Count (Stack) = 0);
      L.Close (Leases, Parent); Check (L.Valid (Leases) and then not L.Is_Live (Leases, Parent));
      for I in 1 .. M.Depth'Last loop
         declare Frame : constant F.Frame_Handle := F.Bind_Frame (Domain, F.Frame_Index (I), 2); begin
            M.Push (Stack, Frame, Domain, Accepted);
            Check (Accepted and then M.Count (Stack) = I and then M.Top (Stack) = Frame);
            Check (M.Contains (Stack, Frame) and then M.Belongs_To (Stack, Domain));
         end;
      end loop;
      Refused (F.Bind_Frame (Domain, 1, 3), Domain);
      for I in reverse 1 .. M.Depth'Last loop
         declare Frame : constant F.Frame_Handle := F.Bind_Frame (Domain, F.Frame_Index (I), 2); begin
            M.Pop (Stack, Frame, Accepted);
            Check (Accepted and then M.Count (Stack) = I - 1 and then not M.Contains (Stack, Frame));
         end;
      end loop;
      M.Push (Stack, F.Bind_Frame (Domain, 1, 3), Domain, Accepted); Check (Accepted);
      M.Pop (Stack, PF, Accepted); Check (not Accepted and then M.Count (Stack) = 1);
      M.Pop (Stack, F.Bind_Frame (Domain, 1, 3), Accepted); Check (Accepted);
      Check (M.Valid (Stack) and then M.Count (Stack) = 0 and then M.Top (Stack) = F.No_Frame
        and then not M.Contains (Stack, F.No_Frame));
   end;
   Ada.Text_IO.Put_Line ("Module frame checks" & Checks'Image);
end Module_Frames_Tests;
