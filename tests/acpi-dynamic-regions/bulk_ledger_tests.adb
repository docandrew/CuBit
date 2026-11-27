pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with AML_Dynamic_Region_Connections;
procedure Bulk_Ledger_Tests is
   package C is new AML_Dynamic_Region_Connections (4);
   use C;
   Run : State;
   Ready : constant Ready_Set := [others => True];
   Accepted, Found : Boolean;
   Item : Region_Identity;
   Space : Interfaces.Unsigned_8;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "bulk ledger check" & Checks'Image; end if;
   end Check;
begin
   for N in Region_Node loop
      Publish (Run, (N, 7), Interfaces.Unsigned_8 (N), Accepted);
      Check (Accepted);
   end loop;
   Adopt_Bulk (Run, (1, 7), Announced, Accepted);
   Check (Accepted and then Observe (Run, (1, 7)).Current = Externally_Announced);
   Adopt_Bulk (Run, (2, 7), Covered, Accepted);
   Check (Accepted and then Observe (Run, (2, 7)).Current = Externally_Covered);
   Adopt_Bulk (Run, (3, 7), Skipped, Accepted);
   Check (Accepted and then Observe (Run, (3, 7)).Current = Externally_Skipped);
   Adopt_Bulk (Run, (4, 7), Announced, Accepted);
   Check (Accepted);
   Adopt_Bulk (Run, (1, 6), Announced, Accepted);
   Check (not Accepted and then Observe (Run, (1, 7)).Current = Externally_Announced);
   Adopt_Bulk (Run, (1, 7), Announced, Accepted);
   Check (not Accepted);
   Begin_Turn (Run, 1, 1, Accepted);
   Check (Accepted);
   Take_Next (Run, Ready, Found, Item, Space);
   Check (not Found);
   Begin_Turn (Run, 2, 2, Accepted);
   Check (Accepted);
   Take_Next (Run, Ready, Found, Item, Space);
   Check (not Found);
   for N in Region_Node loop
      Begin_Disconnect (Run, (N, 7), Accepted);
      Check (not Accepted);
      Retire (Run, (N, 7), Accepted);
      Check (not Accepted);
   end loop;
   Release_Bulk (Run, (1, 6), True, Accepted);
   Check (not Accepted);
   Release_Bulk (Run, (1, 7), False, Accepted);
   Check (Accepted and then Observe (Run, (1, 7)).Current = External_Disconnect_Failed);
   Retire (Run, (1, 7), Accepted);
   Check (not Accepted);
   Release_Bulk (Run, (1, 7), True, Accepted);
   Check (not Accepted);
   for N in Region_Node range 2 .. 4 loop
      Release_Bulk (Run, (N, 7), True, Accepted);
      Check (Accepted and then Observe (Run, (N, 7)).Current = Externally_Closed);
      Begin_Turn (Run, N + 2, N + 2, Accepted);
      Check (Accepted);
      Take_Next (Run, Ready, Found, Item, Space);
      Check (not Found);
      Retire (Run, (N, 7), Accepted);
      Check (Accepted);
   end loop;
   Ada.Text_IO.Put_Line ("bulk ledger checks" & Checks'Image);
end Bulk_Ledger_Tests;
