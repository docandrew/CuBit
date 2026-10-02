with Ada.Text_IO;
with Interfaces; use Interfaces;
with Hardware_Authority;
with Hardware_Catalog; use Hardware_Catalog;
procedure Hardware_Catalog_Tests is
   S, Before : State;
   Item : Descriptor := (Permission => (1, True, True, True, 255),
      Group => ACPI, Space => Memory_Space, Address => 4096, Width => 1);
   Child : Hardware_Authority.Permission := (1, False, True, False, 15);
   Accepted : Boolean;
   Old_Epoch : Unsigned_64;
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Check (not Can_Select (S, 0, ACPI, Child));
   Before := S; Add (S, Item, Accepted); Check (not Accepted and S = Before);
   Seal (S, Accepted); Check (not Accepted and S = Before);
   Begin_Inventory (S, Accepted); Check (Accepted and Epoch (S) = 1);
   Add (S, Item, Accepted); Check (Accepted);
   Before := S;
   Item.Group := GPIO; Add (S, Item, Accepted);
   Check (not Accepted and S = Before); -- Global identity cannot change category.
   Item.Permission.Resource_ID := 2;
   Add (S, Item, Accepted); Check (Accepted);
   Check (not Can_Select (S, Epoch (S), ACPI, Child));
   Seal (S, Accepted); Check (Accepted);
   Check (Can_Select (S, Epoch (S), ACPI, Child));
   Check (not Can_Select (S, Epoch (S), GPIO, Child));
   Check (not Can_Select (S, Epoch (S) + 1, ACPI, Child));
   Child.Write_Mask := 256;
   Check (not Can_Select (S, Epoch (S), ACPI, Child));
   Child.Write_Mask := 15;
   Before := S; Add (S, Item, Accepted); Check (not Accepted and S = Before);
   Begin_Inventory (S, Accepted); Check (not Accepted and S = Before);
   Old_Epoch := Epoch (S);
   Revoke (S); Check (not Can_Select (S, Old_Epoch, ACPI, Child));
   Begin_Inventory (S, Accepted); Check (Accepted and Count (S) = 0);
   Item.Group := ACPI;
   for I in 1 .. Maximum_Resources loop
      Item.Permission.Resource_ID := Unsigned_64 (I);
      Add (S, Item, Accepted); Check (Accepted);
   end loop;
   Before := S; Item.Permission.Resource_ID := 1000;
   Add (S, Item, Accepted); Check (not Accepted and S = Before);
   Seal (S, Accepted); Check (Accepted);
   Check (not Can_Select (S, Old_Epoch, ACPI, Child));
   Check (Can_Select (S, Epoch (S), ACPI, Child));
   declare
      Result : Decision;
      Nondelegable : Hardware_Authority.Permission := (1, True, True, False, 15);
   begin
      Result := Resolve (S, Epoch (S), Nondelegable, True, 15);
      Check (Result.Allowed);
      Check (Result.Resource.Address = 4096 and Result.Resource.Width = 1);
      Check (not Resolve (S, Old_Epoch, Nondelegable, True, 15).Allowed);
      Check (not Resolve (S, Epoch (S), Nondelegable, True, 16).Allowed);
      Check (not Resolve (S, Epoch (S), Nondelegable, False, 1).Allowed);
      Check (Resolve (S, Epoch (S), Nondelegable, False, 0).Allowed);
      Nondelegable.Resource_ID := 1000;
      Check (not Resolve (S, Epoch (S), Nondelegable, False, 0).Allowed);
      Nondelegable.Resource_ID := 1; Nondelegable.Write_Mask := 256;
      Check (not Resolve (S, Epoch (S), Nondelegable, True, 0).Allowed);
      Revoke (S); Begin_Inventory (S, Accepted); Check (Accepted);
      Item := (Permission => (1, True, True, False, 15),
        Group => ACPI, Space => Memory_Space, Address => 8192, Width => 1);
      Add (S, Item, Accepted); Check (Accepted);
      Seal (S, Accepted); Check (Accepted);
      Nondelegable := Item.Permission;
      Check (Resolve (S, Epoch (S), Nondelegable, True, 15).Allowed);
      Check (not Can_Select (S, Epoch (S), ACPI, Nondelegable));
      -- A catalog can allow access but forbid delegation entirely.
      Revoke (S);
      Check (not Resolve (S, Epoch (S), Nondelegable, True, 15).Allowed);
   end;
   declare
      Result : Decision;
      Ticket, First_Ticket, Refused_Ticket : Unsigned_64;
   begin
      Begin_Inventory (S, Accepted); Check (Accepted);
      Add (S, Item, Accepted); Check (Accepted);
      Seal (S, Accepted); Check (Accepted);
      Before := S;
      Begin_Access (S, Epoch (S) + 1, Item.Permission, True, 0, Result, Ticket);
      Check (not Result.Allowed and Ticket = 0 and S = Before);
      Begin_Access (S, Epoch (S), Item.Permission, True, 0, Result, Ticket);
      Check (Result.Allowed and Busy (S) and Ticket /= 0);
      First_Ticket := Ticket;
      Before := S;
      Begin_Access (S, Epoch (S), Item.Permission, True, 0, Result, Refused_Ticket);
      Check (not Result.Allowed and Refused_Ticket = 0 and S = Before);
      Finish_Access (S, Ticket + 1, Accepted); Check (not Accepted and S = Before);
      Revoke (S);
      Check (Busy (S) and not Ready_To_Release (S));
      Check (Resource_At (S, 1) = Item);
      Before := S;
      Begin_Inventory (S, Accepted); Check (not Accepted and S = Before);
      Add (S, Item, Accepted); Check (not Accepted and S = Before);
      Begin_Access (S, Epoch (S), Item.Permission, True, 0, Result, Refused_Ticket);
      Check (not Result.Allowed and Refused_Ticket = 0 and S = Before);
      Finish_Access (S, Ticket, Accepted); Check (Accepted and Ready_To_Release (S));
      Before := S;
      Finish_Access (S, Ticket, Accepted); Check (not Accepted and S = Before);
      Begin_Inventory (S, Accepted); Check (Accepted);
      Add (S, Item, Accepted); Check (Accepted);
      Seal (S, Accepted); Check (Accepted);
      Begin_Access (S, Epoch (S), Item.Permission, True, 0, Result, Ticket);
      Check (Result.Allowed and Ticket > First_Ticket);
      Before := S;
      Finish_Access (S, First_Ticket, Accepted); Check (not Accepted and S = Before);
      Finish_Access (S, Ticket, Accepted); Check (Accepted and not Busy (S));
   end;
   Item.Address := Unsigned_64'Last; Item.Width := 8;
   Check (not Valid (Item));
   Item.Address := 65536; Item.Space := IO_Space; Item.Width := 1;
   Check (not Valid (Item));
   Item.Address := 65535; Check (Valid (Item));
   Item.Width := 2; Check (not Valid (Item));
   Item.Address := 4096; Item.Width := 8; Check (not Valid (Item));
   Item.Space := Memory_Space; Item.Width := 3; Check (not Valid (Item));
   Item.Width := 1; Item.Permission.Write_Mask := 256; Check (not Valid (Item));
   Ada.Text_IO.Put_Line ("HARDWARE-CATALOG: PASS" & Checks'Image);
end Hardware_Catalog_Tests;
