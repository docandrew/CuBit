with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Hardware_Catalog;
with Hardware_Authority;
with Hardware_Grants; use Hardware_Grants;
with Hardware_Grants.Cspace;
procedure Hardware_Cspace_Tests is
   package C renames Hardware_Catalog;
   package Gate renames Hardware_Grants.Cspace;
   G, Uninitialized, Other_Registry : State;
   Inventory, Saved : C.State;
   Table : CapabilityTable := EMPTY_TABLE;
   Root, Child : Handle;
   OK : Boolean;
   Result : C.Decision;
   Ticket : Unsigned_64;
   Checks : Natural := 0;
   Registry : Unsigned_64;
   Good : Capability;
   use type C.State;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Denied (Slot_Number : Unsigned_64; Write : Boolean; Value : Unsigned_64) is
   begin
      Saved := Inventory;
      Gate.Begin_Access (G, Table, Slot_Number, Inventory, Write, Value, Result, Ticket);
      Check (not Result.Allowed and Ticket = 0 and Saved = Inventory);
   end Denied;
begin
   Initialize (G, OK); Check (OK); Registry := Identity (G);
   Initialize (Other_Registry, OK); Check (OK and Identity (Other_Registry) /= Registry);
   Initialize (G, OK); Check (not OK and Identity (G) = Registry);
   C.Begin_Inventory (Inventory, OK); Check (OK);
   C.Add (Inventory, ((1, True, True, True, 255), C.ACPI, C.Memory_Space, 4096, 1), OK); Check (OK);
   C.Seal (Inventory, OK); Check (OK);
   Admit_Group (G, Inventory, C.ACPI, Root);
   Derive (G, Inventory, Root, (1, True, True, False, 3), Child); Check (Live (G, Child));
   Good := (CAP_HARDWARE_REGISTER, READ_WRITE, 0, (Child, Registry), INITIAL_GENERATION);
   Denied (0, False, 0);
   Table (0) := Good;
   declare Other_Root, Other_Child : Handle; begin
      Admit_Group (Other_Registry, Inventory, C.ACPI, Other_Root);
      Derive (Other_Registry, Inventory, Other_Root, (1, True, True, False, 3), Other_Child);
      Check (Other_Child = Child);
      Check (not Gate.Can_Access (Other_Registry, Table, 0, False));
   end;
   Denied (Unsigned_64'Last, False, 0); Denied (64, False, 0);
   for Kind in CapabilityType loop
      if Kind /= CAP_HARDWARE_REGISTER then
         Table (0).capType := Kind; Denied (0, True, 1);
      end if;
   end loop;
   Table (0) := Good; Table (0).gen := 0; Denied (0, False, 0);
   Table (0).gen := INITIAL_GENERATION + 1; Denied (0, False, 0);
   Table (0) := Good; Table (0).object.param := Registry + 1; Denied (0, False, 0);
   Table (0).object.param := 0; Denied (0, False, 0);
   Table (0) := Good; Table (0).object.ref := Root; Denied (0, False, 0);
   Table (0).object.ref := Unsigned_64'Last; Denied (0, False, 0);
   Table (0) := Good;
   Check (not Gate.Can_Access (Uninitialized, Table, 0, False));
   Denied (0, True, 4); Denied (0, False, 1);
   for Bits in Unsigned_32 range 0 .. 31 loop
      for R in CapabilityRight loop
         Table (0).rights (R) := (Bits and Shift_Left (1, CapabilityRight'Pos (R))) /= 0;
      end loop;
      for Write in Boolean loop
         Gate.Begin_Access (G, Table, 0, Inventory, Write, 0, Result, Ticket);
         Check (Result.Allowed = Table (0).rights (if Write then RIGHT_WRITE else RIGHT_READ));
         if Result.Allowed then
            Check (C.Busy (Inventory) and Ticket /= 0);
            C.Finish_Access (Inventory, Ticket, OK); Check (OK);
         else Check (not C.Busy (Inventory) and Ticket = 0); end if;
      end loop;
   end loop;
   Table (0) := Good;
   Gate.Begin_Access (G, Table, 0, Inventory, True, 3, Result, Ticket);
   Check (Result.Allowed and Result.Resource.Address = 4096);
   Revoke (G, Root); Check (not Gate.Can_Access (G, Table, 0, True));
   declare Outstanding : constant Unsigned_64 := Ticket; begin
      Denied (0, True, 1); Check (C.Busy (Inventory));
      C.Finish_Access (Inventory, Outstanding, OK); Check (OK);
   end;
   Denied (0, True, 1);
   declare
      Owner, Before_G : State;
      Parents, Children, Before_T : CapabilityTable := EMPTY_TABLE;
      R : CapabilityRights := READ_WRITE;
      Scope : Hardware_Authority.Permission := (1, True, True, True, 3);
      procedure Reject_Child (Source, Dest : Unsigned_64) is
      begin
         Before_G := Owner; Before_T := Children;
         Gate.Install_Child (Owner, Inventory, Parents, Source, Children, Dest, Scope, R, OK);
         Check (not OK and Owner = Before_G and Children = Before_T);
      end Reject_Child;
   begin
      R (RIGHT_GRANT) := True; R (RIGHT_REVOKE) := True;
      Gate.Install_Group (Owner, Inventory, C.ACPI, Parents, 0, R, OK);
      Check (not OK and Count (Owner) = 0 and Parents = EMPTY_TABLE);
      Initialize (Owner, OK); Check (OK);
      Gate.Install_Group (Owner, Inventory, C.ACPI, Parents, 63, R, OK);
      Check (not OK and Count (Owner) = 0 and Parents = EMPTY_TABLE);
      Gate.Install_Group (Owner, Inventory, C.ACPI, Parents, Unsigned_64'Last, R, OK);
      Check (not OK and Count (Owner) = 0);
      Gate.Install_Group (Owner, Inventory, C.ACPI, Parents, 0, R, OK);
      Check (OK and Count (Owner) = 1 and Parents (0).object.param = Identity (Owner));
      Before_G := Owner; Before_T := Parents;
      Gate.Install_Group (Owner, Inventory, C.ACPI, Parents, 0, R, OK);
      Check (not OK and Owner = Before_G and Parents = Before_T);
      Reject_Child (Unsigned_64'Last, 0); Reject_Child (0, 63); Reject_Child (0, Unsigned_64'Last);
      Parents (0).rights (RIGHT_GRANT) := False; Reject_Child (0, 0);
      Parents (0).rights (RIGHT_GRANT) := True;
      Scope.Resource_ID := 999; Reject_Child (0, 0); Scope.Resource_ID := 1;
      Scope.Write_Mask := 256; Reject_Child (0, 0); Scope.Write_Mask := 3;
      Parents (0).rights (RIGHT_READ) := False; Reject_Child (0, 0);
      Parents (0).rights (RIGHT_READ) := True;
      R (RIGHT_EXECUTE) := True; Reject_Child (0, 0); R (RIGHT_EXECUTE) := False;
      Scope.Delegable := False; Reject_Child (0, 0); Scope.Delegable := True;
      Gate.Install_Child (Owner, Inventory, Parents, 0, Children, 0, Scope, R, OK);
      Check (OK and Children (0).capType = CAP_HARDWARE_REGISTER);
      Check (Children (0).rights = R and Children (0).authorityTag = NO_AUTHORITY_TAG);
      Check (Descends_From (Owner, Children (0).object.ref, Parents (0).object.ref));
      Reject_Child (0, 0);
      Parents := Children; -- Stable kernel snapshot for same-table delegation.
      Scope.Write_Mask := 7; Reject_Child (0, 1); Scope.Write_Mask := 1;
      Scope.Delegable := False; R (RIGHT_GRANT) := False;
      Gate.Install_Child (Owner, Inventory, Parents, 0, Children, 1, Scope, R, OK);
      Check (OK and Descends_From (Owner, Children (1).object.ref, Parents (0).object.ref));
      Gate.Begin_Access (Owner, Children, 1, Inventory, True, 1, Result, Ticket);
      Check (Result.Allowed); C.Finish_Access (Inventory, Ticket, OK); Check (OK);
      Gate.Begin_Access (Owner, Children, 1, Inventory, True, 2, Result, Ticket);
      Check (not Result.Allowed and Ticket = 0);
      Revoke (Owner, Parents (0).object.ref);
      Check (not Gate.Can_Access (Owner, Children, 1, True));
      Reject_Child (0, 2);
      R (RIGHT_GRANT) := True; Scope.Delegable := True;
      Gate.Install_Group (Owner, Inventory, C.ACPI, Parents, 2, R, OK); Check (OK);
      while Count (Owner) < Maximum_Grants loop
         Gate.Install_Group (Owner, Inventory, C.ACPI, Parents, 3, R, OK); Check (OK);
         Parents (3) := NULL_CAPABILITY;
      end loop;
      Reject_Child (2, 2); -- No partial insertion when grant storage is full.
      Before_G := Owner; Before_T := Children;
      Gate.Install_Group (Owner, Inventory, C.ACPI, Children, 2, R, OK);
      Check (not OK and Owner = Before_G and Children = Before_T);
   end;
   for Bits in Unsigned_32 range 0 .. 31 loop
      declare
         Owner, Before : State;
         Roots, Kids, Bad : CapabilityTable := EMPTY_TABLE;
         R : CapabilityRights := READ_WRITE;
         Outstanding : Unsigned_64;
         procedure Reject (Slot_Number : Unsigned_64) is
         begin
            Before := Owner;
            Gate.Revoke (Owner, Bad, Slot_Number, OK);
            Check (not OK and Owner = Before);
         end Reject;
      begin
         Initialize (Owner, OK); Check (OK);
         R (RIGHT_GRANT) := True; R (RIGHT_REVOKE) := True;
         Gate.Install_Group (Owner, Inventory, C.ACPI, Roots, 0, R, OK); Check (OK);
         Gate.Install_Group (Owner, Inventory, C.ACPI, Roots, 1, R, OK); Check (OK);
         Gate.Install_Child (Owner, Inventory, Roots, 0, Kids, 0,
           (1, True, True, True, 3), R, OK); Check (OK);
         if Bits = 0 then
            Bad := Roots; Reject (Unsigned_64'Last); Reject (64);
            for Kind in CapabilityType loop
               if Kind /= CAP_HARDWARE_GROUP then
                  Bad (0).capType := Kind; Reject (0);
               end if;
            end loop;
            Bad := Roots; Bad (0).gen := 0; Reject (0);
            Bad := Roots; Bad (0).object.param := Identity (Owner) + 1; Reject (0);
            Bad := Roots; Bad (0).object.ref := Unsigned_64'Last; Reject (0);
            Bad := Roots; Bad (0).object.ref := Kids (0).object.ref; Reject (0);
         end if;
         Bad := Roots;
         for Right in CapabilityRight loop
            Bad (0).rights (Right) := (Bits and Shift_Left (1, CapabilityRight'Pos (Right))) /= 0;
         end loop;
         Gate.Begin_Access (Owner, Kids, 0, Inventory, True, 1, Result, Outstanding);
         Check (Result.Allowed);
         Before := Owner;
         Gate.Revoke (Owner, Bad, 0, OK);
         Check (OK = Bad (0).rights (RIGHT_REVOKE));
         Check (Live (Owner, Roots (1).object.ref));
         Check (C.Busy (Inventory));
         if OK then
            Check (not Live (Owner, Kids (0).object.ref));
            Check (not Gate.Can_Access (Owner, Kids, 0, True));
            Reject (0); -- Repeated revocation does not gain authority.
         else
            Check (Owner = Before and Gate.Can_Access (Owner, Kids, 0, True));
            Gate.Revoke (Owner, Kids, 0, OK); Check (OK);
            Check (Live (Owner, Roots (0).object.ref));
            Check (not Gate.Can_Access (Owner, Kids, 0, True));
         end if;
         C.Finish_Access (Inventory, Outstanding, OK); Check (OK);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("HARDWARE-CSPACE: PASS" & Checks'Image);
end Hardware_Cspace_Tests;
