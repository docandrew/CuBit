package body Hardware_Grants.Cspace with SPARK_Mode is
   procedure Install_Group
     (S : in out State; Catalog : Hardware_Catalog.State;
      Group : Hardware_Catalog.Resource_Group;
      Table : in out Capabilities.CapabilityTable; Destination : Unsigned_64;
      Rights : Capabilities.CapabilityRights; Accepted : out Boolean) is
      H : Handle;
   begin
      Accepted := False;
      if Destination >= Unsigned_64 (Capabilities.REPLY_CAP_SLOT)
        or else Table (Capabilities.CapabilitySlot (Destination)).capType /= Capabilities.CAP_NULL
        or else Rights (Capabilities.RIGHT_EXECUTE)
        or else not (Rights (Capabilities.RIGHT_READ) or Rights (Capabilities.RIGHT_WRITE))
      then return; end if;
      Admit_Group (S, Catalog, Group, H);
      if H = No_Grant then return; end if;
      Table (Capabilities.CapabilitySlot (Destination)) :=
        (Capabilities.CAP_HARDWARE_GROUP, Rights, Capabilities.NO_AUTHORITY_TAG,
         (H, Identity (S)), Capabilities.INITIAL_GENERATION);
      Accepted := True;
   end Install_Group;

   procedure Install_Child
     (S : in out State; Catalog : Hardware_Catalog.State;
      Source_Table : Capabilities.CapabilityTable; Source : Unsigned_64;
      Table : in out Capabilities.CapabilityTable; Destination : Unsigned_64;
      Requested : Hardware_Authority.Permission;
      Rights : Capabilities.CapabilityRights; Accepted : out Boolean) is
      Parent : Capabilities.Capability;
      H : Handle;
   begin
      Accepted := False;
      if Destination >= Unsigned_64 (Capabilities.REPLY_CAP_SLOT)
        or else Source > Unsigned_64 (Capabilities.CapabilitySlot'Last)
        or else Table (Capabilities.CapabilitySlot (Destination)).capType /= Capabilities.CAP_NULL
        or else Rights (Capabilities.RIGHT_EXECUTE)
        or else Requested.Readable /= Rights (Capabilities.RIGHT_READ)
        or else Requested.Writable /= Rights (Capabilities.RIGHT_WRITE)
        or else Requested.Delegable /= Rights (Capabilities.RIGHT_GRANT)
      then return; end if;
      Parent := Source_Table (Capabilities.CapabilitySlot (Source));
      if Parent.capType not in Capabilities.CAP_HARDWARE_GROUP | Capabilities.CAP_HARDWARE_REGISTER
        or else Identity (S) = 0 or else Parent.object.param /= Identity (S)
        or else Parent.gen /= Capabilities.INITIAL_GENERATION
        or else not Parent.rights (Capabilities.RIGHT_GRANT)
        or else not Capabilities.isSubsetOf (Rights, Parent.rights)
        or else not Live (S, Parent.object.ref)
      then return; end if;
      if S.Items (Slot (Parent.object.ref)).Is_Group /=
        (Parent.capType = Capabilities.CAP_HARDWARE_GROUP) then return; end if;
      Derive (S, Catalog, Parent.object.ref, Requested, H);
      if H = No_Grant then return; end if;
      Table (Capabilities.CapabilitySlot (Destination)) :=
        (Capabilities.CAP_HARDWARE_REGISTER, Rights, Capabilities.NO_AUTHORITY_TAG,
         (H, Identity (S)), Capabilities.INITIAL_GENERATION);
      Accepted := True;
   end Install_Child;

   function Can_Revoke
     (S : State; Table : Capabilities.CapabilityTable; Slot_Number : Unsigned_64)
      return Boolean is
      Cap : Capabilities.Capability;
   begin
      if Identity (S) = 0 or else Slot_Number > Unsigned_64 (Capabilities.CapabilitySlot'Last)
      then return False; end if;
      Cap := Table (Capabilities.CapabilitySlot (Slot_Number));
      if Cap.capType not in Capabilities.CAP_HARDWARE_GROUP | Capabilities.CAP_HARDWARE_REGISTER
        or else Cap.object.param /= Identity (S)
        or else Cap.gen /= Capabilities.INITIAL_GENERATION
        or else not Cap.rights (Capabilities.RIGHT_REVOKE)
        or else not Live (S, Cap.object.ref)
      then return False; end if;
      return S.Items (Slot (Cap.object.ref)).Is_Group =
        (Cap.capType = Capabilities.CAP_HARDWARE_GROUP);
   end Can_Revoke;

   procedure Revoke
     (S : in out State; Table : Capabilities.CapabilityTable;
      Slot_Number : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := Can_Revoke (S, Table, Slot_Number);
      if not Accepted then return; end if;
      Hardware_Grants.Revoke (S,
        Table (Capabilities.CapabilitySlot (Slot_Number)).object.ref);
   end Revoke;

   function Can_Access
     (S : State;
      Table : Capabilities.CapabilityTable; Slot_Number : Unsigned_64;
      For_Write : Boolean) return Boolean is
      Cap : Capabilities.Capability;
   begin
      if Identity (S) = 0 or else
        Slot_Number > Unsigned_64 (Capabilities.CapabilitySlot'Last)
      then return False; end if;
      Cap := Table (Capabilities.CapabilitySlot (Slot_Number));
      if Cap.capType /= Capabilities.CAP_HARDWARE_REGISTER
        or else Cap.object.param /= Identity (S)
        or else Cap.gen /= Capabilities.INITIAL_GENERATION
        or else not Cap.rights
          (if For_Write then Capabilities.RIGHT_WRITE else Capabilities.RIGHT_READ)
        or else not Live (S, Cap.object.ref)
      then return False; end if;
      -- Handles never recycle within this registry identity. Generation is
      -- fixed at INITIAL_GENERATION; ancestor revocation lives in S.
      return not S.Items (Slot (Cap.object.ref)).Is_Group;
   end Can_Access;

   procedure Begin_Access
     (S : State;
      Table : Capabilities.CapabilityTable; Slot_Number : Unsigned_64;
      Catalog : in out Hardware_Catalog.State;
      For_Write : Boolean; Value : Unsigned_64;
      Result : out Hardware_Catalog.Decision; Ticket : out Unsigned_64) is
   begin
      Result := (Allowed => False); Ticket := 0;
      if not Can_Access (S, Table, Slot_Number, For_Write) then return; end if;
      Hardware_Grants.Begin_Access (S, Catalog,
        Table (Capabilities.CapabilitySlot (Slot_Number)).object.ref,
        For_Write, Value, Result, Ticket);
   end Begin_Access;
end Hardware_Grants.Cspace;
