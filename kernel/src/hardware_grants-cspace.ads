with Capabilities;
-- Internal kernel boundary. Table is the caller's actual kernel-owned cspace,
-- not an IPC buffer. Registry lifetime identity comes from S, allocated by
-- Hardware_Grants.Initialize; it is never supplied by a request.
package Hardware_Grants.Cspace with SPARK_Mode is
   use type Capabilities.CapabilityType;
   use type Capabilities.Capability;
   use type Capabilities.CapabilityTable;
   use type Capabilities.CapabilityRights;
   -- Trusted startup policy only; never export group installation as a syscall.
   -- Both installers reject occupied, out-of-range and reserved reply slots.
   procedure Install_Group
     (S : in out State; Catalog : Hardware_Catalog.State;
      Group : Hardware_Catalog.Resource_Group;
      Table : in out Capabilities.CapabilityTable; Destination : Unsigned_64;
      Rights : Capabilities.CapabilityRights; Accepted : out Boolean) with
     Post => (if Accepted then Destination < Unsigned_64 (Capabilities.REPLY_CAP_SLOT)
       and then Table (Capabilities.CapabilitySlot (Destination)).capType = Capabilities.CAP_HARDWARE_GROUP
       and then Table (Capabilities.CapabilitySlot (Destination)).object.param = Identity (S)
       and then Table (Capabilities.CapabilitySlot (Destination)).gen = Capabilities.INITIAL_GENERATION
       and then Table (Capabilities.CapabilitySlot (Destination)).authorityTag = Capabilities.NO_AUTHORITY_TAG
       and then Table (Capabilities.CapabilitySlot (Destination)).rights = Rights
       and then Live (S, Table (Capabilities.CapabilitySlot (Destination)).object.ref)
       and then Count (S) = Count (S'Old) + 1
       and then Table'Old (Capabilities.CapabilitySlot (Destination)).capType = Capabilities.CAP_NULL
       else S = S'Old and then Table = Table'Old)
       and then (for all I in Capabilities.CapabilitySlot =>
         (if Unsigned_64 (I) /= Destination then Table (I) = Table'Old (I)));
   -- Source_Table is a stable kernel-owned snapshot under the same cspace lock;
   -- use a snapshot for same-table delegation to avoid aliased in/out actuals.
   -- Rights and Requested may be untrusted: both must narrow parent authority.
   procedure Install_Child
     (S : in out State; Catalog : Hardware_Catalog.State;
      Source_Table : Capabilities.CapabilityTable; Source : Unsigned_64;
      Table : in out Capabilities.CapabilityTable; Destination : Unsigned_64;
      Requested : Hardware_Authority.Permission;
      Rights : Capabilities.CapabilityRights; Accepted : out Boolean) with
     Post => (if Accepted then Destination < Unsigned_64 (Capabilities.REPLY_CAP_SLOT)
       and then Source <= Unsigned_64 (Capabilities.CapabilitySlot'Last)
       and then Source_Table (Capabilities.CapabilitySlot (Source)).rights (Capabilities.RIGHT_GRANT)
       and then Capabilities.isSubsetOf (Rights, Source_Table (Capabilities.CapabilitySlot (Source)).rights)
       and then Table (Capabilities.CapabilitySlot (Destination)).capType = Capabilities.CAP_HARDWARE_REGISTER
       and then Table (Capabilities.CapabilitySlot (Destination)).object.param = Identity (S)
       and then Table (Capabilities.CapabilitySlot (Destination)).gen = Capabilities.INITIAL_GENERATION
       and then Table (Capabilities.CapabilitySlot (Destination)).authorityTag = Capabilities.NO_AUTHORITY_TAG
       and then Table (Capabilities.CapabilitySlot (Destination)).rights = Rights
       and then Live (S, Table (Capabilities.CapabilitySlot (Destination)).object.ref)
       and then Descends_From (S, Table (Capabilities.CapabilitySlot (Destination)).object.ref,
         Source_Table (Capabilities.CapabilitySlot (Source)).object.ref)
       and then Table'Old (Capabilities.CapabilitySlot (Destination)).capType = Capabilities.CAP_NULL
       else S = S'Old and then Table = Table'Old)
       and then (for all I in Capabilities.CapabilitySlot =>
         (if Unsigned_64 (I) /= Destination then Table (I) = Table'Old (I)));
   function Can_Revoke
     (S : State; Table : Capabilities.CapabilityTable; Slot_Number : Unsigned_64)
      return Boolean with
     Post => (if Can_Revoke'Result then Identity (S) /= 0
       and then Slot_Number <= Unsigned_64 (Capabilities.CapabilitySlot'Last)
       and then Table (Capabilities.CapabilitySlot (Slot_Number)).rights (Capabilities.RIGHT_REVOKE)
       and then Table (Capabilities.CapabilitySlot (Slot_Number)).gen = Capabilities.INITIAL_GENERATION
       and then Table (Capabilities.CapabilitySlot (Slot_Number)).object.param = Identity (S)
       and then Live (S, Table (Capabilities.CapabilitySlot (Slot_Number)).object.ref));
   -- Revokes this grant and its descendants, including copies of the same
   -- capability object. Does not clear cspace slots, release backing, or finish
   -- outstanding catalog reservations. Repeated revocation is rejected.
   procedure Revoke
     (S : in out State; Table : Capabilities.CapabilityTable;
      Slot_Number : Unsigned_64; Accepted : out Boolean) with
     Post => Accepted = Can_Revoke (S'Old, Table, Slot_Number)
       and then Count (S) = Count (S'Old)
       and then (if Accepted then
         not Live (S, Table (Capabilities.CapabilitySlot (Slot_Number)).object.ref)
         and then (for all H in 1 .. Handle (Count (S)) =>
           (if Descends_From (S'Old, H,
             Table (Capabilities.CapabilitySlot (Slot_Number)).object.ref)
            then not Live (S, H)))
         else S = S'Old);
   function Can_Access
     (S : State;
      Table : Capabilities.CapabilityTable; Slot_Number : Unsigned_64;
      For_Write : Boolean) return Boolean with
     Post => (if Can_Access'Result then Identity (S) /= 0
       and then Slot_Number <= Unsigned_64 (Capabilities.CapabilitySlot'Last)
       and then Table (Capabilities.CapabilitySlot (Slot_Number)).capType =
         Capabilities.CAP_HARDWARE_REGISTER
       and then Table (Capabilities.CapabilitySlot (Slot_Number)).gen =
         Capabilities.INITIAL_GENERATION
       and then Table (Capabilities.CapabilitySlot (Slot_Number)).object.param = Identity (S)
       and then Table (Capabilities.CapabilitySlot (Slot_Number)).rights
         (if For_Write then Capabilities.RIGHT_WRITE else Capabilities.RIGHT_READ)
       and then Live (S, Table (Capabilities.CapabilitySlot (Slot_Number)).object.ref));
   -- No pointer, address, width, offset, permission record or raw grant handle
   -- is accepted from the request. Only a slot, operation and value. Serialize
   -- cspace/registry/catalog authorization and reservation as one operation.
   procedure Begin_Access
     (S : State;
      Table : Capabilities.CapabilityTable; Slot_Number : Unsigned_64;
      Catalog : in out Hardware_Catalog.State;
      For_Write : Boolean; Value : Unsigned_64;
      Result : out Hardware_Catalog.Decision; Ticket : out Unsigned_64) with
     Pre => not Result'Constrained,
     Post => (if Result.Allowed then
       Can_Access (S, Table, Slot_Number, For_Write)
       and then Hardware_Catalog.Busy (Catalog)
       and then not Hardware_Catalog.Busy (Catalog'Old)
       and then Ticket = Hardware_Catalog.Receipt (Catalog) and then Ticket /= 0
       else Catalog = Catalog'Old and then Ticket = 0);
end Hardware_Grants.Cspace;
