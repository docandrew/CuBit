pragma Ada_2022;
with Interfaces; use Interfaces;
with Hardware_Authority;
with Hardware_Catalog;
-- Kernel-owned records. Handles must come from an authenticated cspace entry,
-- never directly from a wire argument. Serialize all operations with catalog
-- access. No record reuse: exhaustion fails closed, including after revocation.
package Hardware_Grants with SPARK_Mode,
  Abstract_State => Identity_Allocator,
  Initializes => Identity_Allocator is
   use type Hardware_Catalog.Decision;
   use type Hardware_Catalog.State;
   Maximum_Grants : constant := 128;
   subtype Handle is Unsigned_64;
   No_Grant : constant Handle := 0;
   type State is private;
   function Count (S : State) return Natural;
   function Identity (S : State) return Unsigned_64;
   function Issued_Count return Unsigned_64 with Ghost,
     Global => (Input => Identity_Allocator);
   -- Trusted initialization, serialized across all registries. Identities never
   -- wrap or recycle. A live registry must not be reset/copied into a new owner.
   procedure Initialize (S : in out State; Accepted : out Boolean) with
     Global => (In_Out => Identity_Allocator),
     Post => (Issued_Count'Old < Unsigned_64'Last or not Accepted)
       and (Issued_Count = Issued_Count'Old + Boolean'Pos (Accepted))
       and (if Accepted then Identity (S'Old) = 0 and then Count (S) = 0
         and then Identity (S) = Issued_Count and then Identity (S) /= 0
         else S = S'Old);
   function Live (S : State; H : Handle) return Boolean with
     Post => (if Live'Result then H in 1 .. Handle (Count (S))
       and then H <= Handle (Maximum_Grants));
   function Descends_From (S : State; Child, Parent : Handle) return Boolean;
   -- Trusted boot admission only. This API is not exposed to userspace.
   procedure Admit_Group
     (S : in out State; Catalog : Hardware_Catalog.State;
      Group : Hardware_Catalog.Resource_Group; H : out Handle) with
     Post => (if H /= No_Grant then Live (S, H)
       and then H = Handle (Count (S)) and then Count (S) = Count (S'Old) + 1
       else S = S'Old);
   -- Parent must be authenticated by the kernel, with RIGHT_GRANT checked by
   -- the caller. Requested is untrusted desired authority, checked here.
   procedure Derive
     (S : in out State; Catalog : Hardware_Catalog.State; Parent : Handle;
      Requested : Hardware_Authority.Permission; H : out Handle) with
     Post => (if H /= No_Grant then Live (S, H)
       and then Count (S) = Count (S'Old) + 1
       and then Hardware_Authority.Valid (Requested)
       and then Descends_From (S, H, Parent)
       and then (for all I in 1 .. Handle (Count (S'Old)) =>
         (if Descends_From (S'Old, Parent, I) then Descends_From (S, H, I)))
       else S = S'Old);
   procedure Revoke (S : in out State; H : Handle) with
     Post => not Live (S, H) and then Count (S) = Count (S'Old)
       and then (for all I in 1 .. Handle (Count (S)) =>
         (if Descends_From (S'Old, I, H) then not Live (S, I)
          elsif I /= H then Live (S, I) = Live (S'Old, I)));
   -- Returns catalog metadata internally. Does not reserve backing or do I/O.
   -- Use the grant-aware Begin_Access below before executing a transaction.
   -- Groups cannot directly perform access.
   function Resolve
     (S : State; Catalog : Hardware_Catalog.State; H : Handle;
      For_Write : Boolean; Value : Unsigned_64) return Hardware_Catalog.Decision
     with Post => (if Resolve'Result.Allowed then Live (S, H)
       and then Hardware_Catalog.Active (Catalog)
       and then Hardware_Catalog.Valid (Resolve'Result.Resource));
   -- Authenticate H in cspace and intersect operation rights before calling.
   -- Hold registry/catalog serialization through authorization and reservation.
   -- The returned ticket is kernel-private; trusted completion goes directly to
   -- Hardware_Catalog.Finish_Access. Revocation cannot release busy backing.
   procedure Begin_Access
     (S : State; Catalog : in out Hardware_Catalog.State; H : Handle;
      For_Write : Boolean; Value : Unsigned_64;
      Result : out Hardware_Catalog.Decision; Ticket : out Unsigned_64) with
     Pre => not Result'Constrained,
     Post => Hardware_Catalog.Epoch (Catalog) = Hardware_Catalog.Epoch (Catalog'Old)
       and then Hardware_Catalog.Active (Catalog) = Hardware_Catalog.Active (Catalog'Old)
       and then Hardware_Catalog.Count (Catalog) = Hardware_Catalog.Count (Catalog'Old)
       and then (if Result.Allowed then Live (S, H)
         and then Result = Resolve (S, Catalog'Old, H, For_Write, Value)
         and then not Hardware_Catalog.Busy (Catalog'Old)
         and then Hardware_Catalog.Busy (Catalog)
         and then Ticket = Hardware_Catalog.Receipt (Catalog) and then Ticket /= 0
       else Catalog = Catalog'Old and then Ticket = 0);
private
   subtype Slot is Positive range 1 .. Maximum_Grants;
   type Ancestors is array (Slot) of Boolean with Pack;
   type Grant is record
      Enabled : Boolean := False;
      Is_Group : Boolean := False;
      Group : Hardware_Catalog.Resource_Group := Hardware_Catalog.ACPI;
      Epoch : Unsigned_64 := 0;
      Permission : Hardware_Authority.Permission;
      Parents : Ancestors := [others => False];
   end record;
   type Entries is array (Slot) of Grant;
   type State is record
      Lifetime_ID : Unsigned_64 := 0;
      Used : Natural range 0 .. Maximum_Grants := 0;
      Items : Entries;
   end record with Type_Invariant =>
     (if State.Used > 0 then State.Lifetime_ID /= 0) and then
     (for all I in 1 .. State.Used => State.Items (I).Epoch /= 0
       and then (State.Items (I).Is_Group or else
         Hardware_Authority.Valid (State.Items (I).Permission))
       and then (for all J in I .. Maximum_Grants => not State.Items (I).Parents (J)));
end Hardware_Grants;
