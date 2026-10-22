with AML_Scalar_Ledger;
with AML_Execution_Limits;
private generic
   Slot_Capacity : AML_Execution_Limits.Scalar_Transport_Capacity;
   Object_Capacity : AML_Execution_Limits.Scalar_Transport_Capacity;
   Generation_Limit : Positive := Natural'Last;
   Slot_Stamp_Limit : Positive := Natural'Last;
package AML_Execute.Scalar_Transport with SPARK_Mode, Pure is
   -- Internal storage slots own leases. Ada snapshots and GC projections borrow.
   subtype Slot is Positive range 1 .. Slot_Capacity;
   type Identity_Kind is (Non_Scalar, Temporary_Scalar, Owned_Scalar);
   type Outcome is (Ready, Invalid_Source, Capacity_Limit, Generation_Exhausted);
   type State is private;
   subtype Slot_Locator is Natural range 0 .. Slot_Capacity;
   type Slot_Identity is private;
   No_Slot_Identity : constant Slot_Identity;
   -- Census identity, never an object credential. No-domain states cannot issue
   -- identities, even when they contain non-scalar or owned-source payloads.
   function Current_Slot_Identity (Store : State; At_Slot : Slot) return Slot_Identity;
   function Matches_Slot (Store : State; Item : Slot_Identity) return Boolean;
   -- Safe for No_Slot_Identity; the returned locator is zero in that case.
   function Position (Item : Slot_Identity) return Slot_Locator;
   function Empty (Domain : AML_Frame_Handles.Invocation_Domain) return State;
   function Valid (Store : State) return Boolean;
   function Issued_Stamps (Store : State) return Natural with Ghost;
   function Initialized
     (Store : State; Domain : AML_Frame_Handles.Invocation_Domain) return Boolean with Ghost;
   -- Fresh invocation only, never a same-domain reset of live/stale handles.
   procedure Initialize
     (Store : out State; Domain : AML_Frame_Handles.Invocation_Domain)
     with Post => Valid (Store) and then Initialized (Store, Domain);
   function Occupied (Store : State; At_Slot : Slot) return Boolean;
   function Kind (Store : State; At_Slot : Slot) return Identity_Kind;
   -- Temporary scalars refresh from the ledger. Owned scalar snapshots are
   -- caches: their owner must authenticate/read Source before numeric use.
   function Snapshot (Store : State; At_Slot : Slot) return Datum;
   function Source (Store : State; At_Slot : Slot) return AML_References.Object_Handle;
   function Same_Identity (Store : State; Left, Right : Slot) return Boolean;
   function Temporary_Owners (Store : State; At_Slot : Slot) return Natural;
   -- Counts only distinct canonical transport leases. Ready/False says nothing
   -- about persistent or compound/reference owners outside this transport.
   -- Authenticate the census stamp; object authority remains the owner's job.
   procedure Other_Owned_Lease
     (Store : State; Item : Slot_Identity; Shared : out Boolean;
      Status : out Outcome)
     with Pre => Valid (Store),
       Post => (if not Matches_Slot (Store, Item) then
                   Status = Invalid_Source and then not Shared
                elsif Kind (Store, Position (Item)) /= Owned_Scalar then
                   Status = Invalid_Source and then not Shared
                else Status = Ready and then Shared =
                  (for some Index in Slot => Index /= Position (Item)
                   and then Occupied (Store, Index)
                   and then Kind (Store, Index) = Owned_Scalar
                   and then Same_Identity (Store, Position (Item), Index)));

   -- Enumerate canonical compound/reference/metadata leases only. Borrowed
   -- GC projections and Ada snapshots are not inputs. Visitor authenticates
   -- each value and must not mutate Store or the owner arena, allocate or reenter.
   generic
      with procedure Visit (Value : Datum; Accepted : out Boolean);
   procedure Visit_Compound_Owners
     (Store : State; Excluded : Slot_Identity; Status : out Outcome)
     with Pre => Valid (Store);

   -- This is NOT the ACPICA owner-backed reference count. No such query exists
   -- until named/package ownership multiplicities have a justified model.
   function Temporary_Roots (Store : State) return Natural;
   -- Mutate the temporary object, preserving every slot/lease identity.
   procedure Normalize_Temporary_Result
     (Store : in out State; At_Slot : Slot; Width : AML_Decode.Integer_Width;
      Status : out Outcome)
     with Pre => Valid (Store), Post => Valid (Store);
   procedure Put
     (Store : in out State; At_Slot : Slot; Value : Datum; Status : out Outcome)
     with Pre => Valid (Store), Post => Valid (Store);
   -- Carries an existing opaque credential, never authenticates or manufactures
   -- owner authority. Owner validation is mandatory at admission/read/publication.
   procedure Put_Owned
     (Store : in out State; At_Slot : Slot; Value : AML_Decode.Integer_Value;
      Origin : AML_Decode.Integer_Origin; Identity : AML_References.Object_Handle;
      Status : out Outcome) with Pre => Valid (Store), Post => Valid (Store);
   -- Read-only owner callback authenticates Source before this cache update.
   -- This changes no ownership, slot stamp, object identity or ledger lease.
   procedure Refresh_Owned_Cache
     (Store : in out State; At_Slot : Slot; Value : AML_Decode.Integer_Value;
      Origin : AML_Decode.Integer_Origin)
     with Pre => Valid (Store) and then Occupied (Store, At_Slot)
       and then Kind (Store, At_Slot) = Owned_Scalar,
       Post => (Current_Slot_Identity (Store, At_Slot) = Current_Slot_Identity (Store, At_Slot)'Old
         and AML_References."=" (Source (Store, At_Slot), Source (Store, At_Slot)'Old))
         and then Valid (Store);
   -- Descriptor identity only, never owner authentication. A caller must
   -- authenticate Value through its owner before refreshing these cached fields.
   function Compound_Identity_Matches
     (Store : State; At_Slot : Slot; Value : Object_Value) return Boolean;
   function Compound_Cache_Updated
     (Store, Prior : State; At_Slot : Slot; Value : Object_Value) return Boolean with Ghost;
   procedure Refresh_Compound_Cache
     (Store : in out State; At_Slot : Slot; Value : Object_Value;
      Status : out Outcome)
     with Pre => Valid (Store), Post => Valid (Store)
       and then (if Status = Ready then Compound_Cache_Updated (Store, Store'Old, At_Slot, Value)
                 else Status = Invalid_Source and then Store = Store'Old);
   procedure Share
     (Store : in out State; From, Into : Slot; Status : out Outcome)
     with Pre => Valid (Store), Post => Valid (Store);
   procedure Move
     (Store : in out State; From, Into : Slot; Status : out Outcome)
     with Pre => Valid (Store), Post => Valid (Store);
   procedure Clear (Store : in out State; At_Slot : Slot)
     with Pre => Valid (Store), Post => Valid (Store);
   function Range_Cleared
     (Store, Prior : State; First, Last : Slot) return Boolean with Ghost;
   -- First > Last is the empty range. Bounds always remain typed slots.
   procedure Clear_Range (Store : in out State; First, Last : Slot)
     with Pre => Valid (Store), Post => Valid (Store)
       and then Range_Cleared (Store, Store'Old, First, Last);
   procedure Clear_All (Store : in out State)
     with Pre => Valid (Store),
       Post => Issued_Stamps (Store) = Issued_Stamps (Store)'Old
         and then Valid (Store) and then Temporary_Roots (Store) = 0;
   -- Publication only: truthful Integer Type_Code=1 carrier for existing
   -- namespace Value_Root. Never evaluate/materialize/return this carrier.
   function Root_Projection (Store : State; At_Slot : Slot) return Datum;
private
   type Stamp_Value is new Natural;
   subtype Slot_Stamp is Stamp_Value range 0 .. Stamp_Value (Slot_Stamp_Limit);
   No_Stamp : constant Slot_Stamp := 0;
   type Slot_Identity is record
      Domain : AML_Frame_Handles.Invocation_Domain := AML_Frame_Handles.No_Domain;
      At_Slot : Slot_Locator := 0;
      Stamp : Slot_Stamp := No_Stamp;
   end record;
   No_Slot_Identity : constant Slot_Identity := (others => <>);
   Staging_Root_Count : constant Positive := 1;
   Staging_Object_Count : constant Positive := 1;
   package Ledger is new AML_Scalar_Ledger
     (Object_Capacity + Staging_Object_Count,
      Slot_Capacity + Staging_Root_Count, Generation_Limit);
   type Envelope is record
      Present : Boolean := False;
      Stamp : Slot_Stamp := No_Stamp;
      Payload : Datum := (Integer_Datum, 0, AML_Decode.Ordinary_Integer);
      Tag : Identity_Kind := Non_Scalar;
      Lease : Ledger.Handle := Ledger.No_Handle;
      Owned : AML_References.Object_Handle := AML_References.No_Object_Handle;
   end record;
   type Slot_Table is array (Slot) of Envelope;
   type State is record
      Scalars : Ledger.State;
      Last_Stamp : Slot_Stamp := No_Stamp;
      Slots : Slot_Table := [others => <>];
   end record;
end AML_Execute.Scalar_Transport;
