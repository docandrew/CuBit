with Interfaces;
with CCL.Types;

-- Host-owned references, not kernel capabilities and not persistable data.
-- Single-owner: the host serializes access and assigns each registry a distinct
-- context for as long as any reference/ticket from it can exist. Context IDs
-- are not permissions. Never reconstruct/reset a registry under an old context.
package CCL.Resources with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   use type CCL.Types.Type_Reference;
   Maximum_Resources : constant := 32;
   subtype Context_ID is Interfaces.Unsigned_64 range 1 .. Interfaces.Unsigned_64'Last;
   subtype Slot is Natural range 0 .. Maximum_Resources;
   subtype Occupied_Slot is Slot range 1 .. Maximum_Resources;
   type Run is private;
   No_Run : constant Run;
   type Reference is private;
   No_Reference : constant Reference;
   type Ticket is private;
   No_Ticket : constant Ticket;
   type Registry (Context : Context_ID) is limited private;
   type Outcome is
     (Succeeded, Stale_Run, Stale_Reference, Stale_Ticket, Wrong_Type,
      Busy, Capacity_Exhausted, Identity_Exhausted, Invalid_Type, Not_Ready);
   type Slot_Phase is (Vacant, Acquiring, Available, In_Flight, Retiring);

   function Active (Owner : Registry; Session : Run) return Boolean;
   function Phase (Owner : Registry; Position : Occupied_Slot) return Slot_Phase;
   function Pending (Owner : Registry; Position : Occupied_Slot) return Boolean;
   function Empty (Owner : Registry) return Boolean;
   function Declared_Types (Owner : Registry) return CCL.Types.Registry;
   -- Host metadata pinned by Start, not a script discovery/authority API.
   function Current (Owner : Registry; Item : Reference) return Boolean;
   function Type_Of (Owner : Registry; Item : Reference) return CCL.Types.Type_Reference;
   function Matches_Type
     (Owner : Registry; Item : Reference; Types : CCL.Types.Registry;
      Expected : CCL.Types.Type_Reference) return Boolean with Global => null;
   -- Compare a live reference's complete nominal definition, not process-local
   -- type numbers. This validates metadata/lifetime, not permission to invoke.
   function Valid_Ticket (Owner : Registry; Call : Ticket) return Boolean;
   -- Host-only lookup for its stable backing array. No script integer->ref
   -- conversion exists. Callers must retain the same registry/run/type snapshot.
   function Position_Of (Owner : Registry; Call : Ticket) return Slot;

   procedure Start
     (Owner : in out Registry; Types : CCL.Types.Registry;
      Session : out Run; Result : out Outcome)
     with Post => (if Result = Succeeded then Active (Owner, Session) else Session = No_Run);
   -- Only while stopped and completely drained. Types are pinned for the run.
   procedure Stop (Owner : in out Registry; Session : Run; Result : out Outcome)
     with Post => (if Result = Succeeded then not Active (Owner, Session) and
       (for all I in Occupied_Slot => Phase (Owner, I) in Vacant | Retiring));
   -- Invalidates script access immediately; does not cancel calls or free slots.

   procedure Reserve
     (Owner : in out Registry; Session : Run; Kind : CCL.Types.Type_Reference;
      Call : out Ticket; Result : out Outcome)
     with Post => (if Result = Succeeded then Valid_Ticket (Owner, Call) else Call = No_Ticket);
   -- Reserve BEFORE initiating an externally visible factory operation.
   -- Describing a resource type does not authorize such an operation.
   procedure Publish
     (Owner : in out Registry; Call : Ticket; Acquired : Boolean;
      Item : out Reference; Result : out Outcome)
     with Post => (if Result = Succeeded then Current (Owner, Item) else Item = No_Reference) and
       (if Result /= Stale_Ticket then not Valid_Ticket (Owner, Call));
   -- Host calls only after validating/draining the factory completion. Failed
   -- or stopped acquisitions retire, even if the provider created a resource.
   -- The host then closes/retires backing state before Reclaim.

   procedure Begin_Use
     (Owner : in out Registry; Item : Reference; Expected : CCL.Types.Type_Reference;
      Call : out Ticket; Result : out Outcome)
     with Post => (if Result = Succeeded then Valid_Ticket (Owner, Call) else Call = No_Ticket);
   -- One outstanding operation per resource; a fresh ticket for every use.
   procedure Finish_Use
     (Owner : in out Registry; Call : Ticket; Keep : Boolean; Result : out Outcome)
     with Post => (if Result = Succeeded then not Valid_Ticket (Owner, Call));
   -- Keep=False after close, transport uncertainty, or other terminal use.
   -- A completion can never revive a stopped or explicitly retired resource.
   procedure Begin_Cleanup
     (Owner : in out Registry; Lease : Ticket; Call : out Ticket; Result : out Outcome)
     with Post => (if Result = Succeeded then Valid_Ticket (Owner, Call) else Call = No_Ticket);
   -- Host-only asynchronous cleanup (e.g. close after a stopped Create).
   -- Requires a retiring lease with no outstanding call. Finish_Use cannot
   -- make this lease script-accessible, even if Keep=True is supplied.
   procedure Retire (Owner : in out Registry; Item : Reference; Result : out Outcome)
     with Post => (if Result = Succeeded then not Current (Owner, Item));
   procedure Retire_Lease (Owner : in out Registry; Lease : Ticket; Result : out Outcome)
     with Post => (if Result = Succeeded then Position_Of (Owner, Lease) /= 0 and then
       Phase (Owner, Position_Of (Owner, Lease)) = Retiring);
   -- Host stop of one resource, including an acquisition not yet published.
   -- Keeps the outstanding ticket valid so its completion can still be drained.
   procedure Reclaim
     (Owner : in out Registry; Call : Ticket; Result : out Outcome)
     with Post => (if Result = Succeeded then Position_Of (Owner, Call) = 0 and
       not Valid_Ticket (Owner, Call));
   -- Host-only, AFTER backing handle cleanup and confirmed grant retirement.
   -- Rejects outstanding calls. This pure registry cannot verify external IPC
   -- or DMA quiescence; the platform adapter must establish those facts.

private
   subtype Serial is Interfaces.Unsigned_64;
   type Run is record
      Context, Number : Serial := 0;
   end record;
   No_Run : constant Run := (others => <>);
   type Reference is record
      Session : Run;
      Position : Slot := 0;
      Lease : Serial := 0;
   end record;
   No_Reference : constant Reference := (others => <>);
   type Call_Kind is (Factory, Operation);
   type Ticket is record
      Item : Reference;
      Number : Serial := 0;
      Kind : Call_Kind := Factory;
   end record;
   No_Ticket : constant Ticket := (others => <>);
   type Resource_Entry is record
      Current : Slot_Phase := Vacant;
      Lease, Call_Number : Serial := 0;
      Kind : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Call_Type : Call_Kind := Factory;
   end record;
   type Entry_Array is array (Occupied_Slot) of Resource_Entry;
   type Registry (Context : Context_ID) is limited record
      Running : Boolean := False;
      Session, Issued : Serial := 0;
      Types : CCL.Types.Registry;
      Entries : Entry_Array;
   end record;
   function Active (Owner : Registry; Session : Run) return Boolean is
     (Owner.Running and Session.Context = Owner.Context and
      Session.Number /= 0 and Session.Number = Owner.Session);
   function Phase (Owner : Registry; Position : Occupied_Slot) return Slot_Phase is
     (Owner.Entries (Position).Current);
   function Pending (Owner : Registry; Position : Occupied_Slot) return Boolean is
     (Owner.Entries (Position).Call_Number /= 0);
   function Empty (Owner : Registry) return Boolean is
     (for all E of Owner.Entries => E.Current = Vacant);
   function Declared_Types (Owner : Registry) return CCL.Types.Registry is (Owner.Types);
   function Same_Lease (Owner : Registry; Item : Reference) return Boolean is
     (Item.Session.Context = Owner.Context and then Item.Session.Number /= 0 and then
      Item.Session.Number = Owner.Session and then Item.Position in Occupied_Slot and then
      Item.Lease /= 0 and then Item.Lease = Owner.Entries (Item.Position).Lease and then
      Owner.Entries (Item.Position).Current /= Vacant);
   function Current (Owner : Registry; Item : Reference) return Boolean is
     (Same_Lease (Owner, Item) and then Active (Owner, Item.Session) and then
      Owner.Entries (Item.Position).Current in Available | In_Flight);
   function Type_Of (Owner : Registry; Item : Reference) return CCL.Types.Type_Reference is
     (if Current (Owner, Item) then Owner.Entries (Item.Position).Kind else CCL.Types.Invalid_Type);
   function Valid_Ticket (Owner : Registry; Call : Ticket) return Boolean is
     (Same_Lease (Owner, Call.Item) and then Call.Number /= 0 and then
      Call.Number = Owner.Entries (Call.Item.Position).Call_Number and then
      Call.Kind = Owner.Entries (Call.Item.Position).Call_Type);
   function Position_Of (Owner : Registry; Call : Ticket) return Slot is
     (if Same_Lease (Owner, Call.Item) then Call.Item.Position else 0);
end CCL.Resources;
