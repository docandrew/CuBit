with Interfaces; use Interfaces;
with Intel_GPU_Record_Store;
with System;
package Intel_GPU_Table_Provenance is
   type Mapping is record
      Ticket, Offset, CPU, DMA : Unsigned_64 := 0;
   end record;
   type Ledger is limited private;
   function Generation (Object : Ledger) return Unsigned_64;
   -- Capture with table IDs; do not refresh a stale reference by querying the
   -- current generation at use time. Generation changes only after complete
   -- acknowledged retirement, never during metadata growth.
   type Retirement_Phase is (Open, Searching, Request_Ready, Awaiting_Ack, Sweeping, Complete, Failed);
   -- Storage is independent of dispatcher callbacks, allowing library-level
   -- context/update records to own it without overlays or service-stack arrays.
   generic
      with procedure Resolve_Owned_Page
        (Session, Ticket, Offset : Unsigned_64;
         CPU, DMA : out Unsigned_64; Accepted : out Boolean);
   package Authority is
   procedure Install
     (Object : in out Ledger; Session, Expected_Generation : Unsigned_64; Index : Positive;
      Ticket, Offset : Unsigned_64; Accepted : out Boolean);
   function Lookup
     (Object : Ledger; Session, Expected_Generation : Unsigned_64; Index : Positive) return Mapping;
   type Append_Phase is (Unused, Appending, Appended, Rejected);
   type Append_State is limited private;
   function Status (Operation : Append_State) return Append_Phase;
   function First_ID (Operation : Append_State) return Natural;
   function Installed (Operation : Append_State) return Natural;
   procedure Begin_Append
     (Operation : in out Append_State; Object : Ledger;
      Session, Expected_Generation, Ticket, Offset : Unsigned_64;
      Pages : Positive; Accepted : out Boolean);
   procedure Step (Operation : in out Append_State; Object : in out Ledger);
   procedure Rearm
     (Operation : in out Append_State; Object : Ledger; Accepted : out Boolean);
   -- Metadata-only controller reuse after Appended, on the exact same open
   -- ledger, owner, generation and completed record count. Preserve every
   -- registered reference. Caller captures First_ID before rearming; the next
   -- append receives new IDs, never overwrites or releases previous backing.
   -- Failed/partial operations cannot rearm. No resolver callback or GPU access;
   -- the next Step must independently authenticate its new allocation.
   -- One attempt per armed operation, one authenticated page per step, no DMA writes. Metadata must
   -- already cover the whole group. Caller serializes mutation and validates
   -- aliases as for Install. Partial failure retains all installed references;
   -- never remove them or free the allocation merely because append failed.
   -- First_ID is exposed only after complete registration, not GPU publication.
   private
      type Append_State is limited record
         Phase : Append_Phase := Unused;
         Ledger_Address : System.Address := System.Null_Address;
         Session, Epoch, Ticket, Offset : Unsigned_64 := 0;
         Before, Pages, Done : Natural := 0;
      end record;
   end Authority;
   procedure Extend
     (Object : in out Ledger; Base, Bytes : Unsigned_64; Accepted : out Boolean);
   function Capacity (Object : Ledger) return Positive;
   function Count (Object : Ledger) return Natural;
   -- Bounded retirement census, NOT permission to release backing. Next=0
   -- means complete. Caller retains exclusion across the complete census.
   procedure Scan_Ticket
     (Object : Ledger; Session, Ticket : Unsigned_64; First : Positive;
      Found : out Boolean; Next : out Natural; Accepted : out Boolean);
   -- Trusted serialized owner installs unique table IDs in order only after
   -- full CPU/DMA alias validation. Callback must authenticate session and
   -- exact allocation generation plus table-only role. Ledger owns no backing;
   -- referenced allocations must remain retained through hardware retirement.
   -- No overwrite within a generation. Retirement.Reopen retains metadata but
   -- advances generation after all retained tickets were acknowledged/cleared.
   -- Grow metadata via the existing stable record store.
private
   package Records is new Intel_GPU_Record_Store (Mapping, (others => 0));
   type Ledger is limited record
      Owner : Unsigned_64 := 0;
      Epoch : Unsigned_64 := 1;
      Used : Natural := 0;
      Phase : Retirement_Phase := Open;
      Pending : Unsigned_64 := 0;
      Last_Ticket : Unsigned_64 := 0;
      Include_Last : Boolean := False;
      Cursor : Positive := 1;
      Items : Records.Store;
   end record;
end Intel_GPU_Table_Provenance;
