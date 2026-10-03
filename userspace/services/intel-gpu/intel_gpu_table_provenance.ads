with Interfaces; use Interfaces;
with Intel_GPU_Record_Store;
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
      Cursor : Positive := 1;
      Items : Records.Store;
   end record;
end Intel_GPU_Table_Provenance;
