with Interfaces; use Interfaces;
with Intel_GPU_Record_Store;
package Intel_GPU_Client_Budgets is
   -- Trusted serialized accounting, not an IPC authorization boundary.
   -- Session must be the authenticated, never-reused render-session identity.
   -- This ledger owns no GPU VA, physical memory, or allocation tickets.
   type Ledger is limited private;
   Max_Probes : constant := 65;
   function Capacity (Object : Ledger) return Positive;
   procedure Extend
     (Object : in out Ledger; Base, Bytes : Unsigned_64; Accepted : out Boolean);
   -- Stable owned CPU metadata only, same lifetime contract as Record_Store.
   -- Open never reopens or reassigns an existing session, including after close.
   procedure Open
     (Object : in out Ledger; Session, Limit : Unsigned_64; Accepted : out Boolean);
   type Usage is record
      Known, Closed : Boolean := False;
      Limit, Charged : Unsigned_64 := 0;
   end record;
   function Snapshot (Object : Ledger; Session : Unsigned_64) return Usage;
   procedure Reserve
     (Object : in out Ledger; Session, Bytes : Unsigned_64; Accepted : out Boolean);
   -- Trusted coordinator ONLY after the exact allocation ticket's successful
   -- retirement transition. This arithmetic layer does not validate tickets:
   -- calling twice for one ticket is a caller bug, not a second refund right.
   -- Closed accounts may drain; quarantined ledgers retain every charge.
   procedure Release_Confirmed
     (Object : in out Ledger; Session, Bytes : Unsigned_64;
      Confirmed : Boolean; Accepted : out Boolean);
   procedure Close (Object : in out Ledger; Session : Unsigned_64);
   procedure Quarantine (Object : in out Ledger);
   function Last_Probes (Object : Ledger) return Natural;
private
   type Entry_State is record
      Session, Limit, Charged : Unsigned_64 := 0;
      Left, Right : Natural := 0;
      Closed : Boolean := False;
   end record;
   package Entries is new Intel_GPU_Record_Store (Entry_State, (others => <>));
   type Ledger is limited record
      Items : Entries.Store;
      Count, Root : Natural := 0;
      Failed : Boolean := False;
      Probes : Natural range 0 .. Max_Probes := 0;
   end record;
end Intel_GPU_Client_Budgets;
