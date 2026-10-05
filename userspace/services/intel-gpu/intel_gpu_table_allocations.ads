with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Record_Store;
-- Serialized driver-private authority, never an application-supplied view.
-- Backing and its borrowed extent directory outlive the entry until confirmed
-- retirement. Metadata growth does not allocate or publish GPU page tables.
generic
   -- Must authenticate the supervisor-backed table-only allocation and its
   -- canonical slot, not just membership of an arbitrary client BO ticket.
   with function Admitted (Session, Ticket : Unsigned_64; Slot : Positive) return Boolean;
   with function Retirement_Confirmed (Session, Ticket : Unsigned_64) return Boolean;
package Intel_GPU_Table_Allocations is
   type Allocation_Role is (Replacement_Image, Incremental_Tables);
   type Registry is limited private;
   function Capacity (Object : Registry) return Positive;
   function Revision (Object : Registry) return Unsigned_64;
   type Retained_Allocation (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True =>
            Session, Ticket : Unsigned_64;
            Role : Allocation_Role;
            Revoked : Boolean;
      end case;
   end record;
   function Retained_At (Object : Registry; Slot : Positive) return Retained_Allocation;
   -- Observation remains available after revocation/ownership loss; no backing
   -- address, admission or retirement authority is granted by this identity.
   procedure Check_Retained_Range
     (Object : Registry; Slot : Positive;
      Session, Ticket, Expected_Revision, First, Bytes : Unsigned_64;
      Overlaps, Accepted : out Boolean);
   -- Cleanup observation only: compare a DMA range against the exact retained
   -- allocation, including revoked entries. No CPU/DMA address is returned and
   -- no live admission or release authority is granted. Unknown identity,
   -- stale registry revision or invalid range returns Overlaps=True/Accepted=False.
   procedure Scan_Session
     (Object : Registry; Session, Expected_Revision : Unsigned_64; First : Positive;
      Found : out Boolean; Next : out Natural; Accepted : out Boolean);
   -- At most64 entries. For Accepted and not Found, Next=0 completes this
   -- census; otherwise resume at Next with the SAME revision. Mutations reject
   -- stale continuations. A negative census is valid only while that revision
   -- remains current. Metadata extension alone adds empty entries.
   procedure Extend
     (Object : in out Registry; Base, Bytes : Unsigned_64; Accepted : out Boolean);
   procedure Install
     (Object : in out Registry; Slot : Positive; Session, Ticket : Unsigned_64;
      Role : Allocation_Role; Backing : Intel_GPU_Buffer_Reply.Backing;
      Accepted : out Boolean);
   procedure Lookup
     (Object : Registry; Slot : Positive; Session, Ticket : Unsigned_64;
      Role : Allocation_Role;
      Backing : out Intel_GPU_Buffer_Reply.Backing; Accepted : out Boolean);
   -- Superseding a replacement forbids new resolution but retains ownership
   -- until the exact supervisor retirement acknowledgment arrives.
   procedure Revoke
     (Object : in out Registry; Slot : Positive; Session, Ticket : Unsigned_64;
      Accepted : out Boolean);
   procedure Retire
     (Object : in out Registry; Slot : Positive; Session, Ticket : Unsigned_64;
      Accepted : out Boolean);
private
   type Entry_Record is record
      Session, Ticket : Unsigned_64 := 0;
      Role : Allocation_Role := Replacement_Image;
      Revoked : Boolean := False;
      Backing : Intel_GPU_Buffer_Reply.Backing;
   end record;
   package Entries is new Intel_GPU_Record_Store (Entry_Record, (others => <>));
   type Registry is limited record
      Items : Entries.Store;
      Epoch : Unsigned_64 := 1;
   end record;
end Intel_GPU_Table_Allocations;
