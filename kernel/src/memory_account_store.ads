with DMA_Record_Blocks;
with Memory_Identity_Map;
with Interfaces; use Interfaces;
with Process_Memory_Accounts;
with Process_Memory_Budget;
with System;
with System.Storage_Elements;

-- Internal owner-account storage, independent of PID reuse. Caller serializes
-- ALL operations and retains the Store and its allocated blocks permanently.
-- Allocate must return fresh disjoint storage. No scans or fixed record cap.
-- Handles are kernel-only, not user-supplied addresses or public authority.
generic
   with function Allocate
     (Bytes, Alignment : System.Storage_Elements.Storage_Count)
      return System.Address;
   -- Only dynamic index pages are released; record blocks remain a cache.
   with procedure Release_Index_Page (Page : System.Address);
package Memory_Account_Store is
   type Store is limited private;
   type Handle is private;
   No_Account : constant Handle;
   type Open_Result is (Opened, Metadata_Limit, No_Memory,
                        Invalid_Backing, Identity_Exhausted);
   function Valid (Object : Store; Item : Handle) return Boolean;
   -- Identities are unique within one Store. Native routing must use one
   -- authoritative store, not select a store by a current process PID.
   function Identity (Object : Store; Item : Handle) return Unsigned_64;
   function Resolve (Object : Store; Token : Unsigned_64) return Handle;
   -- Physical charge tags encode an account incarnation and charge kind.
   -- Zero is never a charge. Tag 3 identifies retained DMA separately. The finite
   -- identity domain fails closed rather than wrapping into another owner.
   Max_Identity : constant Unsigned_64 := Unsigned_64'Last / 4;
   function Charge_Identity
     (Object : Store; Item : Handle; Kind : Process_Memory_Budget.Charge_Kind)
      return Unsigned_64;
   procedure Refund_Physical
     (Object : in out Store; Charge : Unsigned_64; Pages : Unsigned_64;
      OK : out Boolean);
   function Metadata_Bytes (Object : Store) return Unsigned_64;
   function Block_Bytes return Unsigned_64;
   procedure Open
     (Object : in out Store; Byte_Limit : Unsigned_64;
      Item : out Handle; Status : out Open_Result);
   procedure Inspect
     (Object : Store; Item : Handle; Live : out Boolean;
      Pages, Limit : out Unsigned_64; OK : out Boolean);
   procedure Adopt
     (Object : in out Store; Item : Handle; Pages : Unsigned_64;
      OK : out Boolean);
   procedure Reserve
     (Object : in out Store; Item : Handle;
      Kind : Process_Memory_Budget.Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean);
   -- Close denies further reservations but preserves all existing charges.
   -- Empty closed accounts release their SLOT, not the backing metadata block.
   procedure Close
     (Object : in out Store; Item : in out Handle; OK : out Boolean);
   procedure Refund
     (Object : in out Store; Item : in out Handle;
      Kind : Process_Memory_Budget.Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean);
private
   type Account_Entry is record
      Data : Process_Memory_Accounts.Account;
      Allocated : Boolean := False;
   end record;
   package Records is new DMA_Record_Blocks
     (Account_Entry, 16, Allocate);
   function Allocate_Index_Page return System.Address;
   package Indexes is new Memory_Identity_Map
     (Allocate_Index_Page, Release_Index_Page);
   type Store is limited record
      Pool : Records.Pool;
      Index : Indexes.Map;
      Free : Records.List;
      Last_Identity : Unsigned_64 := 0;
   end record;
   type Handle is record
      Ref : Records.Reference := null;
      Token : Unsigned_64 := 0;
      Owner : System.Address := System.Null_Address;
   end record;
   No_Account : constant Handle := (others => <>);
end Memory_Account_Store;
