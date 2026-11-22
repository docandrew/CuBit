with Interfaces; use Interfaces;
with Intel_GPU_Record_Store;
generic
   Quota : Positive;
   Bootstrap : Positive := 4;
package Intel_GPU_Table_References is
   pragma Compile_Time_Error (Bootstrap > Quota, "reference bootstrap exceeds quota");
   type Map is limited private;
   function Capacity (Object : Map) return Positive;
   function Generation (Object : Map) return Unsigned_64;
   function Metadata_Bytes return Unsigned_64;
   procedure Extend
     (Object : in out Map; Base, Bytes : Unsigned_64; Accepted : out Boolean);
   -- Stable committed CPU metadata only. Owner retains the reservation;
   -- extension does not change generation, IDs, or backing ownership.
   function Get
     (Object : Map; Expected_Generation : Unsigned_64; Ordinal : Natural)
      return Natural;
   procedure Put
     (Object : in out Map; Expected_Generation : Unsigned_64;
      Ordinal, ID : Natural; Accepted : out Boolean);
   -- Zero means unresolved. A nonzero ID names a record in the accompanying
   -- provenance ledger, NOT a VM ordinal. Caller authenticates that ledger and
   -- serializes installation/adoption; this map does not authorize replacement.
   procedure Reopen
     (Object : in out Map; Expected_Generation, Next_Generation : Unsigned_64;
      Accepted : out Boolean);
   -- O(1) logical reset; exact next generation only, no wrap/replay. Invoke only
   -- after independently confirmed ledger retirement/reopen. Old metadata is
   -- retained but unreadable, not zeroed or released. This is not a retirement
   -- receipt and grants no CPU/GPU address or physical backing reuse authority.
private
   type Entry_Record is record
      Epoch : Unsigned_64 := 0;
      ID : Natural := 0;
   end record;
   package Records is new Intel_GPU_Record_Store
     (Entry_Record, (Epoch => 0, ID => 0), Bootstrap);
   type Map is limited record
      Epoch : Unsigned_64 := 1;
      Items : Records.Store;
   end record;
end Intel_GPU_Table_References;
