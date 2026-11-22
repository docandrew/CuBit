with Interfaces; use Interfaces;
with Process_Memory_Budget;

-- Serialized owner-incarnation policy. The adapter retains each account
-- outside the resettable process table and serializes access. Identity values
-- must come from a unique, non-wrapping kernel-only allocator, not user PIDs.
package Process_Memory_Accounts with SPARK_Mode, Pure is
   type Account is private;
   function Identity (Object : Account) return Unsigned_64;
   function Active (Object : Account) return Boolean;
   function Used (Object : Account) return Unsigned_64;
   function Limit (Object : Account) return Unsigned_64;
   function Reusable (Object : Account) return Boolean is
     (not Active (Object) and then Used (Object) = 0);

   -- Reuse only after every charge has retired. Increasing identities prevent
   -- stale callbacks matching a later life in this storage slot.
   procedure Open
     (Object : in out Account; New_Identity : Unsigned_64; OK : out Boolean)
     with Post =>
       OK = (Reusable (Object'Old) and then New_Identity > Identity (Object'Old)) and then
       (if OK then Active (Object) and Identity (Object) = New_Identity and
          Used (Object) = 0 and Limit (Object) = 0
        else Object = Object'Old);
   procedure Close
     (Object : in out Account; Expected : Unsigned_64; OK : out Boolean)
     with Post =>
       OK = (Active (Object'Old) and then Expected = Identity (Object'Old)) and then
       Used (Object) = Used (Object'Old) and then
       Identity (Object) = Identity (Object'Old) and then
       (if OK then not Active (Object) else Object = Object'Old);
   procedure Adopt
     (Object : in out Account; Expected, Pages : Unsigned_64; OK : out Boolean)
     with Post => Identity (Object) = Identity (Object'Old) and
       Active (Object) = Active (Object'Old) and Used (Object) = Used (Object'Old) and
       (if OK then Active (Object) and Expected = Identity (Object) and
          Limit (Object) = Pages
        else Object = Object'Old);
   procedure Reserve
     (Object : in out Account; Expected : Unsigned_64;
      Kind : Process_Memory_Budget.Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean)
     with Post => Identity (Object) = Identity (Object'Old) and
       Active (Object) = Active (Object'Old) and
       (if OK then Active (Object) and Expected = Identity (Object) and
          Used (Object) = Used (Object'Old) + Pages
        else Object = Object'Old);
   -- Works after Close; never resolves an identity through the current PID.
   -- Exactly-once physical retirement remains the callback adapter's duty.
   procedure Refund
     (Object : in out Account; Expected : Unsigned_64;
      Kind : Process_Memory_Budget.Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean)
     with Post => Identity (Object) = Identity (Object'Old) and
       Active (Object) = Active (Object'Old) and
       (if OK then Expected /= 0 and Expected = Identity (Object) and
          Used (Object) = Used (Object'Old) - Pages
        else Object = Object'Old);
private
   type Account is record
      Token : Unsigned_64 := 0;
      Live : Boolean := False;
      Budget : Process_Memory_Budget.Ledger;
   end record;
   function Identity (Object : Account) return Unsigned_64 is (Object.Token);
   function Active (Object : Account) return Boolean is (Object.Live);
   function Used (Object : Account) return Unsigned_64 is
     (Process_Memory_Budget.Used (Object.Budget));
   function Limit (Object : Account) return Unsigned_64 is
     (Process_Memory_Budget.Limit (Object.Budget));
end Process_Memory_Accounts;
