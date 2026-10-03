with Interfaces; use Interfaces;

-- Admission only. The caller must hold the address-space/registry locks and
-- authenticate the owning process generation and reservation before calling.
-- This does not publish a PTE, allocate a frame, or authorize discarding one.
package Owned_Demand_Policy with SPARK_Mode, Pure is
   Page_Size : constant Unsigned_64 := 4096;
   User_Limit : constant Unsigned_64 := 16#0000_8000_0000_0000#;
   type Access_Mode is (Guard, Read_Only, Read_Write);
   type Backing_State is (Absent, Resident, Retiring, Quarantined);
   type Decision is
     (Denied, Tracking_Full, Quota_Full, Retry_Resident, Needs_Backing);

   function Contains (First, Limit, Address : Unsigned_64) return Boolean is
     (First /= 0 and then First < Limit and then Limit <= User_Limit
      and then First mod Page_Size = 0 and then Limit mod Page_Size = 0
      and then Address >= First and then Address < Limit);

   function Permitted
     (Mode : Access_Mode; Write, Execute : Boolean) return Boolean is
     (not Execute and then Mode /= Guard
      and then (not Write or else Mode = Read_Write));

   function Check
     (First, Limit, Address : Unsigned_64;
      Mode : Access_Mode; State : Backing_State;
      Write, Execute, Protection_Fault : Boolean;
      Used, Capacity, Quota : Natural) return Decision
   with Post =>
     (if Check'Result in Retry_Resident | Needs_Backing then
        Contains (First, Limit, Address)
        and then Permitted (Mode, Write, Execute)
        and then not Protection_Fault
        and then State in Absent | Resident)
     and then
     (if Check'Result = Needs_Backing then
        State = Absent and then Used < Capacity
        and then (Quota = 0 or else Used < Quota))
     and then
     (if Check'Result = Retry_Resident then State = Resident);
end Owned_Demand_Policy;
