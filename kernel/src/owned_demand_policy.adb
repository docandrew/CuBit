package body Owned_Demand_Policy with SPARK_Mode is
   function Check
     (First, Limit, Address : Unsigned_64;
      Mode : Access_Mode; State : Backing_State;
      Write, Execute, Protection_Fault : Boolean;
      Used, Capacity, Quota : Natural) return Decision
   is
   begin
      if not Contains (First, Limit, Address)
        or else not Permitted (Mode, Write, Execute)
        or else Protection_Fault
        or else State in Retiring | Quarantined
      then
         return Denied;
      elsif State = Resident then
         -- Another fault handler may have supplied backing while we waited
         -- for the lock. No new frame or quota is needed to retry the access.
         return Retry_Resident;
      elsif Used >= Capacity then
         return Tracking_Full;
      elsif Quota /= 0 and then Used >= Quota then
         return Quota_Full;
      else
         return Needs_Backing;
      end if;
   end Check;
end Owned_Demand_Policy;
