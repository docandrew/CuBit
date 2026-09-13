package body Page_Admission with SPARK_Mode is
   function Check
     (Stack_First, Stack_Limit, Heap_First, Heap_Limit, Address : Unsigned_64;
      Used, Capacity, Quota : Natural) return Decision
   is
   begin
      if not (Contains (Stack_First, Stack_Limit, Address) or else
              Contains (Heap_First, Heap_Limit, Address)) then
         return Outside_Reservation;
      elsif Used >= Capacity then
         return Tracking_Full;
      elsif Quota /= 0 and then Used >= Quota then
         return Quota_Full;
      else
         return Admitted;
      end if;
   end Check;
end Page_Admission;
