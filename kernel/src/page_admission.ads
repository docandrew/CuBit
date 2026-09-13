with Interfaces; use Interfaces;

-- Pure admission for demand-mapped process pages. Resource acquisition and
-- page-table publication are deliberately outside this proof boundary.
package Page_Admission with SPARK_Mode, Pure is
   User_Limit : constant Unsigned_64 := 16#0000_8000_0000_0000#;
   type Decision is (Outside_Reservation, Tracking_Full, Quota_Full, Admitted);

   function Contains (First, Limit, Address : Unsigned_64) return Boolean is
     (First < Limit and then Limit <= User_Limit and then
      Address >= First and then Address < Limit);

   function Check
     (Stack_First, Stack_Limit, Heap_First, Heap_Limit, Address : Unsigned_64;
      Used, Capacity, Quota : Natural) return Decision
   with Post =>
     (if Check'Result = Admitted then
        (Contains (Stack_First, Stack_Limit, Address) or else
         Contains (Heap_First, Heap_Limit, Address)) and then
        Used < Capacity and then (Quota = 0 or else Used < Quota));
end Page_Admission;
