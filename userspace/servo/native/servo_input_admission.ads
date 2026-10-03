with Client_Input_Budget;
-- Penny's 1ms deadline gates additional server fetches. Already fetched input
-- can drain without another input fetch, within the unchanged 32-event cap. The caller is the
-- sole event-thread consumer; Cached must be an exact local cache observation.
package Servo_Input_Admission with SPARK_Mode, Pure is
   package Budget renames Client_Input_Budget;
   function Can_Take (Batch : Budget.Batch; Now : Budget.Tick;
                      Cached : Natural; Controls_Stale : Boolean) return Boolean is
     (not Controls_Stale and then Budget.Used (Batch) < Budget.Poll_Limit and then
      (Cached > 0 or else Budget.Can_Poll (Batch, Now)))
   with Post =>
     (if Can_Take'Result then not Controls_Stale and
        Budget.Used (Batch) < Budget.Poll_Limit and
        (Cached > 0 or Budget.Can_Poll (Batch, Now)));
end Servo_Input_Admission;
