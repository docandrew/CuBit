package AML_Global_Locks with Pure, SPARK_Mode is
   -- Internal trusted-provider protocol only: no physical address/capability.
   type Operation is (Acquire_Lease, Check_Lease, Release_Lease, Drain_Lease);
   type Outcome is (Unavailable, Succeeded, Timed_Out, Refused, Indeterminate);
   -- Unavailable/Refused/Timed_Out on first Acquire leave no cleanup duty.
   -- Indeterminate retains original completion state in the persistent IO
   -- context, independently of namespace rollback/reset. Drain never replays
   -- AML or a potentially delivered effect. Check validates the original
   -- Timed_Out is valid only for a finite/poll timeout, never Wait_Forever.
   -- The original finite lease must not be renewed. Actuals return normally,
   -- without AML reentry or Arena aliases, preserving failure attribution.
end AML_Global_Locks;
