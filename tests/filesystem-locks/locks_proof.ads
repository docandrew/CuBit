--  The instance gnatprove analyses: the service's own capacity
--  (Fs_Handle_Codes.Maximum_Handles, 2,048 slots).
with File_Locks;
package Locks_Proof with SPARK_Mode is
   Service_Slots : constant := 2_048;
   package Service_Locks is new File_Locks (Capacity => Service_Slots);
end Locks_Proof;
