pragma Ada_2022;
with Firmware_Tables;
with Physical_Ranges;
-- Checked table-to-classifier adapter. The supplied bytes are a kernel-owned
-- snapshot copy, not an IPC address or a mapping request.
package Firmware_PCI_Admission with SPARK_Mode is
   type Outcome is (Admitted, Invalid_Table, Invalid_Allocation,
      Catalog_Capacity, Conflicting_Allocation, Classification_Unavailable);
   procedure Admit
     (M : in out Physical_Ranges.Map; Data : Firmware_Tables.Bytes; Result : out Outcome)
     with Pre => not M.Sealed and then Physical_Ranges.Well_Formed (M),
       Post => not M.Sealed and then Physical_Ranges.Well_Formed (M)
         and then (if Result /= Admitted then not M.Complete);
end Firmware_PCI_Admission;
