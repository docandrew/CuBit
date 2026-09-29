with Intel_GPU_GGTT;
with Intel_GPU_Probe;
with Intel_GPU_Resources;
with Intel_GPU_PCI_Interrupts;
package body Intel_GPU_GGTT_Access with SPARK_Mode is
   function Plan_Write
     (Config : Intel_GPU_PCI_Power.Configuration;
      Expected_BAR, Expected_Table_Bytes : Unsigned_64;
      Owner_Ready, IRQ_Disabled : Boolean) return Grant_Plan
   is
      use type Intel_GPU_PCI_Power.Power_Status;
      use type Intel_GPU_Resources.Admission_Status;
      function Word (Offset : Natural) return Unsigned_32 is
        (Unsigned_32 (Config (Offset)) or
         Shift_Left (Unsigned_32 (Config (Offset + 1)), 8) or
         Shift_Left (Unsigned_32 (Config (Offset + 2)), 16) or
         Shift_Left (Unsigned_32 (Config (Offset + 3)), 24))
        with Pre => Offset <= 252;
      Mapping : Intel_GPU_Resources.Mapping_Plan;
      IRQ : Intel_GPU_PCI_Interrupts.Snapshot;
      Bytes : Unsigned_64;
   begin
      if not Owner_Ready or else not IRQ_Disabled or else Expected_BAR = 0 or else
        Word (0) /= 16#46D2_8086# or else
        Intel_GPU_PCI_Power.Decode (Config) /= Intel_GPU_PCI_Power.D0
      then return (others => <>); end if;
      IRQ := Intel_GPU_PCI_Interrupts.Decode (Config);
      if not IRQ.Valid or else not IRQ.INTx_Disabled or else IRQ.MSI_Enabled or else
        IRQ.MSIX_Enabled or else (IRQ.MSIX_Present and not IRQ.MSIX_Masked)
      then return (others => <>); end if;
      Mapping := Intel_GPU_Resources.Plan_ADLN_Registers
        (Intel_GPU_Probe.Alder_Lake_N, Unsigned_16 (Word (4) and 16#FFFF#),
         Word (16#10#), Word (16#14#), 0, 4096);
      Bytes := Intel_GPU_GGTT.Table_Size (Unsigned_16 (Word (16#50#) and 16#FFFF#));
      if Mapping.Status /= Intel_GPU_Resources.Admitted or else
        Mapping.Physical_Base /= Expected_BAR or else Bytes = 0 or else
        Bytes /= Expected_Table_Bytes or else
        Expected_BAR > Unsigned_64'Last - 16#100_0000#
      then return (others => <>); end if;
      return (True, Expected_BAR + Intel_GPU_GGTT.Table_BAR_Offset, Bytes);
   end Plan_Write;
end Intel_GPU_GGTT_Access;
