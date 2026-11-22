------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Controller facts that are not in the UFSHCI registers. Source: Linux
--  drivers/ufs/host/ufshcd-pci.c (ufs_intel_adl_hba_vops), used for the
--  hardware facts only. Its other Alder Lake facts need nothing here: this
--  driver never enables auto-hibernate (broken on ADL), never repeats a
--  successful link startup, and waits 200 ms for NOP IN everywhere.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package UFS_Quirks with SPARK_Mode, Pure is
   Intel_Vendor : constant Unsigned_64 := 16#8086#;
   --  Alder Lake-P and Alder Lake-N (the IdeaPad's i3-N350, 00:12.7).
   Alder_Lake_P_UFS : constant Unsigned_64 := 16#51FF#;
   Alder_Lake_N_UFS : constant Unsigned_64 := 16#54FF#;

   --  The host's TX line control commands must be off before link startup
   --  (PA_Local_TX_LCC_Enable cleared).
   function Disables_TX_LCC (Vendor, Device : Unsigned_64) return Boolean is
     (Vendor = Intel_Vendor
      and then Device in Alder_Lake_P_UFS | Alder_Lake_N_UFS);
end UFS_Quirks;
