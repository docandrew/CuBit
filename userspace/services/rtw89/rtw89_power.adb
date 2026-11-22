------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with RTW89_Registers; use RTW89_Registers;
with RTW89_MMIO; use RTW89_MMIO;
with RTW89_MAC_Registers; use RTW89_MAC_Registers;

package body RTW89_Power is

   --  read_poll_timeout(..., 1000, 20000, ...) in the power sequences.
   Power_Poll_Budget : constant Microseconds := 20_000;
   --  The efuse controller: read_poll_timeout_atomic(..., 1, 1000000, ...).
   Efuse_Poll_Budget : constant Microseconds := 1_000_000;
   --  rtw89_dump_physical_efuse_map retries a failed dump this often.
   Efuse_Dump_Attempts : constant := 5;
   --  fsleep(1000) between isolation steps.
   Settle : constant Milliseconds := 1;

   function MAC_Was_On return Boolean is
     (Shift_Right (Read32 (IC_PWR_STATE) and WLMAC_PWR_STE_MASK,
                   WLMAC_PWR_STE_SHIFT) = MAC_AX_MAC_ON);

   procedure Read_Identity (Chip : out Identity; Result : out Outcome) is
      Raw : constant Unsigned_32 := Read32 (SYS_CFG1);
      Analog : Unsigned_8;
      OK : Boolean;
   begin
      Chip := (others => <>);
      if Raw = All_Ones then
         Result := Failure (Not_Responding, SYS_CFG1, Raw);
         return;
      end if;
      Chip.Raw_Cut := Unsigned_8 (Shift_Right (Raw and CHIP_VER_MASK, CHIP_VER_SHIFT));
      Chip.Cut := (if Chip.Raw_Cut <= Cut_Version'Pos (CFV)
                   then Cut_Version'Val (Chip.Raw_Cut) else Unknown);
      XTAL_Read (XTAL_SI_CV, Analog, OK);
      if not OK then
         Result := Failure (XTAL_Timeout, WLAN_XTAL_SI_CTRL, Read32 (WLAN_XTAL_SI_CTRL));
         return;
      end if;
      Chip.Analog_Cut := Analog and XTAL_SI_ACV_MASK;
      Result := Success;
   end Read_Identity;

   --  One XTAL SI write; on failure records the outcome and returns False.
   function XTAL (Offset : XTAL_Offset; Value, Mask : Unsigned_8;
                  Result : in out Outcome) return Boolean is
      OK : Boolean;
   begin
      XTAL_Write (Offset, Value, Mask, OK);
      if not OK then
         Result := Failure (XTAL_Timeout, WLAN_XTAL_SI_CTRL,
                            Read32 (WLAN_XTAL_SI_CTRL));
      end if;
      return OK;
   end XTAL;

   function Wait (Offset : Register; Mask, Expected : Unsigned_32;
                  Result : in out Outcome) return Boolean is
   begin
      if Poll32 (Offset, Mask, Expected, Power_Poll_Budget) then
         return True;
      end if;
      Result := Failure (Poll_Timeout, Offset, Read32 (Offset));
      return False;
   end Wait;

   procedure Power_On (Trim : Boolean; Result : out Outcome) is
   begin
      Result := Success;

      --  rtw8852b_pwr_sps_ana
      Write16 (SPS_ANA_ON_CTRL2, RTL8852B_DEFAULT_SPS_ANA);

      Clear32 (SYS_PW_CTRL, AFSM_WLSUS_EN or AFSM_PCIE_SUS_EN);
      Set32 (SYS_PW_CTRL, DIS_WLBT_PDNSUSEN_SOPC);
      Set32 (WLLPS_CTRL, DIS_WLBT_LPSEN_LOPC);
      Clear32 (SYS_PW_CTRL, APDM_HPDN);
      Clear32 (SYS_PW_CTRL, APFM_SWLPS);
      if not Wait (SYS_PW_CTRL, RDY_SYSPWR, RDY_SYSPWR, Result) then
         return;
      end if;

      Set32 (AFE_LDO_CTRL, AON_OFF_PC_EN);
      if not Wait (AFE_LDO_CTRL, AON_OFF_PC_EN, AON_OFF_PC_EN, Result) then
         return;
      end if;

      --  rtw8852b_pwr_sps_dig_off, RFE type not yet known (not 5).
      Field32 (SPS_DIG_OFF_CTRL0, C1_L1_MASK, C1_L1_SHIFT, 1);
      Field32 (SPS_DIG_OFF_CTRL0, C3_L1_MASK, C3_L1_SHIFT, 3);

      Set32 (SYS_PW_CTRL, EN_WLON);
      Set32 (SYS_PW_CTRL, APFN_ONMAC);
      if not Wait (SYS_PW_CTRL, APFN_ONMAC, 0, Result) then
         return;
      end if;

      --  Two platform-enable pulses, then leave it set.
      for Pulse in 1 .. 2 loop
         Set8 (PLATFORM_ENABLE, Unsigned_8 (PLATFORM_EN));
         Clear8 (PLATFORM_ENABLE, Unsigned_8 (PLATFORM_EN));
      end loop;
      Set8 (PLATFORM_ENABLE, Unsigned_8 (PLATFORM_EN));
      Clear32 (SYS_SDIO_CTRL, PCIE_CALIB_EN_V1);

      Set32 (SYS_ADIE_PAD_PWR_CTRL, SYM_PADPDN_WL_PTA_1P3);
      if not XTAL (XTAL_SI_ANAPAR_WL, XTAL_SI_GND_SHDN_WL, XTAL_SI_GND_SHDN_WL, Result) then
         return;
      end if;
      Set32 (SYS_ADIE_PAD_PWR_CTRL, SYM_PADPDN_WL_RFC_1P3);
      if not (XTAL (XTAL_SI_ANAPAR_WL, XTAL_SI_SHDN_WL, XTAL_SI_SHDN_WL, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, XTAL_SI_OFF_WEI, XTAL_SI_OFF_WEI, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, XTAL_SI_OFF_EI, XTAL_SI_OFF_EI, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, 0, XTAL_SI_RFC2RF, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, XTAL_SI_PON_WEI, XTAL_SI_PON_WEI, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, XTAL_SI_PON_EI, XTAL_SI_PON_EI, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, 0, XTAL_SI_SRAM2RFC, Result)
              and then XTAL (XTAL_SI_SRAM_CTRL, 0, XTAL_SI_SRAM_DIS, Result)
              and then XTAL (XTAL_SI_XTAL_XMD_2, 0, XTAL_SI_LDO_LPS, Result)
              and then XTAL (XTAL_SI_XTAL_XMD_4, 0, XTAL_SI_LPS_CAP, Result))
      then
         return;
      end if;

      Set32 (PMC_DBG_CTRL2, SYSON_DIS_PMCR_AX_WRMSK);
      Set32 (SYS_ISO_CTRL, ISO_EB2CORE);
      Clear32 (SYS_ISO_CTRL, PWC_EV2EF_B15);
      Sleep (Settle);
      Clear32 (SYS_ISO_CTRL, PWC_EV2EF_B14);
      Clear32 (PMC_DBG_CTRL2, SYSON_DIS_PMCR_AX_WRMSK);

      if Trim then
         --  Regulator trims (rtw8852b.c, cut CBV over PCIe).
         Field32 (SPS_DIG_ON_CTRL0, VOL_L1_MASK, VOL_L1_SHIFT, VOL_L1_TRIM);
         Field32 (SPS_DIG_ON_CTRL0, VREFPFM_L_MASK, VREFPFM_L_SHIFT, VREFPFM_L_TRIM);
         Set32 (PMC_DBG_CTRL2, SYSON_DIS_PMCR_AX_WRMSK);
         Write16 (HCI_LDO_CTRL, (Read16 (HCI_LDO_CTRL) and not R_AX_VADJ_MASK) or VADJ_TRIM);
         Clear32 (PMC_DBG_CTRL2, SYSON_DIS_PMCR_AX_WRMSK);
      end if;
      Set32 (DMAC_FUNC_EN,
             MAC_FUNC_EN or DMAC_FUNC_ENABLE or MPDU_PROC_EN or WD_RLS_EN
             or DLE_WDE_EN or TXPKT_CTRL_EN or STA_SCH_EN or DLE_PLE_EN
             or PKT_BUF_EN or DMAC_TBL_EN or PKT_IN_EN or DLE_CPUIO_EN
             or DISPATCHER_EN or BBRPT_EN or MAC_SEC_EN or DMACREG_GCKEN);
      Set32 (CMAC_FUNC_EN,
             CMAC_EN or CMAC_TXEN or CMAC_RXEN or FORCE_CMACREG_GCKEN
             or PHYINTF_EN or CMAC_DMA_EN or PTCLTOP_EN or SCHEDULER_EN
             or TMAC_EN or RMAC_EN);
      Field32 (EECS_EESK_FUNC_SEL, PINMUX_EESK_FUNC_SEL_MASK,
               PINMUX_EESK_FUNC_SEL_SHIFT, PINMUX_EESK_FUNC_SEL_BT_LOG);
      --  Tell the Bluetooth side the WLAN MAC is on (scoreboard).
      Write8 (SCOREBOARD_NOTIFY, NOTIFY_TP_MAJOR);
   end Power_On;

   procedure Power_Off (Result : out Outcome) is
   begin
      Result := Success;
      Write16 (SPS_ANA_ON_CTRL2, RTL8852B_DEFAULT_SPS_ANA);
      if not (XTAL (XTAL_SI_ANAPAR_WL, XTAL_SI_RFC2RF, XTAL_SI_RFC2RF, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, 0, XTAL_SI_OFF_EI, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, 0, XTAL_SI_OFF_WEI, Result)
              and then XTAL (XTAL_SI_WL_RFC_S0, 0, XTAL_SI_RF00, Result)
              and then XTAL (XTAL_SI_WL_RFC_S1, 0, XTAL_SI_RF10, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, XTAL_SI_SRAM2RFC, XTAL_SI_SRAM2RFC, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, 0, XTAL_SI_PON_EI, Result)
              and then XTAL (XTAL_SI_ANAPAR_WL, 0, XTAL_SI_PON_WEI, Result))
      then
         return;
      end if;

      Set32 (SYS_PW_CTRL, EN_WLON);
      Clear32 (WLRF_CTRL, AFC_AFEDIG);
      Clear8 (SYS_FUNC_EN, FEN_BB_GLB_RSTN or FEN_BBRSTB);
      Clear32 (SYS_ADIE_PAD_PWR_CTRL, SYM_PADPDN_WL_RFC_1P3);
      if not XTAL (XTAL_SI_ANAPAR_WL, 0, XTAL_SI_SHDN_WL, Result) then
         return;
      end if;
      Clear32 (SYS_ADIE_PAD_PWR_CTRL, SYM_PADPDN_WL_PTA_1P3);
      if not XTAL (XTAL_SI_ANAPAR_WL, 0, XTAL_SI_GND_SHDN_WL, Result) then
         return;
      end if;

      Set32 (SYS_PW_CTRL, APFM_OFFMAC);
      if not Wait (SYS_PW_CTRL, APFM_OFFMAC, 0, Result) then
         return;
      end if;

      Write32 (WLLPS_CTRL, SW_LPS_OPTION);
      Set32 (SYS_SWR_CTRL1, SYM_CTRL_SPS_PWMFREQ);
      Field32 (SPS_DIG_ON_CTRL0, REG_ZCDC_H_MASK, REG_ZCDC_H_SHIFT, REG_ZCDC_H_POWER_OFF);
      Set32 (SYS_PW_CTRL, APFM_SWLPS);
      Write8 (SCOREBOARD_NOTIFY, NOTIFY_PWR_MAJOR);
   end Power_Off;

   function Efuse_Autoloaded return Boolean is
     ((Read16 (SYS_WL_EFUSE_CTRL) and AUTOLOAD_SUS) /= 0);

   --  rtw89_enable/disable_efuse_pwr_cut_ddv (8852B, not CAV burst mode is
   --  handled by the caller's cut check).
   procedure Efuse_Power_Cut (Enable : Boolean; Burst : Boolean) is
   begin
      if Enable then
         Set8 (PMC_DBG_CTRL2, Unsigned_8 (SYSON_DIS_PMCR_AX_WRMSK));
         Set16 (SYS_ISO_CTRL, Unsigned_16 (PWC_EV2EF_B14));
         Sleep (Settle);
         Set16 (SYS_ISO_CTRL, Unsigned_16 (PWC_EV2EF_B15));
         Clear16 (SYS_ISO_CTRL, Unsigned_16 (ISO_EB2CORE));
         if Burst then
            Set32 (EFUSE_CTRL_1_V1, EF_BURST);
         end if;
      else
         if Burst then
            Clear32 (EFUSE_CTRL_1_V1, EF_BURST);
         end if;
         Set16 (SYS_ISO_CTRL, Unsigned_16 (ISO_EB2CORE));
         Clear16 (SYS_ISO_CTRL, Unsigned_16 (PWC_EV2EF_B15));
         Sleep (Settle);
         Clear16 (SYS_ISO_CTRL, Unsigned_16 (PWC_EV2EF_B14));
         Clear8 (PMC_DBG_CTRL2, Unsigned_8 (SYSON_DIS_PMCR_AX_WRMSK));
      end if;
   end Efuse_Power_Cut;

   procedure Read_Efuse (Map : out RTW89_Efuse.Physical_Map; Result : out Outcome) is
      Chip   : Identity;
      Ignore : Outcome;
      Burst  : Boolean;
      Control : Unsigned_32 := 0;
      Failed : Boolean;
   begin
      Map := [others => RTW89_Efuse.Unprogrammed];
      Read_Identity (Chip, Ignore);
      --  OTP burst mode only on the A cut (rtw89 efuse.c).
      Burst := Chip.Cut = CAV;
      Result := Success;
      for Attempt in 1 .. Efuse_Dump_Attempts loop
         Failed := False;
         Efuse_Power_Cut (True, Burst);
         for Address in RTW89_Efuse.Physical_Index loop
            Write32 (EFUSE_CTRL,
                     Shift_Left (Unsigned_32 (Address), EF_ADDR_SHIFT) and EF_ADDR_MASK);
            if not Poll32 (EFUSE_CTRL, EF_RDY, EF_RDY, Efuse_Poll_Budget) then
               Failed := True;
               Control := Read32 (EFUSE_CTRL);
               exit;
            end if;
            Map (Address) := Unsigned_8 (Read32 (EFUSE_CTRL) and EF_DATA_MASK);
         end loop;
         --  rtw89 returns -EBUSY without disabling the power cut; we always
         --  restore isolation so a retry starts from the same state.
         Efuse_Power_Cut (False, Burst);
         if not Failed then
            return;
         end if;
         Result := Failure (Poll_Timeout, EFUSE_CTRL, Control);
      end loop;
   end Read_Efuse;

   procedure Read_Secure_Boot_Info (Info : out Secure_Boot_Info; Result : out Outcome) is
      Chip   : Identity;
      Ignore : Outcome;
      Burst  : Boolean;
      Address : Unsigned_32;
   begin
      Info := [others => RTW89_Efuse.Unprogrammed];
      Read_Identity (Chip, Ignore);
      Burst := Chip.Cut = CAV;
      Result := Success;
      Efuse_Power_Cut (True, Burst);
      for I in Info'Range loop
         Address := Secure_Boot_Info_Address + Unsigned_32 (I - Info'First);
         Write32 (EFUSE_CTRL, Shift_Left (Address, EF_ADDR_SHIFT) and EF_ADDR_MASK);
         if not Poll32 (EFUSE_CTRL, EF_RDY, EF_RDY, Efuse_Poll_Budget) then
            Result := Failure (Poll_Timeout, EFUSE_CTRL, Read32 (EFUSE_CTRL));
            exit;
         end if;
         Info (I) := Unsigned_8 (Read32 (EFUSE_CTRL) and EF_DATA_MASK);
      end loop;
      Efuse_Power_Cut (False, Burst);
   end Read_Secure_Boot_Info;

   procedure Read_Physical_Byte (Address : Efuse_Address; Value : out Unsigned_8;
                                 Result : out Outcome) is
      Chip   : Identity;
      Ignore : Outcome;
      Burst  : Boolean;
   begin
      Value := RTW89_Efuse.Unprogrammed;
      Read_Identity (Chip, Ignore);
      Burst := Chip.Cut = CAV;
      Result := Success;
      Efuse_Power_Cut (True, Burst);
      Write32 (EFUSE_CTRL, Shift_Left (Address, EF_ADDR_SHIFT) and EF_ADDR_MASK);
      if Poll32 (EFUSE_CTRL, EF_RDY, EF_RDY, Efuse_Poll_Budget) then
         Value := Unsigned_8 (Read32 (EFUSE_CTRL) and EF_DATA_MASK);
      else
         Result := Failure (Poll_Timeout, EFUSE_CTRL, Read32 (EFUSE_CTRL));
      end if;
      Efuse_Power_Cut (False, Burst);
   end Read_Physical_Byte;

end RTW89_Power;
