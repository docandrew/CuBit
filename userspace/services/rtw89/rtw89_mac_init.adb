------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with RTW89_Registers; use RTW89_Registers;
with RTW89_MAC_Registers; use RTW89_MAC_Registers;
with RTW89_MMIO; use RTW89_MMIO;

package body RTW89_MAC_Init is

   --  TRXCFG_WAIT_CNT and the BA CAM reset poll (read_poll_timeout).
   Init_Budget  : constant Microseconds := 2_000;
   BACAM_Budget : constant Microseconds := 1_000;
   Settle_Budget : constant Microseconds := HFC_Settle_Us;

   function DMAC_On return Boolean is
      V : constant Unsigned_32 := Read32 (DMAC_FUNC_EN);
   begin
      return V /= Config_EA and then V /= Dead and then (V and DMAC_REQUIRED) = DMAC_REQUIRED;
   end DMAC_On;

   function CMAC_On return Boolean is
      V : constant Unsigned_32 := Read32 (CMAC_FUNC_EN);
   begin
      return V /= Config_EA and then V /= Dead and then (V and CMAC_REQUIRED) = CMAC_REQUIRED;
   end CMAC_On;


   procedure H2C (DMA : RTW89_Bringup.Region; Class, Func : Unsigned_8;
                  Payload : RTW89_H2C.Bytes; Done_Ack : Boolean; Result : in out Outcome) is
      Sent : Boolean;
   begin
      if not OK (Result) then
         return;
      end if;
      RTW89_Bringup.Send_H2C (DMA, RTW89_H2C.Category_MAC, Class, Func,
                              RTW89_Bringup.H2C_Bytes (Payload), Done_Ack, Sent);
      if not Sent then
         Result := Failure (H2C_Ring_Full, TX_Rings (CH12).Index, Read32 (TX_Rings (CH12).Index));
      end if;
   end H2C;

   ---------------------------------------------------------------------------
   procedure Enable_BB_RF (Result : out Outcome) is
      OK : Boolean;
   begin
      Result := Success;
      Set8 (SYS_FUNC_EN, FEN_BBRSTB or FEN_BB_GLB_RSTN);
      Field32 (SPS_DIG_ON_CTRL0, REG_ZCDC_H_MASK, REG_ZCDC_H_SHIFT, ZCDC_H_ENABLE);
      Set32 (WLRF_CTRL, AFC_AFEDIG);
      Clear32 (WLRF_CTRL, AFC_AFEDIG);
      Set32 (WLRF_CTRL, AFC_AFEDIG);
      XTAL_Write (XTAL_SI_WL_RFC_S0, XTAL_SI_RFC_ON, XTAL_SI_FULL_MASK, OK);
      if OK then
         XTAL_Write (XTAL_SI_WL_RFC_S1, XTAL_SI_RFC_ON, XTAL_SI_FULL_MASK, OK);
      end if;
      if not OK then
         Result := Failure (XTAL_Timeout, WLAN_XTAL_SI_CTRL, Read32 (WLAN_XTAL_SI_CTRL));
         return;
      end if;
      Write8 (PHYREG_SET, PHYREG_SET_XYN_CYCLE);
   end Enable_BB_RF;

   procedure Sys_Init is
   begin
      Write32 (DMAC_FUNC_EN, DMAC_FUNC_SYS_INIT);
      Write32 (DMAC_CLK_EN, DMAC_CLK_SYS_INIT);
      Set32 (CK_EN, CK_EN_CMAC_ALL);
      Set32 (CMAC_FUNC_EN, CMAC_FUNC_ALL);
      Set32 (SPS_DIG_ON_CTRL0, OCP_L1_MASK);
   end Sys_Init;

   ---------------------------------------------------------------------------
   procedure DMAC_Init (Result : out Outcome) is
      Value : Unsigned_32;
   begin
      Result := Success;
      if not DMAC_On then
         Result := Failure (DMAC_Disabled, DMAC_FUNC_EN, Read32 (DMAC_FUNC_EN));
         return;
      end if;
      --  DLE, SCC quotas.
      Clear32 (DMAC_FUNC_EN, DLE_WDE_EN or DLE_PLE_EN);
      Set32 (DMAC_CLK_EN, DLE_WDE_CLK_EN or DLE_PLE_CLK_EN);
      Value := (Read32 (WDE_PKTBUF_CFG) and not (PAGE_SEL_MASK or START_BOUND_MASK or FREE_PAGE_NUM_MASK))
        or WDE_PAGE_SEL_64 or Shift_Left (Unsigned_32'(SCC_WDE_Linked), FREE_PAGE_NUM_SHIFT);
      Write32 (WDE_PKTBUF_CFG, Value);
      Value := (Read32 (PLE_PKTBUF_CFG) and not (PAGE_SEL_MASK or START_BOUND_MASK or FREE_PAGE_NUM_MASK))
        or PLE_PAGE_SEL_128
        or Shift_Left (Unsigned_32 ((SCC_WDE_Linked + SCC_WDE_Unlinked) * SCC_WDE_Page_Bytes
                                    / DLE_BOUND_UNIT), START_BOUND_SHIFT)
        or Shift_Left (Unsigned_32'(SCC_PLE_Linked), FREE_PAGE_NUM_SHIFT);
      Write32 (PLE_PKTBUF_CFG, Value);
      Write32 (WDE_QTA0_CFG, WDE_QTA_HIF_SCC);
      Write32 (WDE_QTA1_CFG, WDE_QTA_WCPU_SCC);
      Write32 (WDE_QTA3_CFG, WDE_QTA_PKT_IN_SCC);
      Write32 (WDE_QTA4_CFG, WDE_QTA_CPU_IO_SCC);
      for Index in PLE_QTA_SCC'Range loop
         Write32 (PLE_QTA0_CFG + Register (Index * PLE_QTA_STRIDE), PLE_QTA_SCC (Index));
      end loop;
      Set32 (DMAC_FUNC_EN, DLE_WDE_EN or DLE_PLE_EN);
      if not Poll32 (WDE_INI_STATUS, MGN_INI_RDY, MGN_INI_RDY, Init_Budget) then
         Result := Failure (Poll_Timeout, WDE_INI_STATUS, Read32 (WDE_INI_STATUS));
         return;
      elsif not Poll32 (PLE_INI_STATUS, MGN_INI_RDY, MGN_INI_RDY, Init_Budget) then
         Result := Failure (Poll_Timeout, PLE_INI_STATUS, Read32 (PLE_INI_STATUS));
         return;
      end if;

      --  HCI flow control, normal mode.
      Clear32 (HCI_FC_CTRL, HCI_FC_EN or HCI_FC_CH12_EN);
      for C of HFC_CHANNELS_SCC loop
         Write32 (ACH0_PAGE_CTRL + Register (C.Index * PAGE_CTRL_STRIDE), C.Value);
      end loop;
      Write32 (PUB_PAGE_CTRL1, PUB_PAGE_G0_SCC);
      Write32 (WP_PAGE_CTRL2, 0);
      Write32 (CH_PAGE_CTRL, PREC_CH011_CH12);
      Write32 (PUB_PAGE_CTRL2, PUB_PAGE_ALL_SCC);
      Write32 (WP_PAGE_CTRL1, 0);
      Value := Read32 (HCI_FC_CTRL)
        and not (HCI_FC_MODE_MASK or WD_FULL_COND_MASK or HCI_FC_CH12_FULL_COND_MASK or WP_FULL_COND_MASK);
      Write32 (HCI_FC_CTRL, Value or Shift_Left (WD_FULL_COND_SCC, WD_FULL_COND_SHIFT));
      Set32 (HCI_FC_CTRL, HCI_FC_EN or HCI_FC_CH12_EN);
      Delay_For (Settle_Budget);
      declare
         Used : constant Unsigned_32 := Read32 (PUB_PAGE_INFO1);
         Available : constant Unsigned_32 := Read32 (PUB_PAGE_INFO2) and PAGE_FIELD_MASK;
      begin
         if (Used and PAGE_FIELD_MASK) + (Shift_Right (Used, PAGE_HIGH_SHIFT) and PAGE_FIELD_MASK)
            + Available /= PUB_PAGES_SCC
         then
            Result := Failure (Page_Count_Mismatch, PUB_PAGE_INFO1, Used);
            return;
         end if;
      end;

      --  STA scheduler.
      Set8 (SS_CTRL, SS_EN);
      if not Poll32 (SS_CTRL, SS_INIT_DONE_1, SS_INIT_DONE_1, Init_Budget) then
         Result := Failure (Poll_Timeout, SS_CTRL, Read32 (SS_CTRL));
         return;
      end if;
      Set32 (SS_CTRL, SS_WARM_INIT_FLG);
      Clear32 (SS_CTRL, SS_NONEMPTY_SS2FINFO_EN);

      --  MPDU processor (received MPDUs keep their FCS).
      Write32 (ACTION_FWD0, TRXCFG_MPDU_PROC_ACT_FRWD);
      Write32 (TF_FWD, TRXCFG_MPDU_PROC_TF_FRWD);
      Set32 (MPDU_PROC, APPEND_FCS or A_ICV_ERR);
      Write32 (CUT_AMSDU_CTRL, TRXCFG_MPDU_PROC_CUT_CTRL);

      --  Security engine.
      Write32 (SEC_ENG_CTRL, (Read32 (SEC_ENG_CTRL) or SEC_ENG_ENABLES) and not TX_PARTIAL_MODE);
      Set32 (SEC_MPDU_PROC, APPEND_MIC_ICV);
   end DMAC_Init;

   ---------------------------------------------------------------------------
   procedure CMAC_Init (Result : out Outcome) is
      Value16 : Unsigned_16;
   begin
      Result := Success;
      if not CMAC_On then
         Result := Failure (CMAC_Disabled, CMAC_FUNC_EN, Read32 (CMAC_FUNC_EN));
         return;
      end if;
      --  Scheduler.
      Field32 (PREBKF_CFG_1, SIFS_MACTXEN_T1_MASK, 0, SIFS_MACTXEN_T1);
      Set32 (SCH_EXT_CTRL, PORT_RST_TSF_ADV);
      Clear32 (CCA_CFG_0, BTCCA_EN);
      Field32 (PREBKF_CFG_0, PREBKF_TIME_MASK, 0, SCH_PREBKF_24US);
      --  Address CAM reset.
      Set32 (ADDR_CAM_CTRL, ADDR_CAM_RESET);
      if not Poll16 (ADDR_CAM_CTRL, ADDR_CAM_CLR, 0, Init_Budget) then
         Result := Failure (Poll_Timeout, ADDR_CAM_CTRL, Read32 (ADDR_CAM_CTRL));
         return;
      end if;
      --  Receive filter defaults.
      Write32 (MGNT_FLTR, RX_FLTR_FRAME_TO_HOST);
      Write32 (CTRL_FLTR, RX_FLTR_FRAME_TO_HOST);
      Write32 (DATA_FLTR, RX_FLTR_FRAME_TO_HOST);
      Write32 (RX_FLTR_OPT, DEFAULT_AX_RX_FLTR);
      Write16 (PLCP_HDR_FLTR, PLCP_HDR_CHECKS);
      --  CCA control and response checks.
      Write32 (CCA_CONTROL, (Read32 (CCA_CONTROL) or CCA_CONTROL_SET) and not CCA_CONTROL_CLEAR);
      Clear32 (RSP_CHK_SIG, RSP_CHK_NAVS);
      Clear32 (TRXPTCL_RESP_0, RSP_CHK_CCAS);
      --  NAV.
      Set32 (WMAC_NAV_CTL, WMAC_NAV_ENABLES);
      Field32 (WMAC_NAV_CTL, WMAC_NAV_UPPER_MASK, WMAC_NAV_UPPER_SHIFT, NAV_25MS);
      --  Spatial reuse off.
      Clear8 (RX_SR_CTRL, SR_EN);
      Set8 (BSSID_SRC_CTRL, PLCP_SRC_EN);
      --  TMAC.
      Clear32 (MAC_LOOPBACK, MACLBK_EN);
      Field32 (TCR0, TCR_UDF_THSD_MASK, TCR_UDF_THSD_SHIFT, TCR_UDF_THSD);
      Field32 (TXD_FIFO_CTRL, TXDFIFO_HIGH_MCS_THRE_MASK, TXDFIFO_HIGH_MCS_THRE_SHIFT, TXDFIFO_MCS_THRE);
      Field32 (TXD_FIFO_CTRL, TXDFIFO_LOW_MCS_THRE_MASK, TXDFIFO_LOW_MCS_THRE_SHIFT, TXDFIFO_MCS_THRE);
      --  TRX protocol.
      Write32 (TRXPTCL_RESP_0,
               (Read32 (TRXPTCL_RESP_0) and not (WMAC_SPEC_SIFS_CCK_MASK or WMAC_SPEC_SIFS_OFDM_MASK))
               or WMAC_SPEC_SIFS_CCK or Shift_Left (WMAC_SPEC_SIFS_OFDM_52B, WMAC_SPEC_SIFS_OFDM_SHIFT));
      Set32 (RXTRIG_TEST_USER_2, RXTRIG_FCSCHK_EN);
      Clear32 (TRXPTCL_RRSR_CTL_0, WMAC_RESP_REF_RATE_SEL);
      Field32 (TRXPTCL_RRSR_CTL_0, WMAC_RESP_RSC_MASK, WMAC_RESP_RSC_SHIFT, WMAC_RESP_RSC_8852B);
      --  RMAC: BA CAM reset (rtw89 only warns on timeout), deadlock
      --  protection, primary channel, MPDU length limit.
      Field32 (RESPBA_CAM_CTRL, BACAM_RST_MASK, 0, BACAM_RST_ALL);
      if not Poll32 (RESPBA_CAM_CTRL, BACAM_RST_MASK, 0, BACAM_Budget) then
         null;
      end if;
      Set8 (RESPBA_CAM_CTRL, SSN_SEL);
      Value16 := Read16 (DLK_PROTECT_CTL);
      Value16 := (Value16 and not (RX_DLK_DATA_TIME_MASK or RX_DLK_CCA_TIME_MASK))
        or Shift_Left (TRXCFG_RMAC_DATA_TO, RX_DLK_DATA_TIME_SHIFT)
        or Shift_Left (TRXCFG_RMAC_CCA_TO, RX_DLK_CCA_TIME_SHIFT);
      Write16 (DLK_PROTECT_CTL, Value16);
      Write8 (RCR, (Read8 (RCR) and not CH_EN_MASK) or CH_EN_PRIMARY);
      Field32 (RX_FLTR_OPT, RX_MPDU_MAX_LEN_MASK, RX_MPDU_MAX_LEN_SHIFT, RX_MPDU_MAX_LEN_SCC);
      Clear8 (PLCP_HDR_FLTR, VHT_SU_SIGB_CRC_CHK);
      --  CMAC common.
      Clear32 (TX_SUB_CARRIER_VALUE, TXSC_MASKS);
      Field32 (PTCL_RRSR1, RRSR_RATE_EN_MASK, RRSR_RATE_EN_SHIFT, RRSR_OFDM_CCK_EN);
      --  Protocol (PCIe).
      Write32 (SIFS_SETTING,
               (Read32 (SIFS_SETTING) and not (HW_CTS2SELF_PKT_LEN_TH_MASK or HW_CTS2SELF_PKT_LEN_TH_TWW_MASK))
               or Shift_Left (CTS2S_TH_1K, HW_CTS2SELF_PKT_LEN_TH_SHIFT)
               or Shift_Left (CTS2S_TH_SEC_256B, HW_CTS2SELF_PKT_LEN_TH_TWW_SHIFT)
               or HW_CTS2SELF_EN);
      Write32 (PTCL_FSM_MON,
               (Read32 (PTCL_FSM_MON) and not (PTCL_TX_ARB_TO_THR_MASK or PTCL_TX_ARB_TO_MODE))
               or PTCL_TO_2MS);
      Set8 (PTCL_COMMON_SETTING_0, CMAC_TX_MODES);
      Clear8 (PTCL_COMMON_SETTING_0, PTCL_TRIGGER_SS_ENS);
      Write8 (PTCLRPT_FULL_HDL,
              (Read8 (PTCLRPT_FULL_HDL) and not Unsigned_8 (SPE_RPT_PATH_MASK))
              or Unsigned_8 (Shift_Left (FWD_TO_WLCPU, SPE_RPT_PATH_SHIFT)));
      Field32 (AGG_LEN_VHT_0, AMPDU_MAX_LEN_VHT_MASK, 0, AMPDU_MAX_LEN_VHT);
      --  CMAC DMA.
      Clear8 (RXDMA_CTRL_0, RX_FULL_MODE);
   end CMAC_Init;

   procedure Apply (Steps : IMR_Table) is
   begin
      for S of Steps loop
         Clear32 (S.Where, S.Clear);
         Set32 (S.Where, S.Set);
      end loop;
   end Apply;

   type Register_List is array (Positive range <>) of Register;
   LTR_Registers : constant Register_List := [LTR_CTRL_0, LTR_CTRL_1, LTR_IDLE_LATENCY, LTR_ACTIVE_LATENCY];

   ---------------------------------------------------------------------------
   --  rtw89_pci_ltr_set when the link enabled LTR (Device Control 2).
   procedure LTR (Result : out Outcome) is
      Pointer, Identifier, Control : Unsigned_8;
      OK : Boolean;
      Capability_Hops : constant := 48;   --  a capability list is at most 48 entries
      Next_Offset : constant := 1;
      Pointer_Mask : constant Unsigned_8 := 16#FC#;
      Found : Boolean := False;
   begin
      Result := Success;
      DBI_Read (PCI_CAPABILITY_POINTER, Pointer, OK);
      for Hop in 1 .. Capability_Hops loop
         exit when not OK or else Pointer = 0;
         Pointer := Pointer and Pointer_Mask;
         DBI_Read (Config_Offset (Pointer), Identifier, OK);
         if OK and then Identifier = PCI_CAP_ID_EXP then
            Found := True;
            exit;
         end if;
         if OK then
            DBI_Read (Config_Offset (Pointer) + Next_Offset, Pointer, OK);
         end if;
      end loop;
      if not OK then
         Result := Failure (DBI_Timeout, DBI_FLAG, Unsigned_32 (Read16 (DBI_FLAG)));
         return;
      elsif not Found then
         return;
      end if;
      DBI_Read (Config_Offset (Pointer) + PCI_EXP_DEVCTL2_HIGH, Control, OK);
      if not OK then
         Result := Failure (DBI_Timeout, DBI_FLAG, Unsigned_32 (Read16 (DBI_FLAG)));
         return;
      elsif (Control and PCI_EXP_DEVCTL2_LTR_EN_HIGH) = 0 then
         return;
      end if;
      for Where of LTR_Registers loop
         if Read32 (Where) = All_Ones or else Read32 (Where) = Config_EA then
            Result := Failure (Not_Responding, Where, Read32 (Where));
            return;
         end if;
      end loop;
      Set32 (LTR_CTRL_0, LTR_HW_EN_AND_WD_TIMER);
      Field32 (LTR_CTRL_0, LTR_SPACE_IDX_MASK, LTR_SPACE_IDX_SHIFT, LTR_SPACE_IDX);
      Field32 (LTR_CTRL_0, LTR_IDLE_TIMER_MASK, LTR_IDLE_TIMER_SHIFT, LTR_IDLE_TIMER);
      Field32 (LTR_CTRL_1, LTR_RX0_TH_MASK, 0, LTR_RX_TH);
      Field32 (LTR_CTRL_1, LTR_RX1_TH_MASK, LTR_RX1_TH_SHIFT, LTR_RX_TH);
      Write32 (LTR_IDLE_LATENCY, LTR_IDLE_VALUE);
      Write32 (LTR_ACTIVE_LATENCY, LTR_ACTIVE_VALUE);
   end LTR;

   ---------------------------------------------------------------------------
   procedure Initialize (DMA : RTW89_Bringup.Region; Result : out Outcome) is
   begin
      Enable_BB_RF (Result);
      if not OK (Result) then
         return;
      end if;
      Sys_Init;
      DMAC_Init (Result);
      if not OK (Result) then
         return;
      end if;
      CMAC_Init (Result);
      if not OK (Result) then
         return;
      end if;
      if not DMAC_On then
         Result := Failure (DMAC_Disabled, DMAC_FUNC_EN, Read32 (DMAC_FUNC_EN));
         return;
      end if;
      Apply (DMAC_IMRS);
      if not CMAC_On then
         Result := Failure (CMAC_Disabled, CMAC_FUNC_EN, Read32 (CMAC_FUNC_EN));
         return;
      end if;
      Apply (CMAC_IMRS);
      Write32 (DMAC_ERR_IMR, ERR_IMR_ALL);
      Write32 (CMAC_ERR_IMR, ERR_IMR_ALL);
      --  Host release reports.
      Field32 (WDRLS_CFG, WDRLS_MODE_MASK, 0, RPR_MODE_POH);
      Set32 (RLSRPT0_CFG0, RLSRPT0_FLTR_MAP_MASK);
      Field32 (RLSRPT0_CFG1, RLSRPT0_AGGNUM_MASK, 0, RLSRPT0_AGGNUM);
      Field32 (RLSRPT0_CFG1, RLSRPT0_TO_MASK, RLSRPT0_TO_SHIFT, RLSRPT0_TO);
      --  PCIe post-init.
      LTR (Result);
      if not OK (Result) then
         return;
      end if;
      Set32 (TX_ADDRESS_INFO_MODE_SETTING, HOST_ADDR_INFO_8B_SEL);
      Clear32 (PKTIN_SETTING, WD_ADDR_INFO_LENGTH);
      Clear32 (PCIE_DMA_STOP1, STOP_TX_ACH0_TO_ACH3_CH8_CH9);
      Clear32 (PCIE_DMA_STOP1, STOP_WPDMA or STOP_PCIEIO);
      H2C (DMA, RTW89_H2C.Class_FW_Offload, RTW89_H2C.Function_Offload_Config,
           RTW89_H2C.Offload_Config, Done_Ack => True, Result => Result);
   end Initialize;

   procedure Start (DMA : RTW89_Bringup.Region; Result : out Outcome) is
   begin
      Result := Success;
      if CMAC_On then
         Write32 (PPDU_STAT, PPDU_STAT_ENABLES);
         Field32 (HW_RPT_FWD, FWD_PPDU_STAT_MASK, 0, PRPT_DEST_HOST);
      end if;
      Write32 (AGG_LEN_HT_0,
               (Read32 (AGG_LEN_HT_0) and not (RTS_TXTIME_TH_MASK or RTS_LEN_TH_MASK))
               or Shift_Left (RTS_TXTIME_TH_DEFAULT, RTS_TXTIME_TH_SHIFT) or RTS_LEN_TH_DEFAULT);
      H2C (DMA, RTW89_H2C.Class_FW_Info, RTW89_H2C.Function_Log_Config,
           RTW89_H2C.Log_Config_Off, Done_Ack => False, Result => Result);
   end Start;

   ---------------------------------------------------------------------------
   --  rtw89_mac_port_update for port 0, a station: NO_LINK (not
   --  associated) or INFRA (associated, Interval the BSS's beacon interval).
   procedure Port_Config (Infrastructure : Boolean; Interval : Unsigned_16) is
   begin
      --  func_sw: an enabled port is stopped for a beacon interval first.
      if (Read32 (PORT_CFG_P0) and PORT_FUNC_EN) /= 0 then
         RTW89_MMIO.Sleep (RTW89_MMIO.Milliseconds'Min (Unsigned_64 (Interval) + 1, RTW89_MMIO.Milliseconds'Last));
         Clear32 (PORT_CFG_P0, PORT_FUNC_EN or BRK_SETUP);
         Set32 (PORT_CFG_P0, TSFTR_RST);
         Write32 (BCN_CNT_TMR_P0, 0);
      end if;
      Clear32 (PORT_CFG_P0, TXBCN_RPT_EN);
      Clear32 (PORT_CFG_P0, RXBCN_RPT_EN);
      Field32 (PORT_CFG_P0, NET_TYPE_MASK, NET_TYPE_SHIFT, (if Infrastructure then NET_TYPE_INFRA else 0));
      if Infrastructure then
         Set32 (PORT_CFG_P0, TBTT_PROHIB_EN or BRK_SETUP);    --  bcn_prct
         Set32 (PORT_CFG_P0, RX_BSSID_FIT_EN);                --  rx_sw
         Set32 (PORT_CFG_P0, TSF_UDT_EN);                     --  rx_sync
      else
         Clear32 (PORT_CFG_P0, TBTT_PROHIB_EN or BRK_SETUP);
         Clear32 (PORT_CFG_P0, RX_BSSID_FIT_EN);
         Clear32 (PORT_CFG_P0, TSF_UDT_EN);
      end if;
      Clear32 (PORT_CFG_P0, BCNTX_EN);
      Field32 (BCN_SPACE_P0, BCN_SPACE_MASK, 0,
               (if Infrastructure and then Interval /= 0 then Unsigned_32 (Interval) else BCN_INTERVAL_DEF));
      Write8 (HIQ_WIN_P0, 0);
      Set8 (MD_TSFT_STMP_CTL, UPD_HGQMD_AND_TIMEOUT);
      Write16 (DTIM_CTRL_P0, Read16 (DTIM_CTRL_P0) and not DTIM_NUM_MASK);
      Clear32 (MBSSID_DROP_0, MBSSID_DROP_PORT0);
      Field32 (TBTT_PROHIB_P0, TBTT_SETUP_MASK, 0, BCN_SETUP_DEF);
      Field32 (TBTT_PROHIB_P0, TBTT_HOLD_MASK, TBTT_HOLD_SHIFT, BCN_HOLD_DEF);
      Clear32 (BCN_AREA_P0, BCN_MSK_AREA_MASK);
      Write16 (TBTT_EARLY_P0, (Read16 (TBTT_EARLY_P0) and not TBTT_EARLY_MASK) or TBTT_EARLY_DEF);
      Write16 (TBTT_AGG_P0, (Read16 (TBTT_AGG_P0) and not TBTT_AGG_NUM_MASK)
                            or Shift_Left (TBTT_AGG_DEF, TBTT_AGG_NUM_SHIFT));
      Clear32 (PTCL_BSS_COLOR_0, BSS_COLOR_MASK);
      Clear32 (MBSSID_CTRL, P0MB_ALL);
      Set32 (PORT_CFG_P0, PORT_FUNC_EN);
      Delay_For (BCN_ERLY_SET_DLY_US);
      Field32 (BCN_EARLY_P0, BCN_ERLY_MASK, 0, BCN_ERLY_DEF);
      Clear32 (BCN_PSR_RPT_P0, BCAID_P0_MASK);
   end Port_Config;

   procedure Port_Update is
   begin
      Port_Config (Infrastructure => False, Interval => 0);
   end Port_Update;

   procedure Update_Station (DMA : RTW89_Bringup.Region; Station : RTW89_H2C.Station;
                             Result : out Outcome) is
   begin
      Result := Success;
      H2C (DMA, RTW89_H2C.Class_Address_CAM, RTW89_H2C.Function_Address_CAM,
           RTW89_H2C.Station_CAM (Station), Done_Ack => True, Result => Result);
   end Update_Station;

   procedure Associate (DMA : RTW89_Bringup.Region; Station : RTW89_H2C.Station; Interval : Unsigned_16;
                        Result : out Outcome) is
   begin
      Result := Success;
      Port_Config (Infrastructure => True, Interval => Interval);
      H2C (DMA, RTW89_H2C.Class_Media_Report, RTW89_H2C.Function_Join_Info,
           RTW89_H2C.Join_Info_Connected, Done_Ack => True, Result => Result);
      H2C (DMA, RTW89_H2C.Class_Address_CAM, RTW89_H2C.Function_Address_CAM,
           RTW89_H2C.Station_CAM (Station), Done_Ack => True, Result => Result);
   end Associate;

   procedure Disassociate (DMA : RTW89_Bringup.Region; Station : RTW89_H2C.Station; Result : out Outcome) is
   begin
      Result := Success;
      H2C (DMA, RTW89_H2C.Class_Media_Report, RTW89_H2C.Function_Join_Info,
           RTW89_H2C.Join_Info_Disconnected, Done_Ack => True, Result => Result);
      H2C (DMA, RTW89_H2C.Class_Address_CAM, RTW89_H2C.Function_Address_CAM,
           RTW89_H2C.Station_CAM (Station), Done_Ack => True, Result => Result);
      Port_Config (Infrastructure => False, Interval => 0);
   end Disassociate;

   procedure Install_Key (DMA : RTW89_Bringup.Region; Index : RTW89_H2C.Security_CAM_Index;
                          Key : RTW89_H2C.Key_16; Result : out Outcome) is
   begin
      Result := Success;
      H2C (DMA, RTW89_H2C.Class_Security_CAM, RTW89_H2C.Function_Security_CAM,
           RTW89_H2C.Security_CAM (Index, Key), Done_Ack => False, Result => Result);
   end Install_Key;

   procedure Add_Station (DMA : RTW89_Bringup.Region; Address : RTW89_Efuse.MAC_Address;
                          Result : out Outcome) is
      Own : constant RTW89_H2C.Address := [for I in 1 .. 6 => Address (I - 1)];
   begin
      Result := Success;
      Port_Update;
      H2C (DMA, RTW89_H2C.Class_FW_Offload, RTW89_H2C.Function_MACID_Pause,
           RTW89_H2C.MACID_Unpause_Own, Done_Ack => False, Result => Result);
      H2C (DMA, RTW89_H2C.Class_Media_Report, RTW89_H2C.Function_Role_Maintain,
           RTW89_H2C.Role_Maintain_Create, Done_Ack => True, Result => Result);
      H2C (DMA, RTW89_H2C.Class_Media_Report, RTW89_H2C.Function_Join_Info,
           RTW89_H2C.Join_Info_Disconnected, Done_Ack => True, Result => Result);
      H2C (DMA, RTW89_H2C.Class_Address_CAM, RTW89_H2C.Function_Address_CAM,
           RTW89_H2C.Address_CAM (Own), Done_Ack => True, Result => Result);
      H2C (DMA, RTW89_H2C.Class_Frame_Exchange, RTW89_H2C.Function_CMAC_Table,
           RTW89_H2C.Default_CMAC_Table, Done_Ack => True, Result => Result);
   end Add_Station;

   ---------------------------------------------------------------------------
   Filter_Configurable : constant Unsigned_32 := not RX_MPDU_MAX_LEN_MASK;

   procedure Set_Filter (Value : Unsigned_32) is
   begin
      Write32 (RX_FLTR_OPT, (Read32 (RX_FLTR_OPT) and RX_MPDU_MAX_LEN_MASK) or (Value and Filter_Configurable));
   end Set_Filter;

   procedure Scan_Filter is
   begin
      Set_Filter (DEFAULT_AX_RX_FLTR and not (A_BCN_CHK_EN or A_BC or A_A1_MATCH));
   end Scan_Filter;

   procedure Default_Filter is
   begin
      Set_Filter (DEFAULT_AX_RX_FLTR);
   end Default_Filter;

end RTW89_MAC_Init;
