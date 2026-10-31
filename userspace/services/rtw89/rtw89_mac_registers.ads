------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  RTL8852B (AX) registers and values for the normal-mode MAC init, the
--  receive filter, the station port and PPDU status (NET-WIFI-001 W1).
--  The bring-up registers stay in RTW89_Registers.
--
--  @description
--  Hardware facts from Linux rtw89 reg.h, mac.c, mac.h, pci.c, rtw8852b.c
--  and rtw8852b_common.c (GPL-2.0 OR BSD-3-Clause, Copyright Realtek
--  Corporation), taken under the BSD-3-Clause option, resolved for the
--  8852B over PCIe in single-band (SCC) quota mode. Names follow rtw89's
--  R_AX_* / B_AX_* with the prefix dropped. Composite values carry the
--  rtw89 name of the value, or list the bits they are made of.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with RTW89_Registers; use RTW89_Registers;

package RTW89_MAC_Registers with Pure is

   ---------------------------------------------------------------------------
   --  Power-on additions at core start (rtw8852b_pwr_on_func trims, the
   --  BT scoreboard).
   ---------------------------------------------------------------------------
   VOL_L1_MASK        : constant Unsigned_32 := 16#0000_000F#;   --  SPS_DIG_ON_CTRL0 [3:0]
   VOL_L1_SHIFT       : constant := 0;
   VOL_L1_TRIM        : constant Unsigned_32 := 16#9#;
   VREFPFM_L_MASK     : constant Unsigned_32 := 16#03C0_0000#;   --  [25:22]
   VREFPFM_L_SHIFT    : constant := 22;
   VREFPFM_L_TRIM     : constant Unsigned_32 := 16#A#;
   OCP_L1_MASK        : constant Unsigned_32 := 16#0000_E000#;   --  [15:13]
   HCI_LDO_CTRL       : constant Register := 16#007A#;
   R_AX_VADJ_MASK     : constant Unsigned_16 := 16#000F#;
   VADJ_TRIM          : constant Unsigned_16 := 16#000A#;
   --  The physical efuse byte that says the power trims are calibrated
   --  (rtw8852bx_phycap_parsing_power_cal: PWR_K_CHK_OFFSET / _VALUE).
   POWER_K_CHECK_ADDRESS : constant := 16#5E9#;
   POWER_K_VALID         : constant Unsigned_8 := 16#AA#;
   SCOREBOARD_NOTIFY  : constant Register := 16#00AF#;           --  R_AX_SCOREBOARD + 3
   NOTIFY_TP_MAJOR    : constant Unsigned_8 := 16#81#;
   NOTIFY_PWR_MAJOR   : constant Unsigned_8 := 16#80#;

   ---------------------------------------------------------------------------
   --  enable_bb_rf (__rtw8852bx_mac_enable_bb_rf)
   ---------------------------------------------------------------------------
   ZCDC_H_ENABLE      : constant Unsigned_32 := 1;
   XTAL_SI_RFC_ON     : constant Unsigned_8 := 16#C7#;
   XTAL_SI_FULL_MASK  : constant Unsigned_8 := 16#FF#;
   PHYREG_SET         : constant Register := 16#8040#;
   PHYREG_SET_XYN_CYCLE : constant Unsigned_8 := 16#0E#;

   ---------------------------------------------------------------------------
   --  sys_init_ax
   ---------------------------------------------------------------------------
   --  MAC_FUNC_EN | DMAC_FUNC_EN | MAC_SEC_EN | DISPATCHER_EN | DLE_CPUIO_EN
   --  | PKT_IN_EN | DMAC_TBL_EN | PKT_BUF_EN | STA_SCH_EN | TXPKT_CTRL_EN
   --  | WD_RLS_EN | MPDU_PROC_EN | DMAC_CRPRT (dmac_func_en_ax).
   DMAC_FUNC_SYS_INIT : constant Unsigned_32 := 16#FB7D_0000#;
   --  MAC_SEC_CLK | BBRPT_CLK | DISPATCHER_CLK | DLE_CPUIO_CLK | PKT_IN_CLK
   --  | STA_SCH_CLK | TXPKT_CTRL_CLK | WD_RLS_CLK.
   DMAC_CLK_SYS_INIT  : constant Unsigned_32 := 16#0B1F_0000#;
   --  CMAC_CKEN | PHYINTF | CMAC_DMA | PTCLTOP | SCHEDULER | TMAC | RMAC.
   CK_EN_CMAC_ALL     : constant Unsigned_32 := 16#4000_003F#;
   --  CMAC_CRPRT | CMAC_EN | TXEN | RXEN | PHYINTF .. RMAC.
   CMAC_FUNC_ALL      : constant Unsigned_32 := 16#F000_003F#;
   DMAC_REQUIRED      : constant Unsigned_32 := 16#6000_0000#;   --  check_mac_en DMAC
   CMAC_REQUIRED      : constant Unsigned_32 := 16#4000_0000#;   --  check_mac_en CMAC0

   ---------------------------------------------------------------------------
   --  DLE, normal SCC quota (rtw8852b_dle_mem_pcie[SCC]: wde_size7,
   --  ple_size6, wde_qt7, ple_qt18 / ple_qt58)
   ---------------------------------------------------------------------------
   SCC_WDE_Linked     : constant := 510;
   SCC_WDE_Unlinked   : constant := 2;
   SCC_WDE_Page_Bytes : constant := 64;
   SCC_PLE_Linked     : constant := 496;
   WDE_QTA_HIF_SCC    : constant Unsigned_32 := 16#01BE_01BE#;   --  446/446
   WDE_QTA_WCPU_SCC   : constant Unsigned_32 := 16#0030_0030#;   --  48/48
   WDE_QTA_PKT_IN_SCC : constant Unsigned_32 := 16#0000_0000#;
   WDE_QTA_CPU_IO_SCC : constant Unsigned_32 := 16#0010_0010#;   --  16/16
   type PLE_Quota_Table is array (0 .. PLE_QTA_COUNT - 1) of Unsigned_32;
   --  cma0_tx, cma1_tx, c2h, h2c, wcpu, mpdu_proc, cma0_dma, cma1_dma,
   --  bb_rpt, wd_rel, cpu_io (min [11:0], max [27:16]).
   PLE_QTA_SCC : constant PLE_Quota_Table :=
     [16#0093_0093#, 16#0000_0000#, 16#0010_0010#, 16#0014_0014#, 16#009D_0011#,
      16#000D_000D#, 16#00E5_0059#, 16#0000_0000#, 16#00AC_0020#, 16#000E_000E#,
      16#0018_0008#];

   ---------------------------------------------------------------------------
   --  HCI flow control, normal mode (rtw8852b_hfc_param_ini_pcie[SCC])
   ---------------------------------------------------------------------------
   ACH0_PAGE_CTRL     : constant Register := 16#8A10#;
   PAGE_CTRL_STRIDE   : constant := 4;
   type HFC_Channel is record
      Index : Natural;          --  DMA channel (ACH0 .. 3, B0MGQ 8, B0HIQ 9)
      Value : Unsigned_32;      --  min [12:0] | max [28:16]
   end record;
   type HFC_Channel_Table is array (1 .. 6) of HFC_Channel;
   HFC_CHANNELS_SCC : constant HFC_Channel_Table :=
     [(0, 16#0155_0005#), (1, 16#0155_0005#), (2, 16#0156_0004#), (3, 16#0156_0004#),
      (8, 16#0156_0004#), (9, 16#0156_0004#)];
   PUB_PAGE_CTRL1     : constant Register := 16#8A90#;
   PUB_PAGE_G0_SCC    : constant Unsigned_32 := 16#0000_01BE#;   --  grp0 446, grp1 0
   WP_PAGE_CTRL2      : constant Register := 16#8AA4#;
   PREC_CH011_CH12    : constant Unsigned_32 := 16#0028_0002#;   --  ch011 2, h2c 40
   PUB_PAGE_CTRL2     : constant Register := 16#8A94#;
   PUB_PAGE_ALL_SCC   : constant Unsigned_32 := 16#0000_01BE#;   --  446
   WP_PAGE_CTRL1      : constant Register := 16#8AA0#;
   HCI_FC_MODE_MASK   : constant Unsigned_32 := 16#0000_0006#;   --  [2:1] POH = 0
   WD_FULL_COND_MASK  : constant Unsigned_32 := 16#0000_0030#;   --  [5:4]
   WD_FULL_COND_SHIFT : constant := 4;
   WD_FULL_COND_SCC   : constant Unsigned_32 := 1;
   WP_FULL_COND_MASK  : constant Unsigned_32 := 16#0000_03C0#;   --  [7:6] ch07, [9:8] ch811
   PUB_PAGE_INFO1     : constant Register := 16#8A98#;           --  g0_used [12:0], g1_used [28:16]
   PUB_PAGE_INFO2     : constant Register := 16#8A9C#;           --  pub_aval [12:0]
   PUB_PAGE_INFO3     : constant Register := 16#8A8C#;
   PAGE_FIELD_MASK    : constant Unsigned_32 := 16#1FFF#;
   PAGE_HIGH_SHIFT    : constant := 16;
   PUB_PAGES_SCC      : constant := 446;
   HFC_Settle_Us      : constant := 10;

   ---------------------------------------------------------------------------
   --  STA scheduler, MPDU processor, security engine
   ---------------------------------------------------------------------------
   SS_CTRL            : constant Register := 16#9E10#;
   SS_EN              : constant Unsigned_8 := 16#01#;
   SS_INIT_DONE_1     : constant Unsigned_32 := 2 ** 31;
   SS_WARM_INIT_FLG   : constant Unsigned_32 := 2 ** 29;
   SS_NONEMPTY_SS2FINFO_EN : constant Unsigned_32 := 2 ** 28;
   ACTION_FWD0        : constant Register := 16#9C04#;
   TRXCFG_MPDU_PROC_ACT_FRWD : constant Unsigned_32 := 16#02A9_5A95#;
   TF_FWD             : constant Register := 16#9C14#;
   TRXCFG_MPDU_PROC_TF_FRWD  : constant Unsigned_32 := 16#0000_AA55#;
   MPDU_PROC          : constant Register := 16#9C00#;
   APPEND_FCS         : constant Unsigned_32 := 2 ** 0;
   A_ICV_ERR          : constant Unsigned_32 := 2 ** 1;
   CUT_AMSDU_CTRL     : constant Register := 16#9C40#;
   TRXCFG_MPDU_PROC_CUT_CTRL : constant Unsigned_32 := 16#010E_05F0#;
   HW_RPT_FWD         : constant Register := 16#9C18#;
   FWD_PPDU_STAT_MASK : constant Unsigned_32 := 16#0000_0003#;
   PRPT_DEST_HOST     : constant Unsigned_32 := 1;
   SEC_ENG_CTRL       : constant Register := 16#9D00#;
   --  CLK_EN_CGCMP | CLK_EN_WAPI | CLK_EN_WEP_TKIP | SEC_TX_ENC | SEC_RX_DEC
   --  | MC_DEC | BC_DEC.
   SEC_ENG_ENABLES    : constant Unsigned_32 := 16#0000_070F#;
   TX_PARTIAL_MODE    : constant Unsigned_32 := 2 ** 11;
   SEC_MPDU_PROC      : constant Register := 16#9D04#;
   APPEND_MIC_ICV     : constant Unsigned_32 := 16#0000_0003#;

   ---------------------------------------------------------------------------
   --  CMAC 0 init (cmac_init_ax)
   ---------------------------------------------------------------------------
   PREBKF_CFG_1       : constant Register := 16#C33C#;
   SIFS_MACTXEN_T1_MASK : constant Unsigned_32 := 16#0000_007F#;
   SIFS_MACTXEN_T1    : constant Unsigned_32 := 16#47#;
   SCH_EXT_CTRL       : constant Register := 16#C3FC#;
   PORT_RST_TSF_ADV   : constant Unsigned_32 := 2 ** 1;
   CCA_CFG_0          : constant Register := 16#C340#;
   BTCCA_EN           : constant Unsigned_32 := 2 ** 5;
   PREBKF_CFG_0       : constant Register := 16#C338#;
   PREBKF_TIME_MASK   : constant Unsigned_32 := 16#0000_001F#;
   SCH_PREBKF_24US    : constant Unsigned_32 := 16#18#;
   ADDR_CAM_CTRL      : constant Register := 16#CE34#;
   --  ADDR_CAM_RANGE |= 0x7F, ADDR_CAM_CLR, ADDR_CAM_EN.
   ADDR_CAM_RESET     : constant Unsigned_32 := 16#007F_0101#;
   ADDR_CAM_CLR       : constant Unsigned_16 := 16#0100#;
   RX_FLTR_OPT        : constant Register := 16#CE20#;
   CTRL_FLTR          : constant Register := 16#CE24#;
   MGNT_FLTR          : constant Register := 16#CE28#;
   DATA_FLTR          : constant Register := 16#CE2C#;
   RX_FLTR_FRAME_TO_HOST : constant Unsigned_32 := 16#5555_5555#;
   DEFAULT_AX_RX_FLTR : constant Unsigned_32 := 16#0300_44BE#;
   A_A1_MATCH         : constant Unsigned_32 := 2 ** 1;
   A_BC               : constant Unsigned_32 := 2 ** 2;
   A_BCN_CHK_EN       : constant Unsigned_32 := 2 ** 7;
   RX_MPDU_MAX_LEN_MASK  : constant Unsigned_32 := 16#003F_0000#;
   RX_MPDU_MAX_LEN_SHIFT : constant := 16;
   RX_MPDU_MAX_LEN_SCC   : constant Unsigned_32 := 22;          --  x 512 bytes
   PLCP_HDR_FLTR      : constant Register := 16#CE04#;
   PLCP_HDR_CHECKS    : constant Unsigned_16 := 16#007F#;
   VHT_SU_SIGB_CRC_CHK : constant Unsigned_8 := 16#10#;
   CCA_CONTROL        : constant Register := 16#C390#;
   CCA_CONTROL_SET    : constant Unsigned_32 := 16#7121_00FF#;
   CCA_CONTROL_CLEAR  : constant Unsigned_32 := 16#8E1E_0100#;
   RSP_CHK_SIG        : constant Register := 16#CC00#;
   RSP_CHK_NAVS       : constant Unsigned_32 := 16#0038_0000#;
   TRXPTCL_RESP_0     : constant Register := 16#CC04#;
   RSP_CHK_CCAS       : constant Unsigned_32 := 16#1F80_0000#;
   WMAC_NAV_CTL       : constant Register := 16#CC80#;
   WMAC_NAV_ENABLES   : constant Unsigned_32 := 16#0403_0000#;
   WMAC_NAV_UPPER_MASK : constant Unsigned_32 := 16#0000_FF00#;
   WMAC_NAV_UPPER_SHIFT : constant := 8;
   NAV_25MS           : constant Unsigned_32 := 16#C4#;
   RX_SR_CTRL         : constant Register := 16#CE4A#;
   SR_EN              : constant Unsigned_8 := 16#01#;
   BSSID_SRC_CTRL     : constant Register := 16#CE4B#;
   PLCP_SRC_EN        : constant Unsigned_8 := 16#01#;
   MAC_LOOPBACK       : constant Register := 16#CC20#;
   MACLBK_EN          : constant Unsigned_32 := 2 ** 0;
   TCR0               : constant Register := 16#CA00#;
   TCR_UDF_THSD_MASK  : constant Unsigned_32 := 16#007F_0000#;
   TCR_UDF_THSD_SHIFT : constant := 16;
   TCR_UDF_THSD       : constant Unsigned_32 := 16#6#;
   TXD_FIFO_CTRL      : constant Register := 16#CA1C#;
   TXDFIFO_HIGH_MCS_THRE_MASK  : constant Unsigned_32 := 16#0000_F000#;
   TXDFIFO_HIGH_MCS_THRE_SHIFT : constant := 12;
   TXDFIFO_LOW_MCS_THRE_MASK   : constant Unsigned_32 := 16#0000_0F00#;
   TXDFIFO_LOW_MCS_THRE_SHIFT  : constant := 8;
   TXDFIFO_MCS_THRE   : constant Unsigned_32 := 16#7#;
   WMAC_SPEC_SIFS_CCK_MASK  : constant Unsigned_32 := 16#0000_00FF#;
   WMAC_SPEC_SIFS_CCK       : constant Unsigned_32 := 16#0A#;
   WMAC_SPEC_SIFS_OFDM_MASK : constant Unsigned_32 := 16#0000_FF00#;
   WMAC_SPEC_SIFS_OFDM_SHIFT : constant := 8;
   WMAC_SPEC_SIFS_OFDM_52B : constant Unsigned_32 := 16#11#;
   RXTRIG_TEST_USER_2 : constant Register := 16#CCB0#;
   RXTRIG_FCSCHK_EN   : constant Unsigned_32 := 2 ** 20;
   TRXPTCL_RRSR_CTL_0 : constant Register := 16#CC08#;
   WMAC_RESP_REF_RATE_SEL : constant Unsigned_32 := 2 ** 9;
   WMAC_RESP_RSC_MASK : constant Unsigned_32 := 16#0000_0C00#;
   WMAC_RESP_RSC_SHIFT : constant := 10;
   WMAC_RESP_RSC_8852B : constant Unsigned_32 := 2;
   RESPBA_CAM_CTRL    : constant Register := 16#CE3C#;
   BACAM_RST_MASK     : constant Unsigned_32 := 16#0000_0003#;
   BACAM_RST_ALL      : constant Unsigned_32 := 2;
   SSN_SEL            : constant Unsigned_8 := 16#04#;
   DLK_PROTECT_CTL    : constant Register := 16#CE02#;
   RX_DLK_DATA_TIME_MASK  : constant Unsigned_16 := 16#00F0#;
   RX_DLK_DATA_TIME_SHIFT : constant := 4;
   TRXCFG_RMAC_DATA_TO : constant Unsigned_16 := 15;
   RX_DLK_CCA_TIME_MASK   : constant Unsigned_16 := 16#FF00#;
   RX_DLK_CCA_TIME_SHIFT  : constant := 8;
   TRXCFG_RMAC_CCA_TO : constant Unsigned_16 := 32;
   RCR                : constant Register := 16#CE00#;
   CH_EN_MASK         : constant Unsigned_8 := 16#0F#;
   CH_EN_PRIMARY      : constant Unsigned_8 := 16#01#;
   TX_SUB_CARRIER_VALUE : constant Register := 16#C088#;
   TXSC_MASKS         : constant Unsigned_32 := 16#0000_0FFF#;   --  20M/40M/80M = 0
   PTCL_RRSR1         : constant Register := 16#C090#;
   RRSR_RATE_EN_MASK  : constant Unsigned_32 := 16#0000_0F00#;
   RRSR_RATE_EN_SHIFT : constant := 8;
   RRSR_OFDM_CCK_EN   : constant Unsigned_32 := 3;
   SIFS_SETTING       : constant Register := 16#C624#;
   HW_CTS2SELF_PKT_LEN_TH_MASK  : constant Unsigned_32 := 16#FF00_0000#;
   HW_CTS2SELF_PKT_LEN_TH_SHIFT : constant := 24;
   CTS2S_TH_1K        : constant Unsigned_32 := 4;
   HW_CTS2SELF_PKT_LEN_TH_TWW_MASK  : constant Unsigned_32 := 16#00FC_0000#;
   HW_CTS2SELF_PKT_LEN_TH_TWW_SHIFT : constant := 18;
   CTS2S_TH_SEC_256B  : constant Unsigned_32 := 1;
   HW_CTS2SELF_EN     : constant Unsigned_32 := 2 ** 16;
   PTCL_FSM_MON       : constant Register := 16#C6E8#;
   PTCL_TX_ARB_TO_THR_MASK : constant Unsigned_32 := 16#0000_003F#;
   PTCL_TO_2MS        : constant Unsigned_32 := 16#3F#;
   PTCL_TX_ARB_TO_MODE : constant Unsigned_32 := 2 ** 6;
   PTCL_COMMON_SETTING_0 : constant Register := 16#C600#;
   CMAC_TX_MODES      : constant Unsigned_8 := 16#03#;
   PTCL_TRIGGER_SS_ENS : constant Unsigned_8 := 16#1C#;
   PTCLRPT_FULL_HDL   : constant Register := 16#C660#;
   SPE_RPT_PATH_MASK  : constant Unsigned_32 := 16#0000_0030#;
   SPE_RPT_PATH_SHIFT : constant := 4;
   FWD_TO_WLCPU       : constant Unsigned_32 := 1;
   AGG_LEN_VHT_0      : constant Register := 16#C618#;
   AMPDU_MAX_LEN_VHT_MASK : constant Unsigned_32 := 16#000F_FFFF#;
   AMPDU_MAX_LEN_VHT  : constant Unsigned_32 := 16#3FF80#;
   RXDMA_CTRL_0       : constant Register := 16#C804#;
   RX_FULL_MODE       : constant Unsigned_8 := 16#3F#;
   AGG_LEN_HT_0       : constant Register := 16#C614#;
   RTS_TXTIME_TH_MASK : constant Unsigned_32 := 16#0000_FF00#;
   RTS_TXTIME_TH_SHIFT : constant := 8;
   RTS_TXTIME_TH_DEFAULT : constant Unsigned_32 := 2;           --  88 >> 5
   RTS_LEN_TH_MASK    : constant Unsigned_32 := 16#0000_00FF#;
   RTS_LEN_TH_DEFAULT : constant Unsigned_32 := 255;            --  4080 >> 4
   PPDU_STAT          : constant Register := 16#CE40#;
   --  PPDU_STAT_RPT_EN | APP_MAC_INFO_RPT | APP_PLCP_HDR_RPT | PPDU_STAT_RPT_CRC32.
   PPDU_STAT_ENABLES  : constant Unsigned_32 := 16#0000_002B#;

   ---------------------------------------------------------------------------
   --  Error interrupt masks (enable_imr_ax, err_imr_ctrl_ax): which MAC
   --  errors reach the firmware. Pairs of (register, clear, set).
   ---------------------------------------------------------------------------
   type IMR_Step is record
      Where : Register;
      Clear : Unsigned_32;
      Set   : Unsigned_32;
   end record;
   type IMR_Table is array (Positive range <>) of IMR_Step;
   DMAC_IMRS : constant IMR_Table :=
     [(16#9430#, 16#0000_3337#, 16#0000_3327#),    --  WDRLS_ERR_IMR
      (16#9D1C#, 16#0000_0000#, 16#0000_0008#),    --  SEC_DEBUG IMR_ERROR
      (16#9BF4#, 16#0000_003E#, 16#0000_0000#),    --  MPDU_TX_ERR_IMR
      (16#9CF4#, 16#0000_000B#, 16#0000_0000#),    --  MPDU_RX_ERR_IMR
      (16#9EF0#, 16#0000_0007#, 16#0000_0007#),    --  STA_SCHEDULER_ERR_IMR
      (16#9F1C#, 16#0000_030F#, 16#0000_0101#),    --  TXPKTCTL_ERR_IMR_ISR
      (16#9F2C#, 16#0000_030F#, 16#0000_0303#),    --  TXPKTCTL_ERR_IMR_ISR_B1
      (16#8C38#, 16#070F_F0FF#, 16#070F_F0FF#),    --  WDE_ERR_IMR
      (16#9038#, 16#070F_F0FF#, 16#070F_F0DF#),    --  PLE_ERR_IMR
      (16#9A20#, 16#0000_0000#, 16#0000_0001#),    --  PKTIN_ERR_IMR
      (16#8850#, 16#FF0F_FFFF#, 16#0C00_0161#),    --  HOST_DISPATCHER_ERR_IMR
      (16#8854#, 16#FF07_FFFF#, 16#0400_0062#),    --  CPU_DISPATCHER_ERR_IMR
      (16#8858#, 16#3F03_1F1F#, 16#0000_0000#),    --  OTHER_DISPATCHER_ERR_IMR
      (16#9840#, 16#0000_1111#, 16#0000_1111#),    --  CPUIO_ERR_IMR
      (16#960C#, 16#0000_0000#, 16#0000_0001#),    --  BBRPT_COM_ERR_IMR_ISR
      (16#962C#, 16#0000_00FF#, 16#0000_0000#),    --  BBRPT_CHINFO_ERR_IMR_ISR
      (16#963C#, 16#0000_0000#, 16#0000_0001#),    --  BBRPT_DFS_ERR_IMR_ISR
      (16#966C#, 16#0000_0000#, 16#0000_0001#)];   --  LA_ERRFLAG
   CMAC_IMRS : constant IMR_Table :=
     [(16#C3E8#, 16#0000_0003#, 16#0000_0001#),    --  SCHEDULE_ERR_IMR
      (16#C6C0#, 16#FFFF_FFFF#, 16#1080_0001#),    --  PTCL_IMR0
      (16#C800#, 16#0080_C000#, 16#0000_C000#),    --  DLE_CTRL
      (16#CEF4#, 16#000F_F000#, 16#000E_4000#),    --  RMAC_ERR_ISR
      (16#CCEC#, 16#0000_0780#, 16#0000_0780#)];   --  TMAC_ERR_IMR_ISR
   DMAC_ERR_IMR       : constant Register := 16#8520#;
   CMAC_ERR_IMR       : constant Register := 16#C160#;
   ERR_IMR_ALL        : constant Unsigned_32 := 16#FFFF_FFFF#;

   ---------------------------------------------------------------------------
   --  Host release reports (set_host_rpr_ax, PCIe)
   ---------------------------------------------------------------------------
   WDRLS_CFG          : constant Register := 16#9408#;
   WDRLS_MODE_MASK    : constant Unsigned_32 := 16#0000_0003#;
   RPR_MODE_POH       : constant Unsigned_32 := 0;
   RLSRPT0_CFG0       : constant Register := 16#9410#;
   RLSRPT0_FLTR_MAP_MASK : constant Unsigned_32 := 16#0F00_0000#;
   RLSRPT0_CFG1       : constant Register := 16#9414#;
   RLSRPT0_AGGNUM_MASK : constant Unsigned_32 := 16#0000_00FF#;
   RLSRPT0_AGGNUM     : constant Unsigned_32 := 30;
   RLSRPT0_TO_MASK    : constant Unsigned_32 := 16#00FF_0000#;
   RLSRPT0_TO_SHIFT   : constant := 16;
   RLSRPT0_TO         : constant Unsigned_32 := 255;

   ---------------------------------------------------------------------------
   --  PCIe post-init (rtw89_pci_ops_mac_post_init_ax)
   ---------------------------------------------------------------------------
   LTR_CTRL_0         : constant Register := 16#8410#;
   LTR_CTRL_1         : constant Register := 16#8414#;
   LTR_IDLE_LATENCY   : constant Register := 16#8418#;
   LTR_ACTIVE_LATENCY : constant Register := 16#841C#;
   LTR_HW_EN_AND_WD_TIMER : constant Unsigned_32 := 16#0000_0043#;  --  HW_EN | EN | WD_NOEMP_CHK
   LTR_SPACE_IDX_MASK : constant Unsigned_32 := 16#0000_3000#;
   LTR_SPACE_IDX_SHIFT : constant := 12;
   LTR_SPACE_IDX      : constant Unsigned_32 := 2;
   LTR_IDLE_TIMER_MASK : constant Unsigned_32 := 16#0000_0700#;
   LTR_IDLE_TIMER_SHIFT : constant := 8;
   LTR_IDLE_TIMER     : constant Unsigned_32 := 7;
   LTR_RX0_TH_MASK    : constant Unsigned_32 := 16#0000_0FFF#;
   LTR_RX1_TH_MASK    : constant Unsigned_32 := 16#0FFF_0000#;
   LTR_RX1_TH_SHIFT   : constant := 16;
   LTR_RX_TH          : constant Unsigned_32 := 16#28#;
   LTR_IDLE_VALUE     : constant Unsigned_32 := 16#9003_9003#;
   LTR_ACTIVE_VALUE   : constant Unsigned_32 := 16#880B_880B#;
   --  PCI Express capability: Device Control 2 at capability + 0x28, LTR
   --  mechanism enable bit 10 (byte + 1, bit 2).
   PCI_CAPABILITY_POINTER : constant := 16#34#;
   PCI_CAP_ID_EXP     : constant Unsigned_8 := 16#10#;
   PCI_EXP_DEVCTL2_HIGH : constant := 16#29#;
   PCI_EXP_DEVCTL2_LTR_EN_HIGH : constant Unsigned_8 := 2 ** 2;
   STOP_TX_ACH0_TO_ACH3_CH8_CH9 : constant Unsigned_32 := 16#0007_0F00#;

   ---------------------------------------------------------------------------
   --  RX ring indexes and status (polled; no interrupt is enabled)
   ---------------------------------------------------------------------------
   PCIE_HISR00        : constant Register := 16#10B4#;
   HISR00_RXQ_ACK     : constant Unsigned_32 := 16#0008_0003#;   --  RXDMA | RXP1DMA | RDU
   HISR00_RPQ_ACK     : constant Unsigned_32 := 16#0010_0004#;   --  RPQDMA | RPQBD_FULL
   HISR0              : constant Register := 16#01A4#;
   HALT_C2H_INT       : constant Unsigned_32 := 2 ** 21;

   ---------------------------------------------------------------------------
   --  Station port 0, not associated (rtw89_mac_port_update, NO_LINK)
   ---------------------------------------------------------------------------
   PORT_CFG_P0        : constant Register := 16#C400#;
   RXBCN_RPT_EN       : constant Unsigned_32 := 2 ** 0;
   TXBCN_RPT_EN       : constant Unsigned_32 := 2 ** 1;
   PORT_FUNC_EN       : constant Unsigned_32 := 2 ** 2;
   TSF_UDT_EN         : constant Unsigned_32 := 2 ** 3;
   RX_BSSID_FIT_EN    : constant Unsigned_32 := 2 ** 4;
   NET_TYPE_MASK      : constant Unsigned_32 := 16#0000_0C00#;
   NET_TYPE_SHIFT     : constant := 10;
   NET_TYPE_INFRA     : constant Unsigned_32 := 2;          --  RTW89_NET_TYPE_INFRA
   TSFTR_RST          : constant Unsigned_32 := 2 ** 5;
   BCN_CNT_TMR_P0     : constant Register := 16#C434#;
   BCNTX_EN           : constant Unsigned_32 := 2 ** 12;
   TBTT_PROHIB_EN     : constant Unsigned_32 := 2 ** 13;
   BRK_SETUP          : constant Unsigned_32 := 2 ** 16;
   TBTT_PROHIB_P0     : constant Register := 16#C404#;
   TBTT_SETUP_MASK    : constant Unsigned_32 := 16#0000_00FF#;
   BCN_SETUP_DEF      : constant Unsigned_32 := 2;
   TBTT_HOLD_MASK     : constant Unsigned_32 := 16#0FFF_0000#;
   TBTT_HOLD_SHIFT    : constant := 16;
   BCN_HOLD_DEF       : constant Unsigned_32 := 200;
   BCN_AREA_P0        : constant Register := 16#C408#;
   BCN_MSK_AREA_MASK  : constant Unsigned_32 := 16#0FFF_0000#;
   BCN_EARLY_P0       : constant Register := 16#C40C#;
   BCN_ERLY_MASK      : constant Unsigned_32 := 16#0000_0FFF#;
   BCN_ERLY_DEF       : constant Unsigned_32 := 160;
   TBTT_EARLY_P0      : constant Register := 16#C40E#;
   TBTT_EARLY_MASK    : constant Unsigned_16 := 16#0FFF#;
   TBTT_EARLY_DEF     : constant Unsigned_16 := 5;
   TBTT_AGG_P0        : constant Register := 16#C412#;
   TBTT_AGG_NUM_MASK  : constant Unsigned_16 := 16#FF00#;
   TBTT_AGG_NUM_SHIFT : constant := 8;
   TBTT_AGG_DEF       : constant Unsigned_16 := 1;
   BCN_SPACE_P0       : constant Register := 16#C414#;
   BCN_SPACE_MASK     : constant Unsigned_32 := 16#0000_FFFF#;
   BCN_INTERVAL_DEF   : constant Unsigned_32 := 100;
   DTIM_CTRL_P0       : constant Register := 16#C426#;
   DTIM_NUM_MASK      : constant Unsigned_16 := 16#FF00#;
   HIQ_WIN_P0         : constant Register := 16#C590#;
   MD_TSFT_STMP_CTL   : constant Register := 16#CA08#;
   UPD_HGQMD_AND_TIMEOUT : constant Unsigned_8 := 16#03#;
   MBSSID_DROP_0      : constant Register := 16#C63C#;
   MBSSID_DROP_PORT0  : constant Unsigned_32 := 16#0001_0001#;   --  bit 16 and bit 0
   PTCL_BSS_COLOR_0   : constant Register := 16#C6A0#;
   BSS_COLOR_MASK     : constant Unsigned_32 := 16#0000_003F#;
   MBSSID_CTRL        : constant Register := 16#C568#;
   P0MB_ALL           : constant Unsigned_32 := 16#00FF_FFFE#;
   BCN_PSR_RPT_P0     : constant Register := 16#CE84#;
   BCAID_P0_MASK      : constant Unsigned_32 := 16#0000_07FF#;
   BCN_ERLY_SET_DLY_US : constant := 20;

end RTW89_MAC_Registers;
