------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  RTL8852B (802.11ax, AX generation) register map used by the bring-up
--  driver: power, crystal serial interface, efuse, DMAC data-link engine,
--  HCI flow control, PCIe host interface, MDIO and DBI, WCPU control.
--
--  @description
--  Hardware facts from Linux rtw89 reg.h / pci.h / mac.c / pci.c
--  (GPL-2.0 OR BSD-3-Clause, Copyright Realtek Corporation), taken under
--  the BSD-3-Clause option. Names follow rtw89's R_AX_* / B_AX_* with the
--  prefix dropped so the driver can be read against it. Offsets are bytes
--  into BAR2.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package RTW89_Registers with Pure is

   --  BAR2 covers at least the CMAC range (R_AX_CMAC_REG_END = 0xFFFF).
   Register_Window_Bytes : constant := 16#1_0000#;
   subtype Register is Unsigned_32 range 0 .. Register_Window_Bytes - 1;

   --  Read results that mean "no device" or "block not clocked".
   All_Ones  : constant Unsigned_32 := 16#FFFF_FFFF#;
   Dead      : constant Unsigned_32 := 16#DEAD_BEEF#;  --  RTW89_R32_DEAD
   Config_EA : constant Unsigned_32 := 16#EAEA_EAEA#;  --  RTW89_R32_EA

   ---------------------------------------------------------------------------
   --  System / power (SYSON)
   ---------------------------------------------------------------------------
   SYS_ISO_CTRL           : constant Register := 16#0000#;
   ISO_EB2CORE            : constant Unsigned_32 := 2 ** 8;
   PWC_EV2EF_B14          : constant Unsigned_32 := 2 ** 14;
   PWC_EV2EF_B15          : constant Unsigned_32 := 2 ** 15;

   SYS_PW_CTRL            : constant Register := 16#0004#;
   APFN_ONMAC             : constant Unsigned_32 := 2 ** 8;
   APFM_SWLPS             : constant Unsigned_32 := 2 ** 10;
   AFSM_WLSUS_EN          : constant Unsigned_32 := 2 ** 11;
   AFSM_PCIE_SUS_EN       : constant Unsigned_32 := 2 ** 12;
   PSUS_OFF_CAPC_EN       : constant Unsigned_32 := 2 ** 14;
   APDM_HPDN              : constant Unsigned_32 := 2 ** 15;
   EN_WLON                : constant Unsigned_32 := 2 ** 16;
   RDY_SYSPWR             : constant Unsigned_32 := 2 ** 17;
   DIS_WLBT_PDNSUSEN_SOPC : constant Unsigned_32 := 2 ** 18;

   APFM_OFFMAC            : constant Unsigned_32 := 2 ** 9;

   SYS_FUNC_EN            : constant Register := 16#0002#;  --  8-bit
   FEN_BBRSTB             : constant Unsigned_8 := 2 ** 0;
   FEN_BB_GLB_RSTN        : constant Unsigned_8 := 2 ** 1;

   SYS_CLK_CTRL           : constant Register := 16#0008#;
   CPU_CLK_EN             : constant Unsigned_32 := 2 ** 14;

   SYS_SWR_CTRL1          : constant Register := 16#0010#;
   SYM_CTRL_SPS_PWMFREQ   : constant Unsigned_32 := 2 ** 10;

   SYS_WL_EFUSE_CTRL      : constant Register := 16#000A#;  --  16-bit
   AUTOLOAD_SUS           : constant Unsigned_16 := 2 ** 5;

   SYS_ADIE_PAD_PWR_CTRL  : constant Register := 16#0018#;
   SYM_PADPDN_WL_RFC_1P3  : constant Unsigned_32 := 2 ** 5;
   SYM_PADPDN_WL_PTA_1P3  : constant Unsigned_32 := 2 ** 6;

   AFE_LDO_CTRL           : constant Register := 16#0020#;
   AON_OFF_PC_EN          : constant Unsigned_32 := 2 ** 23;

   EFUSE_CTRL             : constant Register := 16#0030#;
   EF_DATA_MASK           : constant Unsigned_32 := 16#0000_00FF#;
   EF_ADDR_SHIFT          : constant := 16;
   EF_ADDR_MASK           : constant Unsigned_32 := 16#07FF_0000#;  --  [26:16]
   EF_RDY                 : constant Unsigned_32 := 2 ** 29;

   EFUSE_CTRL_1_V1        : constant Register := 16#0038#;
   EF_BURST               : constant Unsigned_32 := 2 ** 19;

   SYS_SDIO_CTRL          : constant Register := 16#0070#;
   PCIE_CALIB_EN_V1       : constant Unsigned_32 := 2 ** 12;
   PCIE_DIS_WLSUS_AFT_PDN : constant Unsigned_32 := 2 ** 14;
   PCIE_DIS_L2_CTRL_LDO_HCI : constant Unsigned_32 := 2 ** 15;

   HCI_OPT_CTRL           : constant Register := 16#0074#;
   BIT_WAKE_CTRL          : constant Unsigned_32 := 2 ** 5;

   PLATFORM_ENABLE        : constant Register := 16#0088#;
   PLATFORM_EN            : constant Unsigned_32 := 2 ** 0;
   WCPU_EN                : constant Unsigned_32 := 2 ** 1;
   APB_WRAP_EN            : constant Unsigned_32 := 2 ** 2;

   WLLPS_CTRL             : constant Register := 16#0090#;
   DIS_WLBT_LPSEN_LOPC    : constant Unsigned_32 := 2 ** 1;
   SW_LPS_OPTION          : constant Unsigned_32 := 16#0001_A0B2#;

   PMC_DBG_CTRL2          : constant Register := 16#00CC#;
   SYSON_DIS_PMCR_AX_WRMSK : constant Unsigned_32 := 2 ** 2;

   SYS_CFG1               : constant Register := 16#00F0#;
   CHIP_VER_SHIFT         : constant := 12;
   CHIP_VER_MASK          : constant Unsigned_32 := 16#0000_F000#;  --  [15:12]

   SPS_DIG_ON_CTRL0       : constant Register := 16#0200#;
   REG_ZCDC_H_MASK        : constant Unsigned_32 := 16#0006_0000#;  --  [18:17]
   REG_ZCDC_H_SHIFT       : constant := 17;
   REG_ZCDC_H_POWER_OFF   : constant Unsigned_32 := 3;
   SPS_ANA_ON_CTRL2       : constant Register := 16#0228#;  --  16-bit
   RTL8852B_DEFAULT_SPS_ANA : constant Unsigned_16 := 16#4F42#;

   WLAN_XTAL_SI_CTRL      : constant Register := 16#0270#;
   XTAL_SI_ADDR_SHIFT     : constant := 0;   --  [7:0]
   XTAL_SI_DATA_SHIFT     : constant := 8;   --  [15:8]
   XTAL_SI_BITMASK_SHIFT  : constant := 16;  --  [23:16]
   XTAL_SI_MODE_SHIFT     : constant := 24;  --  [25:24]
   XTAL_SI_CMD_POLL       : constant Unsigned_32 := 2 ** 31;
   XTAL_SI_BYTE_MASK      : constant Unsigned_32 := 16#FF#;
   XTAL_SI_NORMAL_WRITE   : constant Unsigned_32 := 0;
   XTAL_SI_NORMAL_READ    : constant Unsigned_32 := 1;

   WLRF_CTRL              : constant Register := 16#02F0#;
   AFC_AFEDIG             : constant Unsigned_32 := 2 ** 17;

   EECS_EESK_FUNC_SEL     : constant Register := 16#02D8#;
   PINMUX_EESK_FUNC_SEL_MASK : constant Unsigned_32 := 16#0000_00F0#;  --  [7:4]
   PINMUX_EESK_FUNC_SEL_SHIFT : constant := 4;
   PINMUX_EESK_FUNC_SEL_BT_LOG : constant Unsigned_32 := 1;

   IC_PWR_STATE           : constant Register := 16#03F0#;
   WLMAC_PWR_STE_SHIFT    : constant := 8;
   WLMAC_PWR_STE_MASK     : constant Unsigned_32 := 16#0000_0300#;  --  [9:8]
   MAC_AX_MAC_OFF         : constant Unsigned_32 := 0;
   MAC_AX_MAC_ON          : constant Unsigned_32 := 1;

   SPS_DIG_OFF_CTRL0      : constant Register := 16#0400#;
   C1_L1_MASK             : constant Unsigned_32 := 16#03#;  --  [1:0]
   C1_L1_SHIFT            : constant := 0;
   C3_L1_MASK             : constant Unsigned_32 := 16#30#;  --  [5:4]
   C3_L1_SHIFT            : constant := 4;

   ---------------------------------------------------------------------------
   --  Crystal serial interface: indirect analog registers (reg.h enum
   --  xtal_si_offset), reached through WLAN_XTAL_SI_CTRL.
   ---------------------------------------------------------------------------
   subtype XTAL_Offset is Unsigned_8;
   XTAL_SI_XTAL_XMD_2     : constant XTAL_Offset := 16#24#;
   XTAL_SI_LDO_LPS        : constant Unsigned_8 := 16#70#;  --  [6:4]
   XTAL_SI_XTAL_XMD_4     : constant XTAL_Offset := 16#26#;
   XTAL_SI_LPS_CAP        : constant Unsigned_8 := 16#0F#;  --  [3:0]
   XTAL_SI_CV             : constant XTAL_Offset := 16#41#;
   XTAL_SI_ACV_MASK       : constant Unsigned_8 := 16#0F#;
   XTAL_SI_WL_RFC_S0      : constant XTAL_Offset := 16#80#;
   XTAL_SI_WL_RFC_S1      : constant XTAL_Offset := 16#81#;
   XTAL_SI_RF00           : constant Unsigned_8 := 2 ** 0;
   XTAL_SI_RF10           : constant Unsigned_8 := 2 ** 0;
   XTAL_SI_ANAPAR_WL      : constant XTAL_Offset := 16#90#;
   XTAL_SI_PON_WEI        : constant Unsigned_8 := 2 ** 0;
   XTAL_SI_PON_EI         : constant Unsigned_8 := 2 ** 1;
   XTAL_SI_OFF_WEI        : constant Unsigned_8 := 2 ** 2;
   XTAL_SI_OFF_EI         : constant Unsigned_8 := 2 ** 3;
   XTAL_SI_RFC2RF         : constant Unsigned_8 := 2 ** 4;
   XTAL_SI_SHDN_WL        : constant Unsigned_8 := 2 ** 5;
   XTAL_SI_GND_SHDN_WL    : constant Unsigned_8 := 2 ** 6;
   XTAL_SI_SRAM2RFC       : constant Unsigned_8 := 2 ** 7;
   XTAL_SI_SRAM_CTRL      : constant XTAL_Offset := 16#A1#;
   XTAL_SI_SRAM_DIS       : constant Unsigned_8 := 2 ** 1;

   ---------------------------------------------------------------------------
   --  PCIe host interface (HCI)
   ---------------------------------------------------------------------------
   PCIE_INIT_CFG1         : constant Register := 16#1000#;
   RST_BDRAM              : constant Unsigned_32 := 2 ** 3;
   PCIE_MAX_TXDMA_MASK    : constant Unsigned_32 := 16#0000_0700#;  --  [10:8]
   PCIE_MAX_TXDMA_SHIFT   : constant := 8;
   TXHCI_EN               : constant Unsigned_32 := 2 ** 11;
   LATENCY_CONTROL        : constant Unsigned_32 := 2 ** 12;
   RXHCI_EN               : constant Unsigned_32 := 2 ** 13;
   PCIE_MAX_RXDMA_MASK    : constant Unsigned_32 := 16#0001_C000#;  --  [16:14]
   PCIE_MAX_RXDMA_SHIFT   : constant := 14;
   RXBD_MODE              : constant Unsigned_32 := 2 ** 18;
   PCIE_TXRST_KEEP_REG    : constant Unsigned_32 := 2 ** 22;
   PCIE_RXRST_KEEP_REG    : constant Unsigned_32 := 2 ** 23;
   TX_BURST_2048B         : constant Unsigned_32 := 7;  --  MAC_AX_TX_BURST_2048B
   RX_BURST_128B          : constant Unsigned_32 := 3;  --  MAC_AX_RX_BURST_128B

   PCIE_INIT_CFG2         : constant Register := 16#1004#;
   WD_ITVL_ACT_MASK       : constant Unsigned_32 := 16#000F_0000#;  --  [19:16]
   WD_ITVL_ACT_SHIFT      : constant := 16;
   WD_ITVL_IDLE_MASK      : constant Unsigned_32 := 16#0F00_0000#;  --  [27:24]
   WD_ITVL_IDLE_SHIFT     : constant := 24;
   WD_DMA_INTVL_256NS     : constant Unsigned_32 := 1;

   PCIE_PS_CTRL           : constant Register := 16#1008#;
   L1OFF_PWR_OFF_EN       : constant Unsigned_32 := 2 ** 5;

   PCIE_DMA_STOP1         : constant Register := 16#1010#;
   STOP_ACH0_TO_ACH3      : constant Unsigned_32 := 16#0000_0F00#;  --  [11:8]
   STOP_CH8               : constant Unsigned_32 := 2 ** 16;
   STOP_CH9               : constant Unsigned_32 := 2 ** 17;
   STOP_CH12              : constant Unsigned_32 := 2 ** 18;
   STOP_WPDMA             : constant Unsigned_32 := 2 ** 19;
   STOP_PCIEIO            : constant Unsigned_32 := 2 ** 20;
   TX_STOP1_MASK_V1       : constant Unsigned_32 :=
     STOP_ACH0_TO_ACH3 or STOP_CH8 or STOP_CH9 or STOP_CH12;

   TXBD_RWPTR_CLR1        : constant Register := 16#1014#;
   CLR_TX_IDX_8852B       : constant Unsigned_32 := 16#0000_070F#;  --  ACH0..3, CH8, CH9, CH12
   RXBD_RWPTR_CLR         : constant Register := 16#1018#;
   CLR_RXQ_RPQ_IDX        : constant Unsigned_32 := 16#0000_0003#;

   PCIE_DMA_BUSY1         : constant Register := 16#101C#;
   DMA_BUSY1_CHECK_V1     : constant Unsigned_32 := 16#0007_0F00#;
   RXQ_RPQ_BUSY           : constant Unsigned_32 := 16#0000_0003#;

   --  Ring registers: element count (16-bit), index (host [11:0] written
   --  with a 16-bit store, hardware [27:16]), BD RAM partition, base.
   RING_HOST_INDEX_MASK   : constant Unsigned_32 := 16#0000_0FFF#;
   RING_HW_INDEX_SHIFT    : constant := 16;
   RING_HW_INDEX_MASK     : constant Unsigned_32 := 16#0FFF_0000#;

   type TX_Channel is (ACH0, ACH1, ACH2, ACH3, CH8, CH9, CH12);
   type RX_Channel is (RXQ, RPQ);
   type TX_Register_Set is record
      Count, Index, BD_RAM, Base_Low, Base_High : Register;
      BD_RAM_Value : Unsigned_32;   --  start index | max << 8 | min << 16
   end record;
   type RX_Register_Set is record
      Count, Index, Base_Low, Base_High : Register;
   end record;
   TX_Rings : constant array (TX_Channel) of TX_Register_Set :=
     [ACH0 => (16#1024#, 16#1058#, 16#1200#, 16#1110#, 16#1114#, 16#0002_0500#),
      ACH1 => (16#1026#, 16#105C#, 16#1204#, 16#1118#, 16#111C#, 16#0002_0505#),
      ACH2 => (16#1028#, 16#1060#, 16#1208#, 16#1120#, 16#1124#, 16#0002_050A#),
      ACH3 => (16#102A#, 16#1064#, 16#120C#, 16#1128#, 16#112C#, 16#0002_050F#),
      CH8  => (16#1034#, 16#1078#, 16#1220#, 16#1150#, 16#1154#, 16#0001_0414#),
      CH9  => (16#1036#, 16#107C#, 16#1224#, 16#1158#, 16#115C#, 16#0001_0418#),
      CH12 => (16#1038#, 16#1080#, 16#1228#, 16#1160#, 16#1164#, 16#0001_041C#)];
   RX_Rings : constant array (RX_Channel) of RX_Register_Set :=
     [RXQ => (16#1020#, 16#1050#, 16#1100#, 16#1104#),
      RPQ => (16#1022#, 16#1054#, 16#1108#, 16#110C#)];

   DBI_FLAG               : constant Register := 16#1090#;  --  16-bit address/enables
   DBI_FLAG_COMMAND       : constant Register := 16#1092#;  --  8-bit
   DBI_WDATA              : constant Register := 16#1094#;
   DBI_RDATA              : constant Register := 16#1098#;
   DBI_ADDR_MASK          : constant Unsigned_16 := 16#0FFC#;
   DBI_WREN_SHIFT         : constant := 12;
   DBI_WRITE_FLAG         : constant Unsigned_8 := 16#01#;
   DBI_READ_FLAG          : constant Unsigned_8 := 16#02#;

   MDIO_CFG               : constant Register := 16#10A0#;  --  16-bit
   MDIO_WDATA             : constant Register := 16#10A4#;
   MDIO_RDATA             : constant Register := 16#10A6#;
   MDIO_ADDR_MASK         : constant Unsigned_16 := 16#001F#;
   MDIO_WFLAG             : constant Unsigned_16 := 2 ** 8;
   MDIO_RFLAG             : constant Unsigned_16 := 2 ** 9;
   MDIO_PAGE_SHIFT        : constant := 12;
   MDIO_PAGE_MASK         : constant Unsigned_16 := 16#3000#;
   MDIO_HIGH_PAGE_FIRST   : constant Unsigned_8 := 16#20#;

   PCIE_DBG_CTRL          : constant Register := 16#11C0#;
   ASFF_FULL_NO_STK_AND_EN_STUCK_DBG : constant Unsigned_32 := 16#0000_0003#;

   LBC_WATCHDOG           : constant Register := 16#11D8#;
   LBC_EN_AND_FLAG        : constant Unsigned_32 := 16#0000_0003#;
   LBC_TIMER_MASK         : constant Unsigned_32 := 16#0000_00F0#;
   LBC_TIMER_SHIFT        : constant := 4;
   LBC_TMR_2MS            : constant Unsigned_32 := 8;

   PCIE_EXP_CTRL          : constant Register := 16#13F0#;
   EXP_CTRL_DBG_MASK      : constant Unsigned_32 := 16#0000_0003#;
   EXP_CTRL_DBG_VALUE     : constant Unsigned_32 := 1;
   SIC_EN_FORCE_CLKREQ    : constant Unsigned_32 := 2 ** 4;
   MAX_TAG_NUM_MASK       : constant Unsigned_32 := 16#0007_0000#;
   MAX_TAG_NUM_SHIFT      : constant := 16;
   TAG_NUM_8              : constant Unsigned_32 := 7;

   --  PCIe PHY (MDIO) registers and fields (pci.h).
   RAC_REG_REV2           : constant Unsigned_8 := 16#1B#;
   BAC_CMU_EN_DLY_MASK    : constant Unsigned_16 := 16#F000#;
   PCIE_DPHY_DLY_25US     : constant Unsigned_16 := 16#1000#;  --  1 << 12
   RAC_REG_FLD_0          : constant Unsigned_8 := 16#1D#;
   BAC_AUTOK_N_MASK       : constant Unsigned_16 := 16#000C#;
   PCIE_AUTOK_4           : constant Unsigned_16 := 16#000C#;  --  3 << 2
   RAC_CTRL_PPR_V1        : constant Unsigned_8 := 16#30#;
   CALIB_EN               : constant Unsigned_16 := 2 ** 13;

   --  Device configuration space reached through DBI (extended space).
   PCIE_PHY_RATE          : constant Unsigned_16 := 16#0082#;
   PCIE_PHY_RATE_MASK     : constant Unsigned_8 := 16#03#;
   PCIE_PHY_RATE_GEN1     : constant Unsigned_8 := 1;
   PCIE_PHY_RATE_GEN2     : constant Unsigned_8 := 2;
   PCIE_L1_CTRL           : constant Unsigned_16 := 16#0719#;
   PCIE_BIT_L1            : constant Unsigned_8 := 2 ** 3;
   PCIE_BIT_EN_64BITS     : constant Unsigned_8 := 2 ** 5;

   --  Interrupt masks (all written 0 before devmgr unmasks the MSI: the
   --  bring-up driver polls).
   HIMR0                  : constant Register := 16#01A0#;
   PCIE_HIMR00            : constant Register := 16#10B0#;
   PCIE_HIMR10            : constant Register := 16#13B0#;

   ---------------------------------------------------------------------------
   --  WCPU / firmware download
   ---------------------------------------------------------------------------
   WCPU_FW_CTRL           : constant Register := 16#01E0#;
   WCPU_FWDL_EN           : constant Unsigned_32 := 2 ** 0;
   H2C_PATH_RDY           : constant Unsigned_32 := 2 ** 1;
   FWDL_PATH_RDY          : constant Unsigned_32 := 2 ** 2;
   WCPU_FWDL_STS_SHIFT    : constant := 5;
   WCPU_FWDL_STS_MASK     : constant Unsigned_32 := 16#0000_00E0#;  --  [7:5]
   --  enum rtw89_fw_dl_status, R_AX_WCPU_FW_CTRL[7:5].
   type Download_Status is
     (Initial_State, Download_Ongoing, Checksum_Fail, Security_Fail,
      Cut_Not_Match, Reserved, WCPU_Download_Ready, WCPU_Firmware_Ready);
   function Name (Status : Download_Status) return String is
     (case Status is
        when Initial_State       => "initial",
        when Download_Ongoing    => "ongoing",
        when Checksum_Fail       => "checksum-fail",
        when Security_Fail       => "security-fail",
        when Cut_Not_Match       => "cut-not-match",
        when Reserved            => "reserved",
        when WCPU_Download_Ready => "wcpu-download-ready",
        when WCPU_Firmware_Ready => "wcpu-firmware-ready");

   HALT_H2C_CTRL          : constant Register := 16#0160#;
   HALT_C2H_CTRL          : constant Register := 16#0164#;
   HALT_H2C               : constant Register := 16#0168#;
   HALT_C2H               : constant Register := 16#016C#;
   BOOT_REASON            : constant Register := 16#01E6#;  --  16-bit, [2:0]
   BOOT_REASON_MASK       : constant Unsigned_16 := 16#0007#;
   BOOT_REASON_POWER_ON   : constant Unsigned_16 := 0;
   UDM1                   : constant Register := 16#01F4#;
   UDM2                   : constant Register := 16#01F8#;
   SEC_CTRL               : constant Register := 16#0C00#;
   SEC_IDMEM_SIZE_CONFIG_MASK  : constant Unsigned_32 := 16#0003_0000#;  --  [17:16]
   SEC_IDMEM_SIZE_CONFIG_SHIFT : constant := 16;
   SEC_IDMEM_SIZE_885XB   : constant Unsigned_32 := 2;
   BOOT_DBG               : constant Register := 16#83F0#;  --  diagnostics only

   ---------------------------------------------------------------------------
   --  DMAC
   ---------------------------------------------------------------------------
   HCI_FUNC_EN            : constant Register := 16#8380#;
   HCI_TXDMA_EN           : constant Unsigned_32 := 2 ** 0;
   HCI_RXDMA_EN           : constant Unsigned_32 := 2 ** 1;

   DMAC_FUNC_EN           : constant Register := 16#8400#;
   DMACREG_GCKEN          : constant Unsigned_32 := 2 ** 15;
   MAC_SEC_EN             : constant Unsigned_32 := 2 ** 16;
   BBRPT_EN               : constant Unsigned_32 := 2 ** 17;
   DISPATCHER_EN          : constant Unsigned_32 := 2 ** 18;
   DLE_CPUIO_EN           : constant Unsigned_32 := 2 ** 19;
   PKT_IN_EN              : constant Unsigned_32 := 2 ** 20;
   DMAC_TBL_EN            : constant Unsigned_32 := 2 ** 21;
   PKT_BUF_EN             : constant Unsigned_32 := 2 ** 22;
   DLE_PLE_EN             : constant Unsigned_32 := 2 ** 23;
   STA_SCH_EN             : constant Unsigned_32 := 2 ** 24;
   TXPKT_CTRL_EN          : constant Unsigned_32 := 2 ** 25;
   DLE_WDE_EN             : constant Unsigned_32 := 2 ** 26;
   WD_RLS_EN              : constant Unsigned_32 := 2 ** 27;
   MPDU_PROC_EN           : constant Unsigned_32 := 2 ** 28;
   DMAC_FUNC_ENABLE       : constant Unsigned_32 := 2 ** 29;  --  B_AX_DMAC_FUNC_EN
   MAC_FUNC_EN            : constant Unsigned_32 := 2 ** 30;

   DMAC_CLK_EN            : constant Register := 16#8404#;
   DISPATCHER_CLK_EN      : constant Unsigned_32 := 2 ** 18;
   DLE_PLE_CLK_EN         : constant Unsigned_32 := 2 ** 23;
   DLE_WDE_CLK_EN         : constant Unsigned_32 := 2 ** 26;

   TX_ADDRESS_INFO_MODE_SETTING : constant Register := 16#8810#;
   HOST_ADDR_INFO_8B_SEL  : constant Unsigned_32 := 2 ** 0;

   HCI_FC_CTRL            : constant Register := 16#8A00#;
   HCI_FC_EN              : constant Unsigned_32 := 2 ** 0;
   HCI_FC_CH12_EN         : constant Unsigned_32 := 2 ** 3;
   HCI_FC_CH12_FULL_COND_MASK  : constant Unsigned_32 := 16#0000_0C00#;  --  [11:10]
   HCI_FC_CH12_FULL_COND_SHIFT : constant := 10;
   CH_PAGE_CTRL           : constant Register := 16#8A04#;
   PREC_PAGE_CH12_SHIFT   : constant := 16;   --  [24:16]

   --  Data-link engine: WDE and PLE packet buffers and quotas.
   WDE_PKTBUF_CFG         : constant Register := 16#8C08#;
   PLE_PKTBUF_CFG         : constant Register := 16#9008#;
   PAGE_SEL_MASK          : constant Unsigned_32 := 16#0000_0003#;  --  [1:0]
   START_BOUND_MASK       : constant Unsigned_32 := 16#0000_3F00#;  --  [13:8]
   START_BOUND_SHIFT      : constant := 8;
   FREE_PAGE_NUM_MASK     : constant Unsigned_32 := 16#1FFF_0000#;  --  [28:16]
   FREE_PAGE_NUM_SHIFT    : constant := 16;
   WDE_PAGE_SEL_64        : constant Unsigned_32 := 0;
   PLE_PAGE_SEL_128       : constant Unsigned_32 := 1;
   DLE_BOUND_UNIT         : constant := 8 * 1024;

   --  Quota registers: min [11:0], max [27:16].
   QUOTA_MAX_SHIFT        : constant := 16;
   WDE_QTA0_CFG           : constant Register := 16#8C40#;  --  HIF
   WDE_QTA1_CFG           : constant Register := 16#8C44#;  --  WCPU
   WDE_QTA3_CFG           : constant Register := 16#8C4C#;  --  PKT_IN
   WDE_QTA4_CFG           : constant Register := 16#8C50#;  --  CPU_IO
   WDE_INI_STATUS         : constant Register := 16#8D00#;
   PLE_QTA0_CFG           : constant Register := 16#9040#;  --  CMA0_TX
   PLE_QTA_STRIDE         : constant := 4;                  --  .. QTA10 = CPU_IO
   PLE_QTA_COUNT          : constant := 11;
   PLE_INI_STATUS         : constant Register := 16#9100#;
   MGN_INI_RDY            : constant Unsigned_32 := 16#0000_0003#;  --  Q | BUF

   PKTIN_SETTING          : constant Register := 16#9A00#;
   WD_ADDR_INFO_LENGTH    : constant Unsigned_32 := 2 ** 1;

   --  CMAC range: narrow reads go through an aligned 32-bit read, and a
   --  read of Dead means the block's clocks are gated (pci.c, ACCESS_CMAC).
   CMAC_REG_START         : constant Register := 16#C000#;
   CK_EN                  : constant Register := 16#C004#;
   CMAC_ALLCKEN           : constant Unsigned_32 := 16#FFFF_FFFF#;
   CMAC_DEAD_RETRIES      : constant := 10;   --  MAC_REG_POOL_COUNT

   CMAC_FUNC_EN           : constant Register := 16#C000#;
   RMAC_EN                : constant Unsigned_32 := 2 ** 0;
   TMAC_EN                : constant Unsigned_32 := 2 ** 1;
   SCHEDULER_EN           : constant Unsigned_32 := 2 ** 2;
   PTCLTOP_EN             : constant Unsigned_32 := 2 ** 3;
   CMAC_DMA_EN            : constant Unsigned_32 := 2 ** 4;
   PHYINTF_EN             : constant Unsigned_32 := 2 ** 5;
   FORCE_CMACREG_GCKEN    : constant Unsigned_32 := 2 ** 15;
   CMAC_RXEN              : constant Unsigned_32 := 2 ** 28;
   CMAC_TXEN              : constant Unsigned_32 := 2 ** 29;
   CMAC_EN                : constant Unsigned_32 := 2 ** 30;

end RTW89_Registers;
