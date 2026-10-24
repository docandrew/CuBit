------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with RTW89_Registers; use RTW89_Registers;
with RTW89_MMIO; use RTW89_MMIO;
with RTW89_Layout; use RTW89_Layout;
with RTW89_Descriptors; use RTW89_Descriptors;

package body RTW89_Bringup is

   ---------------------------------------------------------------------------
   --  DMA memory
   ---------------------------------------------------------------------------
   procedure Put8 (DMA : Region; Offset : Natural; Value : Unsigned_8) is
      Cell : Unsigned_8 with Import, Volatile,
        Address => To_Address (Integer_Address (DMA.Virtual + Unsigned_64 (Offset)));
   begin
      Cell := Value;
   end Put8;

   procedure Put32 (DMA : Region; Offset : Natural; Value : Unsigned_32) is
      Cell : Unsigned_32 with Import, Volatile,
        Address => To_Address (Integer_Address (DMA.Virtual + Unsigned_64 (Offset)));
   begin
      Cell := Value;
   end Put32;

   procedure Put_BD (DMA : Region; Offset : Natural; BD : BD_Words) is
   begin
      Put32 (DMA, Offset, BD.Low);
      Put32 (DMA, Offset + 4, BD.High);
   end Put_BD;

   function Bus (DMA : Region; Offset : Natural) return Bus_Address is
     (DMA.Physical + Unsigned_64 (Offset));

   function Low32 (Value : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Value and 16#FFFF_FFFF#));
   function High32 (Value : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Shift_Right (Value, 32)));

   ---------------------------------------------------------------------------
   procedure Mask_Interrupts is
   begin
      Write32 (HIMR0, 0);
      Write32 (PCIE_HIMR00, 0);
      Write32 (PCIE_HIMR10, 0);
   end Mask_Interrupts;

   Narrow_DMA_Limit : constant Unsigned_64 := 2 ** 32;
   Wide_DMA_Limit   : constant Unsigned_64 := 2 ** 36;

   procedure Configure_DMA_Width (DMA : Region; Wide : out Boolean;
                                  Result : out Outcome) is
      Last : constant Unsigned_64 := DMA.Physical + DMA.Bytes - 1;
      Control : Unsigned_8;
      OK : Boolean;
   begin
      Wide := False;
      Result := Success;
      if Last < Narrow_DMA_Limit then
         return;
      elsif Last >= Wide_DMA_Limit then
         Result := Failure (DMA_Unreachable, 0, High32 (Last));
         return;
      end if;
      --  rtw89_pci_cfg_dac: set EN_64BITS in configuration byte 0x719.
      DBI_Read (PCIE_L1_CTRL, Control, OK);
      if OK then
         DBI_Write (PCIE_L1_CTRL, Control or PCIE_BIT_EN_64BITS, OK);
      end if;
      if not OK then
         Result := Failure (DBI_Timeout, DBI_FLAG, Unsigned_32 (Read16 (DBI_FLAG)));
         return;
      end if;
      Wide := True;
   end Configure_DMA_Width;

   ---------------------------------------------------------------------------
   --  DMAC pre-init, firmware-download quota mode (rtw8852b_dle_mem_pcie
   --  [RTW89_QTA_DLFW]: WDE 64-byte pages, 0 linked + 1024 unlinked; PLE
   --  128-byte pages, 64 linked + 960 unlinked; the whole 192 KiB FIFO).
   ---------------------------------------------------------------------------
   DLFW_WDE_Page_Bytes : constant := 64;
   DLFW_WDE_Linked     : constant := 0;
   DLFW_WDE_Unlinked   : constant := 1024;
   DLFW_PLE_Linked     : constant := 64;
   --  Quotas (min = max unless noted): WDE WCPU min 48 from the SCC mode
   --  (rtw89 ext_wde_min_qt_wcpu), PLE C2H 16 and H2C 48, all else 0.
   DLFW_WDE_WCPU_Min   : constant := 48;
   DLFW_PLE_C2H        : constant := 16;
   DLFW_PLE_H2C        : constant := 48;
   PLE_QTA_C2H_Index   : constant := 2;
   PLE_QTA_H2C_Index   : constant := 3;
   --  hfc_preccfg_pcie: H2C pre-cost 40 pages, full condition 0.
   H2C_Pre_Cost        : constant := 40;
   H2C_Full_Condition  : constant := 0;
   DLE_Ready_Budget    : constant Microseconds := 2_000;

   function Quota (Min, Max : Unsigned_32) return Unsigned_32 is
     (Min or Shift_Left (Max, QUOTA_MAX_SHIFT));

   procedure DMAC_Pre_Init (Result : out Outcome) is
      Enabled : Unsigned_32;
      Value   : Unsigned_32;
      Required : constant Unsigned_32 := MAC_FUNC_EN or DMAC_FUNC_ENABLE;
   begin
      Result := Success;
      --  rtw89_mac_ctrl_hci_dma_trx (true)
      Set32 (HCI_FUNC_EN, HCI_TXDMA_EN or HCI_RXDMA_EN);
      --  hci_func_en_ax, dmac_func_pre_en_ax: plain writes.
      Write32 (DMAC_FUNC_EN, MAC_FUNC_EN or DMAC_FUNC_ENABLE or DISPATCHER_EN or PKT_BUF_EN);
      Write32 (DMAC_CLK_EN, DISPATCHER_CLK_EN);

      Enabled := Read32 (DMAC_FUNC_EN);
      if Enabled = Config_EA or else Enabled = Dead or else (Enabled and Required) /= Required then
         Result := Failure (DMAC_Disabled, DMAC_FUNC_EN, Enabled);
         return;
      end if;

      Clear32 (DMAC_FUNC_EN, DLE_WDE_EN or DLE_PLE_EN);
      Set32 (DMAC_CLK_EN, DLE_WDE_CLK_EN or DLE_PLE_CLK_EN);

      Value := Read32 (WDE_PKTBUF_CFG);
      Value := (Value and not (PAGE_SEL_MASK or START_BOUND_MASK or FREE_PAGE_NUM_MASK))
        or WDE_PAGE_SEL_64
        or Shift_Left (Unsigned_32'(DLFW_WDE_Linked), FREE_PAGE_NUM_SHIFT);
      Write32 (WDE_PKTBUF_CFG, Value);

      Value := Read32 (PLE_PKTBUF_CFG);
      Value := (Value and not (PAGE_SEL_MASK or START_BOUND_MASK or FREE_PAGE_NUM_MASK))
        or PLE_PAGE_SEL_128
        or Shift_Left (Unsigned_32 ((DLFW_WDE_Linked + DLFW_WDE_Unlinked)
                                    * DLFW_WDE_Page_Bytes / DLE_BOUND_UNIT),
                       START_BOUND_SHIFT)
        or Shift_Left (Unsigned_32'(DLFW_PLE_Linked), FREE_PAGE_NUM_SHIFT);
      Write32 (PLE_PKTBUF_CFG, Value);

      Write32 (WDE_QTA0_CFG, 0);
      Write32 (WDE_QTA1_CFG, Quota (DLFW_WDE_WCPU_Min, 0));
      Write32 (WDE_QTA3_CFG, 0);
      Write32 (WDE_QTA4_CFG, 0);
      for Index in 0 .. PLE_QTA_COUNT - 1 loop
         Write32 (PLE_QTA0_CFG + Register (Index * PLE_QTA_STRIDE),
                  (case Index is
                     when PLE_QTA_C2H_Index => Quota (DLFW_PLE_C2H, DLFW_PLE_C2H),
                     when PLE_QTA_H2C_Index => Quota (DLFW_PLE_H2C, DLFW_PLE_H2C),
                     when others => 0));
      end loop;

      Set32 (DMAC_FUNC_EN, DLE_WDE_EN or DLE_PLE_EN);
      if not Poll32 (WDE_INI_STATUS, MGN_INI_RDY, MGN_INI_RDY, DLE_Ready_Budget) then
         Result := Failure (Poll_Timeout, WDE_INI_STATUS, Read32 (WDE_INI_STATUS));
         return;
      end if;
      if not Poll32 (PLE_INI_STATUS, MGN_INI_RDY, MGN_INI_RDY, DLE_Ready_Budget) then
         Result := Failure (Poll_Timeout, PLE_INI_STATUS, Read32 (PLE_INI_STATUS));
         return;
      end if;

      --  rtw89_mac_hfc_init (reset, en => false, h2c_en => true)
      Clear32 (HCI_FC_CTRL, HCI_FC_EN or HCI_FC_CH12_EN);
      Write32 (CH_PAGE_CTRL, Shift_Left (Unsigned_32'(H2C_Pre_Cost), PREC_PAGE_CH12_SHIFT));
      Field32 (HCI_FC_CTRL, HCI_FC_CH12_FULL_COND_MASK, HCI_FC_CH12_FULL_COND_SHIFT,
               H2C_Full_Condition);
      Clear32 (HCI_FC_CTRL, HCI_FC_EN);
      Set32 (HCI_FC_CTRL, HCI_FC_CH12_EN);
   end DMAC_Pre_Init;

   ---------------------------------------------------------------------------
   --  PCIe pre-init (rtw89_pci_ops_mac_pre_init_ax for the 8852B)
   ---------------------------------------------------------------------------
   DMA_Idle_Budget  : constant Microseconds := 100;
   BDRAM_Budget     : constant Microseconds := 100;

   procedure PHY_Mask (Address : Unsigned_8; Mask, Bits : Unsigned_16;
                       Speed : PHY_Speed; OK : out Boolean) is
      Value : Unsigned_16;
   begin
      MDIO_Read (Address, Speed, Value, OK);
      if OK then
         MDIO_Write (Address, Speed, (Value and not Mask) or (Bits and Mask), OK);
      end if;
   end PHY_Mask;

   procedure Program_Rings (DMA : Region) is
      Ring_Base : Natural;
   begin
      for Channel in TX_Channel loop
         Ring_Base := TX_Ring_Offset (Channel);
         for Slot in 0 .. TX_Ring_Entries - 1 loop
            Put_BD (DMA, Ring_Base + Slot * BD_Bytes, (0, 0));
         end loop;
         Write16 (TX_Rings (Channel).Count, TX_Ring_Entries);
         Write32 (TX_Rings (Channel).BD_RAM, TX_Rings (Channel).BD_RAM_Value);
         Write32 (TX_Rings (Channel).Base_Low, Low32 (Bus (DMA, Ring_Base)));
         Write32 (TX_Rings (Channel).Base_High, High32 (Bus (DMA, Ring_Base)));
      end loop;
      for Channel in RX_Channel loop
         Ring_Base := RX_Ring_Offset (Channel);
         for Slot in 0 .. RX_Ring_Entries - 1 loop
            Put_BD (DMA, Ring_Base + Slot * BD_Bytes,
                    RX_BD (RX_Buffer_Bytes, Bus (DMA, RX_Buffer_Offset (Channel, Slot))));
         end loop;
         Write16 (RX_Rings (Channel).Count, RX_Ring_Entries);
         Write32 (RX_Rings (Channel).Base_Low, Low32 (Bus (DMA, Ring_Base)));
         Write32 (RX_Rings (Channel).Base_High, High32 (Bus (DMA, Ring_Base)));
      end loop;
   end Program_Rings;

   procedure PCIe_Pre_Init (DMA : Region; Result : out Outcome) is
      OK, Ignore_OK : Boolean;
      Rate, L1_Control : Unsigned_8;
      Speed : PHY_Speed;
      Calibration : Unsigned_16;
      L1_Was_On : Boolean;
      Watchdog : Unsigned_32;
   begin
      Result := Success;
      Clear32 (PCIE_PS_CTRL, L1OFF_PWR_OFF_EN);                 --  l1off_pwroff
      Clear32 (SYS_PW_CTRL, PSUS_OFF_CAPC_EN);                  --  aphy_pwrcut
      Set32 (SYS_SDIO_CTRL, PCIE_DIS_L2_CTRL_LDO_HCI);          --  hci_ldo
      Clear32 (SYS_SDIO_CTRL, PCIE_DIS_WLSUS_AFT_PDN);

      --  dphy_delay: rtw89 ignores its result.
      PHY_Mask (RAC_REG_REV2, BAC_CMU_EN_DLY_MASK, PCIE_DPHY_DLY_25US, Gen1, Ignore_OK);
      --  autok_x
      PHY_Mask (RAC_REG_FLD_0, BAC_AUTOK_N_MASK, PCIE_AUTOK_4, Gen1, OK);
      if not OK then
         Result := Failure (MDIO_Timeout, MDIO_CFG, Unsigned_32 (Read16 (MDIO_CFG)));
         return;
      end if;

      --  auto_refclk_cal (autook disabled): turn PHY calibration off with
      --  ASPM L1 held off around it.
      DBI_Read (PCIE_PHY_RATE, Rate, OK);
      if not OK then
         Result := Failure (DBI_Timeout, DBI_FLAG, Unsigned_32 (Read16 (DBI_FLAG)));
         return;
      end if;
      case Rate and PCIE_PHY_RATE_MASK is
         when PCIE_PHY_RATE_GEN1 => Speed := Gen1;
         when PCIE_PHY_RATE_GEN2 => Speed := Gen2;
         when others =>
            Result := Failure (Unsupported_Link, 0, Unsigned_32 (Rate));
            return;
      end case;
      DBI_Read (PCIE_L1_CTRL, L1_Control, OK);
      if not OK then
         Result := Failure (DBI_Timeout, DBI_FLAG, Unsigned_32 (Read16 (DBI_FLAG)));
         return;
      end if;
      L1_Was_On := (L1_Control and PCIE_BIT_L1) /= 0;
      if L1_Was_On then
         DBI_Write (PCIE_L1_CTRL, L1_Control and not PCIE_BIT_L1, OK);
      end if;
      if OK then
         MDIO_Read (RAC_CTRL_PPR_V1, Speed, Calibration, OK);
         if OK and then (Calibration and CALIB_EN) /= 0 then
            MDIO_Write (RAC_CTRL_PPR_V1, Speed, Calibration and not CALIB_EN, OK);
         end if;
      end if;
      if L1_Was_On then
         DBI_Write (PCIE_L1_CTRL, L1_Control, Ignore_OK);
      end if;
      if not OK then
         Result := Failure (MDIO_Timeout, MDIO_CFG, Unsigned_32 (Read16 (MDIO_CFG)));
         return;
      end if;

      Set32 (HCI_OPT_CTRL, BIT_WAKE_CTRL);                      --  power_wake (true)
      Clear32 (PCIE_EXP_CTRL, SIC_EN_FORCE_CLKREQ);             --  set_sic
      Watchdog := Read32 (LBC_WATCHDOG);                        --  set_lbc
      Watchdog := (Watchdog and not LBC_TIMER_MASK)
        or Shift_Left (LBC_TMR_2MS, LBC_TIMER_SHIFT) or LBC_EN_AND_FLAG;
      Write32 (LBC_WATCHDOG, Watchdog);
      Set32 (LBC_WATCHDOG, Watchdog);
      Set32 (PCIE_DBG_CTRL, ASFF_FULL_NO_STK_AND_EN_STUCK_DBG); --  set_dbg
      Field32 (PCIE_EXP_CTRL, EXP_CTRL_DBG_MASK, 0, EXP_CTRL_DBG_VALUE);
      Set32 (PCIE_INIT_CFG1, PCIE_TXRST_KEEP_REG or PCIE_RXRST_KEEP_REG);

      --  Stop all DMA and wait for idle.
      Set32 (PCIE_DMA_STOP1, STOP_WPDMA);
      Set32 (PCIE_DMA_STOP1, STOP_PCIEIO);
      Clear32 (PCIE_INIT_CFG1, RXHCI_EN or TXHCI_EN);
      if not Poll32 (PCIE_DMA_BUSY1, DMA_BUSY1_CHECK_V1, 0, DMA_Idle_Budget)
        or else not Poll32 (PCIE_DMA_BUSY1, RXQ_RPQ_BUSY, 0, DMA_Idle_Budget)
      then
         Result := Failure (Poll_Timeout, PCIE_DMA_BUSY1, Read32 (PCIE_DMA_BUSY1));
         return;
      end if;
      Set32 (TXBD_RWPTR_CLR1, CLR_TX_IDX_8852B);
      Set32 (RXBD_RWPTR_CLR, CLR_RXQ_RPQ_IDX);

      --  mode_op
      Clear32 (PCIE_INIT_CFG1, RXBD_MODE);
      Field32 (PCIE_INIT_CFG1, PCIE_MAX_TXDMA_MASK, PCIE_MAX_TXDMA_SHIFT, TX_BURST_2048B);
      Field32 (PCIE_INIT_CFG1, PCIE_MAX_RXDMA_MASK, PCIE_MAX_RXDMA_SHIFT, RX_BURST_128B);
      Set32 (PCIE_INIT_CFG1, LATENCY_CONTROL);
      Field32 (PCIE_EXP_CTRL, MAX_TAG_NUM_MASK, MAX_TAG_NUM_SHIFT, TAG_NUM_8);
      Field32 (PCIE_INIT_CFG2, WD_ITVL_IDLE_MASK, WD_ITVL_IDLE_SHIFT, WD_DMA_INTVL_256NS);
      Field32 (PCIE_INIT_CFG2, WD_ITVL_ACT_MASK, WD_ITVL_ACT_SHIFT, WD_DMA_INTVL_256NS);
      Set32 (TX_ADDRESS_INFO_MODE_SETTING, HOST_ADDR_INFO_8B_SEL);
      Clear32 (PKTIN_SETTING, WD_ADDR_INFO_LENGTH);

      Program_Rings (DMA);

      Set32 (PCIE_INIT_CFG1, RST_BDRAM);
      if not Poll32 (PCIE_INIT_CFG1, RST_BDRAM, 0, BDRAM_Budget) then
         Result := Failure (Poll_Timeout, PCIE_INIT_CFG1, Read32 (PCIE_INIT_CFG1));
         return;
      end if;

      --  Only the firmware-command channel runs; RX runs.
      Set32 (PCIE_DMA_STOP1, TX_STOP1_MASK_V1);
      Clear32 (PCIE_DMA_STOP1, STOP_CH12);
      Clear32 (PCIE_DMA_STOP1, STOP_PCIEIO);
      Set32 (PCIE_INIT_CFG1, RXHCI_EN or TXHCI_EN);
   end PCIe_Pre_Init;

   ---------------------------------------------------------------------------
   --  Firmware download
   ---------------------------------------------------------------------------
   --  FWDL_WAIT_CNT: 400 ms for path-ready and firmware-ready polls.
   Firmware_Budget  : constant Microseconds := 400_000;
   --  How long one channel-12 slot may stay busy before we give up.
   Ring_Budget      : constant Microseconds := 400_000;
   --  mdelay(5) between the last section and the ready poll.
   Boot_Settle      : constant Milliseconds := 5;

   function Download_Status return RTW89_Registers.Download_Status is
     (RTW89_Registers.Download_Status'Val
        (Shift_Right (Read32 (WCPU_FW_CTRL) and WCPU_FWDL_STS_MASK, WCPU_FWDL_STS_SHIFT)));

   type Command_Ring is record
      Host_Next : Natural := 0;   --  next BD the host fills
   end record;

   function Hardware_Done return Natural is
     (Hardware_Index (Read32 (TX_Rings (CH12).Index)) mod TX_Ring_Entries);

   --  Wait until at most Limit packets are in flight.
   function Drain_To (Ring : Command_Ring; Limit : Natural) return Boolean is
   begin
      for Attempt in 1 .. Natural (Ring_Budget) loop
         if Outstanding (Ring.Host_Next, Hardware_Done, TX_Ring_Entries) <= Limit then
            return True;
         end if;
         --  Each register read takes at least ~1 us over PCIe.
         null;
      end loop;
      return Outstanding (Ring.Host_Next, Hardware_Done, TX_Ring_Entries) <= Limit;
   end Drain_To;

   procedure Send
     (DMA : Region; Ring : in out Command_Ring;
      Chunk : Boolean; Header : Unsigned_32; Header_Present : Boolean;
      File : RTW89_Firmware.Byte_Array; First, Count : Natural;
      Patch_Part_Size : Boolean; OK : out Boolean)
   with Pre => Count >= 1 and then Count <= RTW89_Firmware.Part_Size
               and then First + Count <= File'Length and then File'First = 0
   is
      Slot   : constant Natural := Ring.Host_Next mod Command_Slots;
      Base   : constant Natural := Command_Offset + Slot * Command_Slot_Bytes;
      Prefix : constant Natural := WD_Body_Bytes + (if Header_Present then H2C_Header_Bytes else 0);
      Size   : constant Packet_Size := Count + (if Header_Present then H2C_Header_Bytes else 0);
      Value  : Unsigned_8;
      Byte_Base       : constant := 256;
      Part_Size_Low_Value  : constant := RTW89_Firmware.Part_Size mod Byte_Base;
      Part_Size_High_Value : constant := RTW89_Firmware.Part_Size / Byte_Base;
      Part_Size_Low  : constant Unsigned_8 := Part_Size_Low_Value;
      Part_Size_High : constant Unsigned_8 := Part_Size_High_Value;
   begin
      OK := Drain_To (Ring, Maximum_In_Flight);
      if not OK then
         return;
      end if;
      Put32 (DMA, Base, WD_Dword0 (Chunk));
      Put32 (DMA, Base + 4, 0);
      Put32 (DMA, Base + 8, WD_Dword2 (Size));
      Put32 (DMA, Base + 12, 0);
      Put32 (DMA, Base + 16, 0);
      Put32 (DMA, Base + 20, 0);
      if Header_Present then
         Put32 (DMA, Base + WD_Body_Bytes, Header);
         Put32 (DMA, Base + WD_Body_Bytes + 4, H2C_Header1 (Count));
      end if;
      for I in 0 .. Count - 1 loop
         Value := File (First + I);
         --  rtw89 rewrites PART_SIZE (w7[15:0]) in the header packet.
         if Patch_Part_Size and then I = RTW89_Firmware.Part_Size_Offset then
            Value := Part_Size_Low;
         elsif Patch_Part_Size and then I = RTW89_Firmware.Part_Size_Offset + 1 then
            Value := Part_Size_High;
         end if;
         Put8 (DMA, Base + Prefix + I, Value);
      end loop;
      Put_BD (DMA, TX_Ring_Offset (CH12) + Ring.Host_Next * BD_Bytes,
              TX_BD (Buffer_Length (Prefix + Count), Bus (DMA, Base)));
      Ring.Host_Next := (Ring.Host_Next + 1) mod TX_Ring_Entries;
      Write16 (TX_Rings (CH12).Index, Unsigned_16 (Ring.Host_Next));
   end Send;

   procedure Download
     (DMA     : Region;
      File    : RTW89_Firmware.Byte_Array;
      Plan    : RTW89_Firmware.Plan;
      Packets : out Natural;
      Result  : out Outcome)
   is
      Ring : Command_Ring;
      OK : Boolean;
      Remaining, Offset, Count : Natural;
      Value : Unsigned_32;
   begin
      Packets := 0;
      Result := Success;

      --  rtw89_mac_disable_cpu_ax
      Clear32 (PLATFORM_ENABLE, WCPU_EN);
      Clear32 (WCPU_FW_CTRL, WCPU_FWDL_EN or H2C_PATH_RDY or FWDL_PATH_RDY);
      Clear32 (SYS_CLK_CTRL, CPU_CLK_EN);
      Clear32 (PLATFORM_ENABLE, APB_WRAP_EN);                   --  watchdog off (885xb)
      Set32 (PLATFORM_ENABLE, APB_WRAP_EN);
      Clear32 (PLATFORM_ENABLE, PLATFORM_EN);
      Set32 (PLATFORM_ENABLE, PLATFORM_EN);

      --  rtw89_mac_enable_cpu_ax (boot reason power-on, download)
      if (Read32 (PLATFORM_ENABLE) and WCPU_EN) /= 0 then
         Result := Failure (Firmware_Download_Failed, PLATFORM_ENABLE, Read32 (PLATFORM_ENABLE));
         return;
      end if;
      Write32 (UDM1, 0);
      Write32 (UDM2, 0);
      Write32 (HALT_H2C_CTRL, 0);
      Write32 (HALT_C2H_CTRL, 0);
      Write32 (HALT_H2C, 0);
      Write32 (HALT_C2H, 0);
      Set32 (SYS_CLK_CTRL, CPU_CLK_EN);
      Value := Read32 (WCPU_FW_CTRL)
        and not (WCPU_FWDL_EN or H2C_PATH_RDY or FWDL_PATH_RDY or WCPU_FWDL_STS_MASK);
      Write32 (WCPU_FW_CTRL, Value or WCPU_FWDL_EN);
      Field32 (SEC_CTRL, SEC_IDMEM_SIZE_CONFIG_MASK, SEC_IDMEM_SIZE_CONFIG_SHIFT,
               SEC_IDMEM_SIZE_885XB);
      Write16 (BOOT_REASON, (Read16 (BOOT_REASON) and not BOOT_REASON_MASK)
                            or BOOT_REASON_POWER_ON);
      Set32 (PLATFORM_ENABLE, WCPU_EN);

      if not Poll8 (WCPU_FW_CTRL, Unsigned_8 (H2C_PATH_RDY), Unsigned_8 (H2C_PATH_RDY),
                    Firmware_Budget)
      then
         Result := Failure (Firmware_Not_Ready, WCPU_FW_CTRL, Read32 (WCPU_FW_CTRL));
         return;
      end if;

      --  The header, as an H2C (MAC / FWDL / FWHDR_DL, sequence 0).
      Send (DMA, Ring, Chunk => False,
            Header => H2C_Header0 (H2C_Category_MAC, H2C_Class_FWDL,
                                   H2C_Function_Header_Download, 0),
            Header_Present => True, File => File,
            First => Plan.Header, Count => Plan.Header_Bytes,
            Patch_Part_Size => True, OK => OK);
      if not OK then
         Result := Failure (Firmware_Ring_Full, TX_Rings (CH12).Index, Read32 (TX_Rings (CH12).Index));
         return;
      end if;
      Packets := 1;
      if not Poll8 (WCPU_FW_CTRL, Unsigned_8 (FWDL_PATH_RDY), Unsigned_8 (FWDL_PATH_RDY),
                    Firmware_Budget)
      then
         Result := Failure (Firmware_Not_Ready, WCPU_FW_CTRL, Read32 (WCPU_FW_CTRL));
         return;
      end if;
      Write32 (HALT_H2C_CTRL, 0);
      Write32 (HALT_C2H_CTRL, 0);

      --  Sections, raw, in chunks of Part_Size.
      for S in 1 .. Plan.Sections loop
         Offset := Plan.Items (S).Data;
         Remaining := Plan.Items (S).Length;
         while Remaining > 0 loop
            Count := Natural'Min (Remaining, RTW89_Firmware.Part_Size);
            Send (DMA, Ring, Chunk => True, Header => 0, Header_Present => False,
                  File => File, First => Offset, Count => Count,
                  Patch_Part_Size => False, OK => OK);
            if not OK then
               Result := Failure (Firmware_Ring_Full, TX_Rings (CH12).Index,
                                  Read32 (TX_Rings (CH12).Index));
               return;
            end if;
            Packets := Packets + 1;
            Offset := Offset + Count;
            Remaining := Remaining - Count;
         end loop;
      end loop;

      if not Drain_To (Ring, 0) then
         Result := Failure (Firmware_Ring_Full, TX_Rings (CH12).Index, Read32 (TX_Rings (CH12).Index));
         return;
      end if;
      Sleep (Boot_Settle);
      if not Poll32 (WCPU_FW_CTRL, WCPU_FWDL_STS_MASK,
                     Shift_Left (Unsigned_32 (RTW89_Registers.Download_Status'Pos
                                                (WCPU_Firmware_Ready)),
                                 WCPU_FWDL_STS_SHIFT),
                     Firmware_Budget)
      then
         Result := Failure (Firmware_Not_Ready, WCPU_FW_CTRL, Read32 (WCPU_FW_CTRL));
      end if;
   end Download;

end RTW89_Bringup;
