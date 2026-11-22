------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with RTW89_Registers; use RTW89_Registers;
with RTW89_MAC_Registers;
with RTW89_MMIO; use RTW89_MMIO;
with RTW89_PHY_Tables;
with RTW89_Tables_8852B;
with RTW89_Channel_Values; use RTW89_Channel_Values;
with RTW89_TX_Power;

package body RTW89_PHY is
   use type WS.Band;
   package MR renames RTW89_MAC_Registers;

   ---------------------------------------------------------------------------
   --  Access paths
   ---------------------------------------------------------------------------
   BB_Base       : constant := 16#1_0000#;    --  phy_def cr_base
   BB_Last       : constant := 16#FFFF#;
   subtype BB_Address is Unsigned_32 range 0 .. BB_Last;

   function BB (A : BB_Address) return Register is (Register (BB_Base + A));

   --  The lowest set bit's position (rtw89 __ffs); 0 for an empty mask.
   function Shift_Of (Mask : Unsigned_32) return Natural is
   begin
      for Bit in 0 .. 31 loop
         if (Mask and Shift_Left (Unsigned_32'(1), Bit)) /= 0 then
            return Bit;
         end if;
      end loop;
      return 0;
   end Shift_Of;

   Full_Mask_32 : constant Unsigned_32 := 16#FFFF_FFFF#;

   procedure BB_Write (A : BB_Address; V : Unsigned_32) is
   begin
      Write32 (BB (A), V);
   end BB_Write;

   procedure BB_Mask (A : BB_Address; Mask, V : Unsigned_32) is
   begin
      Field32 (BB (A), Mask, Shift_Of (Mask), V);
   end BB_Mask;

   procedure BB_Set (A : BB_Address; Bits : Unsigned_32) is
   begin
      Set32 (BB (A), Bits);
   end BB_Set;

   procedure BB_Clear (A : BB_Address; Bits : Unsigned_32) is
   begin
      Clear32 (BB (A), Bits);
   end BB_Clear;

   function BB_Read (A : BB_Address; Mask : Unsigned_32) return Unsigned_32 is
     (Shift_Right (Read32 (BB (A)) and Mask, Shift_Of (Mask)));

   --  RF: addresses with bit 16 go to the direct window of the path,
   --  others through the serial interface (rtw89_phy_write_rf_v1).
   type RF_Path is (A, B);
   RF_Direct_Bit  : constant Unsigned_32 := 16#1_0000#;   --  RTW89_RF_ADDR_ADSEL_MASK
   RF_Address_Mask : constant Unsigned_32 := 16#FF#;
   RF_Mask        : constant Unsigned_32 := 16#F_FFFF#;   --  RFREG_MASK
   RF_Direct_Base : constant array (RF_Path) of Unsigned_32 := [A => 16#E000#, B => 16#F000#];
   RF_Word_Bytes  : constant := 4;
   RF_Invalid     : constant Unsigned_32 := 16#FFFF_FFFF#;  --  INV_RF_DATA
   R_SWSI_V1          : constant BB_Address := 16#174C#;
   SWSI_BUSY          : constant Unsigned_32 := 16#0300_0000#;  --  W_BUSY | R_BUSY
   SWSI_DATA_DONE     : constant Unsigned_32 := 2 ** 26;
   R_SWSI_BIT_MASK_V1 : constant BB_Address := 16#0374#;
   R_SWSI_DATA_V1     : constant BB_Address := 16#0370#;
   SWSI_BIT_MASK_EN   : constant Unsigned_32 := 2 ** 31;
   SWSI_PATH_SHIFT    : constant := 28;
   SWSI_ADDR_SHIFT    : constant := 20;
   R_SWSI_READ_ADDR_V1 : constant BB_Address := 16#0378#;
   SWSI_READ_MASK     : constant Unsigned_32 := 16#7FF#;
   SWSI_READ_PATH_SHIFT : constant := 8;
   SWSI_Budget        : constant Microseconds := 30;
   SWSI_Read_Settle   : constant Microseconds := 2;
   RF_Write_Settle    : constant Microseconds := 1;

   function Direct (P : RF_Path; Address : Unsigned_32) return Register is
     (BB (RF_Direct_Base (P) + (Address and RF_Address_Mask) * RF_Word_Bytes));

   procedure RF_Write (P : RF_Path; Address, Mask, Data : Unsigned_32) is
      M : constant Unsigned_32 := Mask and RF_Mask;
      Value : Unsigned_32 := Data;
      Enable : Unsigned_32 := 0;
   begin
      if (Address and RF_Direct_Bit) /= 0 then
         Field32 (Direct (P, Address), M, Shift_Of (M), Data);
         Delay_For (RF_Write_Settle);
         return;
      end if;
      if not Poll32 (BB (R_SWSI_V1), SWSI_BUSY, 0, SWSI_Budget) then
         return;   --  rtw89 drops the write
      end if;
      if M /= RF_Mask then
         BB_Mask (R_SWSI_BIT_MASK_V1, RF_Mask, M);
         Value := Shift_Left (Data, Shift_Of (M)) and RF_Mask;
         Enable := SWSI_BIT_MASK_EN;
      end if;
      BB_Write (R_SWSI_DATA_V1,
                Enable or Shift_Left (Unsigned_32 (RF_Path'Pos (P)), SWSI_PATH_SHIFT)
                or Shift_Left (Address and RF_Address_Mask, SWSI_ADDR_SHIFT) or (Value and RF_Mask));
   end RF_Write;

   function RF_Read (P : RF_Path; Address, Mask : Unsigned_32) return Unsigned_32 is
      M : constant Unsigned_32 := Mask and RF_Mask;
   begin
      if (Address and RF_Direct_Bit) /= 0 then
         return Shift_Right (Read32 (Direct (P, Address)) and M, Shift_Of (M));
      end if;
      if not Poll32 (BB (R_SWSI_V1), SWSI_BUSY, 0, SWSI_Budget) then
         return RF_Invalid;
      end if;
      BB_Mask (R_SWSI_READ_ADDR_V1, SWSI_READ_MASK,
               Shift_Left (Unsigned_32 (RF_Path'Pos (P)), SWSI_READ_PATH_SHIFT) or (Address and RF_Address_Mask));
      Delay_For (SWSI_Read_Settle);
      if not Poll32 (BB (R_SWSI_V1), SWSI_DATA_DONE, SWSI_DATA_DONE, SWSI_Budget) then
         return RF_Invalid;
      end if;
      return Shift_Right (Read32 (BB (R_SWSI_V1)) and M, Shift_Of (M));
   end RF_Read;

   procedure RF_Full (P : RF_Path; Address, Data : Unsigned_32) is
   begin
      RF_Write (P, Address, RF_Mask, Data);
   end RF_Full;

   ---------------------------------------------------------------------------
   --  Register-program tables (rtw89_rfk_parser: WRF, WM, DELAY)
   ---------------------------------------------------------------------------
   type Step_Kind is (Mask_BB, Write_RF_A, Write_RF_B, Pause);
   type Program_Step is record
      Kind : Step_Kind;
      Where : Unsigned_32;
      Mask  : Unsigned_32;
      Value : Unsigned_32;
   end record;
   type Program is array (Positive range <>) of Program_Step;

   procedure Run (P : Program) is
   begin
      for S of P loop
         case S.Kind is
            when Mask_BB    => BB_Mask (S.Where, S.Mask, S.Value);
            when Write_RF_A => RF_Write (A, S.Where, S.Mask, S.Value);
            when Write_RF_B => RF_Write (B, S.Where, S.Mask, S.Value);
            when Pause      => Delay_For (Microseconds (S.Value));
         end case;
      end loop;
   end Run;

   function M (Where, Mask, Value : Unsigned_32) return Program_Step is ((Mask_BB, Where, Mask, Value));
   function Wait (Us : Unsigned_32) return Program_Step is ((Pause, 0, 0, Us));

   ---------------------------------------------------------------------------
   --  BB/RF reset (rtw8852b_common.c disable/enable_bb_rf)
   ---------------------------------------------------------------------------
   RF00S_EN : constant Unsigned_8 := 16#07#;
   type XTAL_List is array (Positive range <>) of XTAL_Offset;
   RFC_Offsets : constant XTAL_List := [XTAL_SI_WL_RFC_S0, XTAL_SI_WL_RFC_S1];

   procedure Reset_BB_RF (Result : out Outcome) is
      Value : Unsigned_8;
      OK : Boolean := True;
   begin
      Result := Success;
      Clear32 (WLRF_CTRL, AFC_AFEDIG);
      Clear8 (SYS_FUNC_EN, FEN_BBRSTB or FEN_BB_GLB_RSTN);
      for Offset of RFC_Offsets loop
         if OK then
            XTAL_Read (Offset, Value, OK);
         end if;
         if OK then
            XTAL_Write (Offset, Value and not RF00S_EN, MR.XTAL_SI_FULL_MASK, OK);
         end if;
      end loop;
      if OK then
         Set8 (SYS_FUNC_EN, FEN_BBRSTB or FEN_BB_GLB_RSTN);
         Field32 (SPS_DIG_ON_CTRL0, REG_ZCDC_H_MASK, REG_ZCDC_H_SHIFT, MR.ZCDC_H_ENABLE);
         Set32 (WLRF_CTRL, AFC_AFEDIG);
         Clear32 (WLRF_CTRL, AFC_AFEDIG);
         Set32 (WLRF_CTRL, AFC_AFEDIG);
         XTAL_Write (XTAL_SI_WL_RFC_S0, MR.XTAL_SI_RFC_ON, MR.XTAL_SI_FULL_MASK, OK);
      end if;
      if OK then
         XTAL_Write (XTAL_SI_WL_RFC_S1, MR.XTAL_SI_RFC_ON, MR.XTAL_SI_FULL_MASK, OK);
      end if;
      if not OK then
         Result := Failure (XTAL_Timeout, WLAN_XTAL_SI_CTRL, Read32 (WLAN_XTAL_SI_CTRL));
         return;
      end if;
      Write8 (MR.PHYREG_SET, MR.PHYREG_SET_XYN_CYCLE);
   end Reset_BB_RF;

   ---------------------------------------------------------------------------
   --  Tables
   ---------------------------------------------------------------------------
   --  rtw89_phy_config_bb_reg pseudo-addresses and the bypass datum.
   BB_Delay_50ms : constant := 16#FE#;
   BB_Delay_5ms  : constant := 16#FD#;
   BB_Delay_1ms  : constant := 16#FC#;
   BB_Delay_50us : constant := 16#FB#;
   BB_Delay_5us  : constant := 16#FA#;
   BB_Delay_1us  : constant := 16#F9#;
   Bypass_Data   : constant Unsigned_32 := 16#BABE_CAFE#;
   RF_Delay_50ms : constant := 16#FE#;
   RF_Last_Address : constant := 16#1_00FF#;
   Out_Of_Window : Natural := 0;   --  table entries refused (address past BB)

   procedure BB_Entry (E : RTW89_PHY_Tables.Table_Entry) is
   begin
      case E.Address is
         when BB_Delay_50ms => Sleep (50);
         when BB_Delay_5ms  => Sleep (5);
         when BB_Delay_1ms  => Sleep (1);
         when BB_Delay_50us => Delay_For (50);
         when BB_Delay_5us  => Delay_For (5);
         when BB_Delay_1us  => Delay_For (1);
         when others =>
            if E.Data = Bypass_Data then
               null;
            elsif E.Address > BB_Last then
               Out_Of_Window := Out_Of_Window + 1;
            else
               BB_Write (E.Address, E.Data);
            end if;
      end case;
   end BB_Entry;

   Current_Path : RF_Path := A;

   procedure RF_Entry (E : RTW89_PHY_Tables.Table_Entry) is
   begin
      if E.Address = RF_Delay_50ms then
         Sleep (50);
      elsif E.Address > RF_Last_Address then
         Out_Of_Window := Out_Of_Window + 1;
      else
         RF_Full (Current_Path, E.Address, E.Data);
      end if;
   end RF_Entry;

   procedure Load_BB is new RTW89_PHY_Tables.Load (BB_Entry);
   procedure Load_RF is new RTW89_PHY_Tables.Load (RF_Entry);

   procedure Check (Status : RTW89_PHY_Tables.Load_Status; Which : Register; Result : in out Outcome) is
      use type RTW89_PHY_Tables.Load_Status;
   begin
      if not OK (Result) then
         return;
      elsif Status = RTW89_PHY_Tables.No_Configuration then
         Result := Failure (Table_Unconfigured, Which, 0);
      elsif Status = RTW89_PHY_Tables.Aborted then
         Result := Failure (Table_Aborted, Which, 0);
      elsif Out_Of_Window /= 0 then
         Result := Failure (Table_Aborted, Which, Unsigned_32 (Out_Of_Window));
      end if;
   end Check;

   --  Which table failed, for the stage line ("reg=" carries it).
   Table_BB      : constant Register := 1;
   Table_Radio_A : constant Register := 2;
   Table_Radio_B : constant Register := 3;
   Table_NCTL    : constant Register := 4;

   --  rtw8852b_bb_reset: TX power / TSSI reset held around bb_reset_all.
   R_P0_TXPW_RSTB : constant BB_Address := 16#58DC#;
   R_P0_TSSI_TRK  : constant BB_Address := 16#5818#;
   R_P1_TXPW_RSTB : constant BB_Address := 16#78DC#;
   R_P1_TSSI_TRK  : constant BB_Address := 16#7818#;
   TXPW_TSSI_HOLD : constant Unsigned_32 := 2 ** 30;
   R_S0_HW_SI_DIS : constant BB_Address := 16#1200#;
   R_S1_HW_SI_DIS : constant BB_Address := 16#3200#;
   HW_SI_TRIG     : constant Unsigned_32 := 16#7000_0000#;
   HW_SI_DISABLE  : constant Unsigned_32 := 7;
   R_RSTB_ASYNC   : constant BB_Address := 16#0704#;
   type BB_List is array (Positive range <>) of BB_Address;
   TSSI_Registers : constant BB_List := [R_P0_TXPW_RSTB, R_P0_TSSI_TRK, R_P1_TXPW_RSTB, R_P1_TSSI_TRK];
   RSTB_ASYNC_ALL : constant Unsigned_32 := 2 ** 1;

   procedure Reset_All is
   begin
      BB_Mask (R_S0_HW_SI_DIS, HW_SI_TRIG, HW_SI_DISABLE);
      BB_Mask (R_S1_HW_SI_DIS, HW_SI_TRIG, HW_SI_DISABLE);
      Delay_For (1);
      BB_Mask (R_RSTB_ASYNC, RSTB_ASYNC_ALL, 1);
      BB_Mask (R_RSTB_ASYNC, RSTB_ASYNC_ALL, 0);
      BB_Mask (R_S0_HW_SI_DIS, HW_SI_TRIG, 0);
      BB_Mask (R_S1_HW_SI_DIS, HW_SI_TRIG, 0);
      BB_Mask (R_RSTB_ASYNC, RSTB_ASYNC_ALL, 1);
   end Reset_All;

   procedure BB_Reset is
   begin
      for R of TSSI_Registers loop
         BB_Set (R, TXPW_TSSI_HOLD);
      end loop;
      Reset_All;
      for R of TSSI_Registers loop
         BB_Clear (R, TXPW_TSSI_HOLD);
      end loop;
   end BB_Reset;

   procedure Load_Tables (Card : Board; Counts : out Table_Counts; Result : out Outcome) is
      Status : RTW89_PHY_Tables.Load_Status;
   begin
      Counts := (others => 0);
      Result := Success;
      Out_Of_Window := 0;
      Load_BB (RTW89_Tables_8852B.BB_Registers, Card.RFE, Card.Cut, Counts.BB, Status);
      Check (Status, Table_BB, Result);
      if not OK (Result) then
         return;
      end if;
      BB_Reset;
      Current_Path := A;
      Load_RF (RTW89_Tables_8852B.Radio_A, Card.RFE, Card.Cut, Counts.Radio_A, Status);
      Check (Status, Table_Radio_A, Result);
      if not OK (Result) then
         return;
      end if;
      Current_Path := B;
      Load_RF (RTW89_Tables_8852B.Radio_B, Card.RFE, Card.Cut, Counts.Radio_B, Status);
      Check (Status, Table_Radio_B, Result);
   end Load_Tables;

   ---------------------------------------------------------------------------
   --  Coexistence: PTA init, then the grant forced to WLAN
   ---------------------------------------------------------------------------
   GPIO_MUXCFG    : constant Register := 16#0040#;
   ENBT           : constant Unsigned_8 := 16#20#;
   BTMODE_MASK    : constant Unsigned_8 := 16#C0#;
   GPIO_MUXCFG_1  : constant Register := 16#0041#;
   MUXCFG_INNER_CLEAR : constant Unsigned_8 := 16#04#;
   MUXCFG_INNER_SET   : constant Unsigned_8 := 16#02#;
   BTC_FUNC_EN    : constant Register := 16#DA20#;
   PTA_WL_TX_EN   : constant Unsigned_8 := 16#02#;
   BT_COEX_CFG_2_1 : constant Register := 16#DA35#;
   GNT_BT_POLARITY : constant Unsigned_8 := 16#01#;
   CSR_MODE       : constant Register := 16#DA40#;
   STATIS_BT_EN_WL_ACT_MSK : constant Unsigned_8 := 16#0C#;
   CSR_MODE_2     : constant Register := 16#DA42#;
   BT_CNT_RST_HIGH : constant Unsigned_8 := 16#01#;
   TRXPTCL_RESP_0_3 : constant Register := 16#CC07#;
   RSP_CHK_BTCCA_HIGH : constant Unsigned_8 := 16#02#;
   CCA_CFG_0      : constant Register := 16#C340#;
   BTCCA_EN       : constant Unsigned_16 := 16#0020#;
   BTCCA_BRK_TXOP_EN : constant Unsigned_16 := 16#0200#;
   LTE_SW_CFG_1   : constant Unsigned_32 := 16#38#;
   LTE_SW_CFG_2   : constant Unsigned_32 := 16#3C#;
   WL_RX_CTRL     : constant Unsigned_32 := 16#0000_0100#;
   TDMA_MODE      : constant Register := 16#DA4C#;
   RTK_BT_ENABLE  : constant Unsigned_8 := 16#01#;
   BT_COEX_CFG_5  : constant Register := 16#DA6C#;
   SAMPLE_RATE_MASK : constant Unsigned_8 := 16#3F#;
   RTK_RATE       : constant Unsigned_8 := 5;
   BTC_COEX_MSK_TABLE : constant Register := 16#DA30#;
   PRI_MASK_TX_RESP : constant Unsigned_32 := 16#0000_0008#;
   WL_PRI_MSK     : constant Register := 16#DA10#;
   PTA_WL_PRI_MASK_BCNQ : constant Unsigned_32 := 16#0000_0100#;
   BTC_BREAK_TABLE : constant Register := 16#DA2C#;
   BTC_BREAK_PARAM : constant Unsigned_32 := 16#F0FF_FFFF#;
   CSR_BT_CNT_RST_STATIS : constant Unsigned_32 := 16#0001_0004#;
   --  The grant: WL forced high and BT forced low on both paths
   --  (rtw89_mac_cfg_gnt with BTC_ANT_WONLY).
   GNT_WL_ONLY    : constant Unsigned_32 := 16#7700_7700#;
   SYS_SDIO_CTRL_3 : constant Register := 16#0073#;
   CTRL_PATH_WL   : constant Unsigned_8 := 16#04#;
   BT_PLT         : constant Register := 16#C67C#;
   PLT_GNT_WL     : constant Unsigned_16 := 16#0111#;   --  TX_PLT_GNT_WL | RX_PLT_GNT_WL | PLT_EN
   --  RF trx mask LUT (RR_LUTWE / RR_LUTWA / RR_LUTWD0).
   RR_WLSEL   : constant Unsigned_32 := 16#02#;
   RR_LUTWE   : constant Unsigned_32 := 16#EF#;
   RR_LUTWA   : constant Unsigned_32 := 16#33#;
   RR_LUTWD0  : constant Unsigned_32 := 16#3F#;
   LUT_WRITE_ENABLE : constant Unsigned_32 := 16#2_0000#;
   GROUP_SS   : constant Unsigned_32 := 0;
   GROUP_TX   : constant Unsigned_32 := 2;
   TRX_MASK_ALL : constant Unsigned_32 := 16#5FF#;
   TRX_MASK_B_TX : constant Unsigned_32 := 16#55F#;
   --  LTE indirect access (rtw89_mac_write_lte / read_lte).
   LTE_CTRL       : constant Register := 16#DAF0#;
   LTE_CTRL_READY : constant Register := 16#DAF3#;
   LTE_READY_BIT  : constant Unsigned_8 := 16#20#;
   LTE_WDATA      : constant Register := 16#DAF4#;
   LTE_RDATA      : constant Register := 16#DAF8#;
   LTE_WRITE_CMD  : constant Unsigned_32 := 16#C00F_0000#;
   LTE_READ_CMD   : constant Unsigned_32 := 16#800F_0000#;
   LTE_Budget     : constant Microseconds := 50_000;

   procedure LTE_Write (Offset, Value : Unsigned_32; OK : out Boolean) is
   begin
      OK := Poll8 (LTE_CTRL_READY, LTE_READY_BIT, LTE_READY_BIT, LTE_Budget);
      if OK then
         Write32 (LTE_WDATA, Value);
         Write32 (LTE_CTRL, LTE_WRITE_CMD or Offset);
      end if;
   end LTE_Write;

   procedure LTE_Read (Offset : Unsigned_32; Value : out Unsigned_32; OK : out Boolean) is
   begin
      Value := 0;
      OK := Poll8 (LTE_CTRL_READY, LTE_READY_BIT, LTE_READY_BIT, LTE_Budget);
      if OK then
         Write32 (LTE_CTRL, LTE_READ_CMD or Offset);
         Value := Read32 (LTE_RDATA);
      end if;
   end LTE_Read;

   procedure Set_TRX_Mask (P : RF_Path; Group, Value : Unsigned_32) is
   begin
      RF_Full (P, RR_LUTWE, LUT_WRITE_ENABLE);
      RF_Full (P, RR_LUTWA, Group);
      RF_Full (P, RR_LUTWD0, Value);
      RF_Full (P, RR_LUTWE, 0);
   end Set_TRX_Mask;

   procedure Grant_Antenna_To_WLAN (Result : out Outcome) is
      Value : Unsigned_32;
      OK : Boolean;
   begin
      Result := Success;
      --  rtw89_mac_coex_init (RTK mode, inner direction).
      Set8 (GPIO_MUXCFG, ENBT);
      Set8 (BTC_FUNC_EN, PTA_WL_TX_EN);
      Set8 (BT_COEX_CFG_2_1, GNT_BT_POLARITY);
      Set8 (CSR_MODE, STATIS_BT_EN_WL_ACT_MSK);
      Set8 (CSR_MODE_2, BT_CNT_RST_HIGH);
      Clear8 (TRXPTCL_RESP_0_3, RSP_CHK_BTCCA_HIGH);
      Write16 (CCA_CFG_0, (Read16 (CCA_CFG_0) or BTCCA_EN) and not BTCCA_BRK_TXOP_EN);
      LTE_Read (LTE_SW_CFG_2, Value, OK);
      if OK then
         LTE_Write (LTE_SW_CFG_2, Value and WL_RX_CTRL, OK);
      end if;
      if not OK then
         Result := Failure (Poll_Timeout, LTE_CTRL, Read32 (LTE_CTRL));
         return;
      end if;
      Write8 (GPIO_MUXCFG, Read8 (GPIO_MUXCFG) and not BTMODE_MASK);
      Write8 (TDMA_MODE, Read8 (TDMA_MODE) or RTK_BT_ENABLE);
      Write8 (BT_COEX_CFG_5, (Read8 (BT_COEX_CFG_5) and not SAMPLE_RATE_MASK) or RTK_RATE);
      Write8 (GPIO_MUXCFG_1, (Read8 (GPIO_MUXCFG_1) and not MUXCFG_INNER_CLEAR) or MUXCFG_INNER_SET);
      --  rtw8852bx_btc_init_cfg's rest (shared antenna, RFE 1).
      Set32 (BTC_COEX_MSK_TABLE, PRI_MASK_TX_RESP);
      Set32 (WL_PRI_MSK, PTA_WL_PRI_MASK_BCNQ);
      RF_Full (A, RR_WLSEL, 0);
      RF_Full (B, RR_WLSEL, 0);
      Set_TRX_Mask (A, GROUP_SS, TRX_MASK_ALL);
      Set_TRX_Mask (B, GROUP_SS, TRX_MASK_ALL);
      Set_TRX_Mask (A, GROUP_TX, TRX_MASK_ALL);
      Set_TRX_Mask (B, GROUP_TX, TRX_MASK_B_TX);
      Write32 (BTC_BREAK_TABLE, BTC_BREAK_PARAM);
      Set32 (CSR_MODE, CSR_BT_CNT_RST_STATIS);
      --  The grant to WLAN, the control path to WLAN, the PTA LUT.
      LTE_Write (LTE_SW_CFG_1, GNT_WL_ONLY, OK);
      if not OK then
         Result := Failure (Poll_Timeout, LTE_CTRL, Read32 (LTE_CTRL));
         return;
      end if;
      Set8 (SYS_SDIO_CTRL_3, CTRL_PATH_WL);
      Write16 (BT_PLT, PLT_GNT_WL);
   end Grant_Antenna_To_WLAN;

   ---------------------------------------------------------------------------
   --  DM init (RX side)
   ---------------------------------------------------------------------------
   R_P0_EN_SOUND_WO_NDP : constant BB_Address := 16#0D7C#;
   R_P1_EN_SOUND_WO_NDP : constant BB_Address := 16#2D7C#;
   EN_SOUND_WO_NDP      : constant Unsigned_32 := 2 ** 1;
   R_P0_RPL1            : constant BB_Address := 16#49B0#;
   R_P1_RPL1            : constant BB_Address := 16#4A00#;
   RPL1_BIAS_MASK       : constant Unsigned_32 := 16#FF#;
   R_PLCP_HISTOGRAM     : constant BB_Address := 16#0738#;
   DIS_TRIG_BY_FAIL_BRK : constant Unsigned_32 := 16#C#;
   PHYSTS_PAGE_FIRST    : constant BB_Address := 16#073C#;
   IE13_DL_MU_DEF       : constant Unsigned_32 := 2 ** 13;
   IE01_CMN_OFDM        : constant Unsigned_32 := 2 ** 1;
   IE04_TO_07           : constant Unsigned_32 := 16#F0#;
   IE20_DBG_OFDM_FD     : constant Unsigned_32 := 2 ** 20;
   R_SEG0R_PD_V1        : constant BB_Address := 16#4860#;
   PD_LOWER_BOUND       : constant Unsigned_32 := 16#0000_07C0#;
   PD_SPATIAL_REUSE_EN  : constant Unsigned_32 := 16#4000_0000#;
   R_BMODE_PDTH_EN_V1   : constant BB_Address := 16#4B74#;
   BMODE_PDTH_LIMIT_EN  : constant Unsigned_32 := 16#4000_0000#;
   R_BMODE_PDTH_V1      : constant BB_Address := 16#4B64#;
   BMODE_PDTH_LOWER     : constant Unsigned_32 := 16#FF00_0000#;
   CCK_PD_LOWER_DEFAULT : constant Unsigned_32 := 16#80#;      --  -128: no floor
   PAGC_FOLLOW_REGS     : constant array (1 .. 4) of BB_Address := [16#46E8#, 16#46EC#, 16#47A8#, 16#47AC#];
   PAGC_FOLLOW_EN       : constant Unsigned_32 := 16#20#;
   XTAL_SI_XTAL_SC_XO   : constant XTAL_Offset := 16#05#;
   XTAL_SI_XTAL_SC_XI   : constant XTAL_Offset := 16#04#;
   XTAL_CAP_MASK        : constant Unsigned_8 := 16#7F#;
   R_DCFO_OPT           : constant BB_Address := 16#4494#;
   DCFO_OPT_EN          : constant Unsigned_32 := 16#2000_0000#;
   R_DCFO_WEIGHT        : constant BB_Address := 16#4490#;
   DCFO_WEIGHT_MASK     : constant Unsigned_32 := 16#0F00_0000#;
   DCFO_WEIGHT          : constant Unsigned_32 := 8;
   R_IOQ_IQK_DPK        : constant BB_Address := 16#0C60#;
   R_GNT_BT_WGT_EN      : constant BB_Address := 16#0C6C#;
   R_P0_PATH_RST        : constant BB_Address := 16#58AC#;
   R_P1_PATH_RST        : constant BB_Address := 16#78AC#;
   PATH_RST             : constant Unsigned_32 := 16#0800_0000#;
   R_NCTL_CFG           : constant BB_Address := 16#8000#;
   NCTL_CFG_INIT        : constant Unsigned_32 := 16#8#;
   R_NCTL_RPT           : constant BB_Address := 16#8080#;
   NCTL_POLL            : constant Unsigned_32 := 16#4#;
   NCTL_Attempts        : constant := 100;           --  10 us apart: rtw89's 1000 us

   --  The RPL bases rtw89 reads after the BB table (efuse_gain offset_base,
   --  rssi_base), as signed octets.
   Offset_Base, RSSI_Base : Integer := 0;

   function Signed_8 (V : Unsigned_32) return Integer is
     (if (V and 16#80#) /= 0 then Integer (V and 16#FF#) - 256 else Integer (V and 16#FF#));

   procedure DM_Init (Card : Board; Counts : in out Table_Counts; Result : out Outcome) is
      Cap : constant Unsigned_8 := Card.Logical (RTW89_Efuse.Crystal_Trim_Offset) and XTAL_CAP_MASK;
      OK : Boolean;
      Ready : Boolean := False;
      Status : RTW89_PHY_Tables.Load_Status;
   begin
      Result := Success;
      --  bb_sethw (the MACID power-limit table is transmit-side).
      BB_Clear (R_P0_EN_SOUND_WO_NDP, EN_SOUND_WO_NDP);
      BB_Clear (R_P1_EN_SOUND_WO_NDP, EN_SOUND_WO_NDP);
      Offset_Base := Signed_8 (BB_Read (R_P0_RPL1, RPL1_BIAS_MASK));
      RSSI_Base := Signed_8 (BB_Read (R_P1_RPL1, RPL1_BIAS_MASK));
      --  PHY status contents.
      BB_Set (R_PLCP_HISTOGRAM, DIS_TRIG_BY_FAIL_BRK);
      for Page in 0 .. 15 loop
         if Page /= 9 then
            declare
               J : constant Natural := (if Page < 9 then Page else Page - 1);
               Where : constant BB_Address := PHYSTS_PAGE_FIRST + BB_Address (4 * J);
               V : Unsigned_32 := Read32 (BB (Where));
            begin
               case Page is
                  when 6 | 7 => V := V or IE13_DL_MU_DEF;
                  when 10    => V := V or IE13_DL_MU_DEF or IE01_CMN_OFDM;
                  when 11    => V := (V and not IE04_TO_07) or IE01_CMN_OFDM;
                  when 12    => V := V and not IE04_TO_07;
                  when 13 .. 15 => V := (V and not IE04_TO_07) or IE20_DBG_OFDM_FD;
                  when others => null;
               end case;
               BB_Write (Where, V);
            end;
         end if;
      end loop;
      --  Packet detection: no floors (DIG reset).
      BB_Mask (R_SEG0R_PD_V1, PD_LOWER_BOUND, 0);
      BB_Mask (R_SEG0R_PD_V1, PD_SPATIAL_REUSE_EN, 0);
      BB_Mask (R_BMODE_PDTH_EN_V1, BMODE_PDTH_LIMIT_EN, 0);
      BB_Mask (R_BMODE_PDTH_V1, BMODE_PDTH_LOWER, CCK_PD_LOWER_DEFAULT);
      for R of PAGC_FOLLOW_REGS loop
         BB_Mask (R, PAGC_FOLLOW_EN, 0);
      end loop;
      --  Crystal trim from the efuse, digital CFO.
      XTAL_Write (XTAL_SI_XTAL_SC_XO, Cap, MR.XTAL_SI_FULL_MASK, OK);
      if OK then
         XTAL_Write (XTAL_SI_XTAL_SC_XI, Cap, MR.XTAL_SI_FULL_MASK, OK);
      end if;
      if not OK then
         Result := Failure (XTAL_Timeout, WLAN_XTAL_SI_CTRL, Read32 (WLAN_XTAL_SI_CTRL));
         return;
      end if;
      BB_Mask (R_DCFO_OPT, DCFO_OPT_EN, 1);
      BB_Mask (R_DCFO_WEIGHT, DCFO_WEIGHT_MASK, DCFO_WEIGHT);
      --  NCTL engine, then its table.
      BB_Set (R_IOQ_IQK_DPK, 16#3#);
      BB_Set (R_GNT_BT_WGT_EN, 16#1#);
      BB_Set (R_P0_PATH_RST, PATH_RST);
      BB_Set (R_P1_PATH_RST, PATH_RST);
      BB_Set (R_IOQ_IQK_DPK, 16#2#);
      BB_Write (R_NCTL_CFG, NCTL_CFG_INIT);
      for Attempt in 1 .. NCTL_Attempts loop
         BB_Write (R_NCTL_RPT, NCTL_POLL);
         Delay_For (1);
         Ready := Read32 (BB (R_NCTL_RPT)) = NCTL_POLL;
         exit when Ready;
         Delay_For (10);
      end loop;
      if not Ready then
         Result := Failure (Poll_Timeout, BB (R_NCTL_RPT), Read32 (BB (R_NCTL_RPT)));
         return;
      end if;
      Out_Of_Window := 0;
      Load_BB (RTW89_Tables_8852B.NCTL, Card.RFE, Card.Cut, Counts.NCTL, Status);
      Check (Status, Table_NCTL, Result);
   end DM_Init;

   ---------------------------------------------------------------------------
   --  Calibration (rtw8852b_rfk.c: _rck, _dac_cal, _rx_dck)
   ---------------------------------------------------------------------------
   RR_MOD     : constant Unsigned_32 := 16#00#;
   RR_MOD_MASK : constant Unsigned_32 := 16#F_0000#;
   RR_MOD_V_RX : constant Unsigned_32 := 3;
   RR_MOD_V_TX : constant Unsigned_32 := 2;
   RR_MODOPT  : constant Unsigned_32 := 16#01#;
   RR_RSV1    : constant Unsigned_32 := 16#05#;
   RR_RSV1_RST : constant Unsigned_32 := 16#1#;
   RR_RCKC    : constant Unsigned_32 := 16#1B#;
   RR_RCKC_TRIGGER : constant Unsigned_32 := 16#0_0240#;
   RR_RCKC_CA : constant Unsigned_32 := 16#7C00#;
   RR_RCKS    : constant Unsigned_32 := 16#1C#;
   RR_RCKS_DONE : constant Unsigned_32 := 16#8#;
   RR_DCK     : constant Unsigned_32 := 16#92#;
   RR_DCK_FINE : constant Unsigned_32 := 16#2#;
   RR_DCK_LV  : constant Unsigned_32 := 16#1#;
   RR_DCK1    : constant Unsigned_32 := 16#93#;
   RR_DCK1_CLR : constant Unsigned_32 := 16#F#;
   RR_MOD_DACK : constant Unsigned_32 := 16#3_37E1#;
   RCK_Attempts  : constant := 15;      --  2 us apart: rtw89's 30 us
   RX_Mode_Attempts : constant := 2_500; --  2 us apart: rtw89's 5000 us
   Calibration_Budget : constant Microseconds := 10_000;

   Timeout_Count : Natural := 0;

   procedure Rck (P : RF_Path) is
      Saved : constant Unsigned_32 := RF_Read (P, RR_RSV1, RF_Mask);
      Value : Unsigned_32;
   begin
      RF_Write (P, RR_RSV1, RR_RSV1_RST, 0);
      RF_Write (P, RR_MOD, RR_MOD_MASK, RR_MOD_V_RX);
      RF_Full (P, RR_RCKC, RR_RCKC_TRIGGER);
      for Attempt in 1 .. RCK_Attempts loop
         exit when RF_Read (P, RR_RCKS, RR_RCKS_DONE) /= 0;
         Delay_For (2);
      end loop;
      if RF_Read (P, RR_RCKS, RR_RCKS_DONE) = 0 then
         Timeout_Count := Timeout_Count + 1;
      end if;
      Value := RF_Read (P, RR_RCKC, RR_RCKC_CA);
      --  rtw89 writes the right-aligned field back to the whole register.
      RF_Full (P, RR_RCKC, Value);
      RF_Full (P, RR_RSV1, Saved);
   end Rck;

   procedure Count_Unless (Done : Boolean) is
   begin
      if not Done then
         Timeout_Count := Timeout_Count + 1;
      end if;
   end Count_Unless;

   --  AFE init table (rtw8852b_afe_init_defs).
   AFE_Init : constant Program :=
     [M (16#C0D4#, Full_Mask_32, 16#4486_888C#), M (16#C0D8#, Full_Mask_32, 16#C6BA_10E0#),
      M (16#C0DC#, Full_Mask_32, 16#30C5_2868#), M (16#C0E0#, Full_Mask_32, 16#0500_8128#),
      M (16#C0E4#, Full_Mask_32, 16#0000_272B#), M (16#C1D4#, Full_Mask_32, 16#4486_888C#),
      M (16#C1D8#, Full_Mask_32, 16#C6BA_10E0#), M (16#C1DC#, Full_Mask_32, 16#30C5_2868#),
      M (16#C1E0#, Full_Mask_32, 16#0500_8128#), M (16#C1E4#, Full_Mask_32, 16#0000_272B#)];
   PHYREG_SET_ALL : constant Unsigned_32 := 16#0000_000F#;

   --  _check_addc debug-port selection (the averaged reads are only a
   --  debug print in rtw89).
   function Check_ADDC (P : RF_Path) return Program is
     [M (16#20F4#, 16#0100_0000#, 0), M (16#20F8#, 16#8000_0000#, 1), M (16#20F0#, 16#00FF_0000#, 1),
      M (16#20F0#, 16#0F00#, 2), M (16#20F0#, 16#000F#, 0),
      M (16#20F0#, 16#00C0#, (if P = A then 2 else 3))];

   ADDCK_S0_Start : constant Program :=
     [M (16#C0F4#, 16#30#, 0), M (16#C1D4#, 16#30#, 0), M (16#12B8#, 16#4000_0000#, 1),
      M (16#032C#, 16#4000_0000#, 0), M (16#032C#, 16#0040_0000#, 0), M (16#032C#, 16#0040_0000#, 1),
      M (16#030C#, 16#0F00_0000#, 16#F#), M (16#032C#, 16#0001_0000#, 0), M (16#C0D4#, 16#2#, 1),
      M (16#030C#, 16#0F00_0000#, 16#3#)];
   ADDCK_S0_Trigger : constant Program :=
     [M (16#C0F4#, 16#800#, 1), M (16#C0F4#, 16#800#, 0), Wait (1), M (16#C0F4#, 16#300#, 1)];
   ADDCK_S0_End : constant Program :=
     [M (16#C0D4#, 16#2#, 0), M (16#032C#, 16#0001_0000#, 1), M (16#030C#, 16#0F00_0000#, 16#C#),
      M (16#032C#, 16#4000_0000#, 1), M (16#12B8#, 16#4000_0000#, 0)];
   ADDCK_S1_Start : constant Program :=
     [M (16#32B8#, 16#4000_0000#, 1), M (16#032C#, 16#4000_0000#, 0), M (16#032C#, 16#0040_0000#, 0),
      M (16#032C#, 16#0040_0000#, 1), M (16#030C#, 16#0F00_0000#, 16#F#), M (16#032C#, 16#0001_0000#, 0),
      M (16#C1D4#, 16#2#, 1), M (16#030C#, 16#0F00_0000#, 16#3#)];
   ADDCK_S1_Trigger : constant Program :=
     [M (16#C1F4#, 16#800#, 1), M (16#C1F4#, 16#800#, 0), Wait (1), M (16#C1F4#, 16#300#, 1)];
   ADDCK_S1_End : constant Program :=
     [M (16#C1D4#, 16#2#, 0), M (16#032C#, 16#0001_0000#, 1), M (16#030C#, 16#0F00_0000#, 16#C#),
      M (16#032C#, 16#4000_0000#, 1), M (16#32B8#, 16#4000_0000#, 0)];

   DACK_S0_1 : constant Program :=
     [M (16#12A0#, 16#8000#, 1), M (16#12A0#, 16#7000#, 3), M (16#12B8#, 16#4000_0000#, 1),
      M (16#030C#, 16#1000_0000#, 1), M (16#032C#, 16#8000_0000#, 0), M (16#C0D8#, 16#1_0000#, 1),
      M (16#C0DC#, 16#0C00_0000#, 3), M (16#C004#, 16#4000_0000#, 0), M (16#C024#, 16#4000_0000#, 0),
      M (16#C004#, 16#3FF0_0000#, 16#30#), M (16#C004#, 16#C000_0000#, 0), M (16#C004#, 16#2_0000#, 1),
      M (16#C024#, 16#2_0000#, 1), M (16#C00C#, 16#4#, 0), M (16#C02C#, 16#4#, 0),
      M (16#C004#, 16#1#, 1), M (16#C024#, 16#1#, 1), Wait (1)];
   DACK_S0_2 : constant Program :=
     [M (16#C0DC#, 16#0C00_0000#, 0), M (16#C00C#, 16#4#, 1), M (16#C02C#, 16#4#, 1)];
   DACK_S0_3 : constant Program :=
     [M (16#C004#, 16#1#, 0), M (16#C024#, 16#1#, 0), M (16#C0D8#, 16#1_0000#, 0),
      M (16#12A0#, 16#8000#, 0), M (16#12A0#, 16#7000#, 7)];
   DACK_S1_1 : constant Program :=
     [M (16#32A0#, 16#8000#, 1), M (16#32A0#, 16#7000#, 3), M (16#32B8#, 16#4000_0000#, 1),
      M (16#030C#, 16#1000_0000#, 1), M (16#032C#, 16#8000_0000#, 0), M (16#C1D8#, 16#1_0000#, 1),
      M (16#C1DC#, 16#0C00_0000#, 3), M (16#C104#, 16#4000_0000#, 0), M (16#C124#, 16#4000_0000#, 0),
      M (16#C104#, 16#3FF0_0000#, 16#30#), M (16#C104#, 16#C000_0000#, 0), M (16#C104#, 16#2_0000#, 1),
      M (16#C124#, 16#2_0000#, 1), M (16#C10C#, 16#4#, 0), M (16#C12C#, 16#4#, 0),
      M (16#C104#, 16#1#, 1), M (16#C124#, 16#1#, 1), Wait (1)];
   DACK_S1_2 : constant Program :=
     [M (16#C1DC#, 16#0C00_0000#, 0), M (16#C10C#, 16#4#, 1), M (16#C12C#, 16#4#, 1), Wait (1)];
   DACK_S1_3 : constant Program :=
     [M (16#C104#, 16#1#, 0), M (16#C124#, 16#1#, 0), M (16#C1D8#, 16#1_0000#, 0),
      M (16#32A0#, 16#8000#, 0), M (16#32A0#, 16#7000#, 7)];
   DADC_Enable_B : constant Program :=
     [M (16#032C#, 16#4000_0000#, 0), M (16#030C#, 16#0F00_0000#, 16#F#), M (16#030C#, 16#0F00_0000#, 16#3#),
      M (16#032C#, 16#1_0000#, 0), M (16#32DC#, 16#1#, 1), M (16#32E8#, 16#4#, 1),
      (Write_RF_B, 16#8F#, 16#2000#, 1)];
   DADC_Disable_B : constant Program :=
     [M (16#32DC#, 16#1#, 0), M (16#32E8#, 16#4#, 0), (Write_RF_B, 16#8F#, 16#2000#, 0),
      M (16#032C#, 16#1_0000#, 1)];

   ADDCK_Result_S0 : constant BB_Address := 16#C0FC#;
   ADDCK_Result_S1 : constant BB_Address := 16#C1FC#;
   ADDCK_Done      : constant Unsigned_32 := 16#1#;
   ADDCK_Value_Mask : constant Unsigned_32 := 16#3FF#;
   ADDCK_High_Shift : constant := 10;
   ADDCK_Low_Bits  : constant := 6;
   ADDCK_Low_Mask  : constant Unsigned_32 := 16#3F#;

   procedure Wait_BB (Where : BB_Address; Bits : Unsigned_32) is
   begin
      Count_Unless (Poll32 (BB (Where), Bits, Bits, Calibration_Budget));
   end Wait_BB;

   --  Both (S0) or either (S1) of two done bits: rtw89 uses AND for S0
   --  and OR for S1, kept as is.
   procedure Wait_Both (First, Second : BB_Address; Bits : Unsigned_32) is
      Done : Boolean := False;
   begin
      for Attempt in 1 .. Natural (Calibration_Budget) loop
         Done := (Read32 (BB (First)) and Bits) /= 0 and then (Read32 (BB (Second)) and Bits) /= 0;
         exit when Done;
         Delay_For (1);
      end loop;
      Count_Unless (Done);
   end Wait_Both;

   procedure Wait_Either (First, Second : BB_Address; Bits : Unsigned_32) is
      Done : Boolean := False;
   begin
      for Attempt in 1 .. Natural (Calibration_Budget) loop
         Done := (Read32 (BB (First)) and Bits) /= 0 or else (Read32 (BB (Second)) and Bits) /= 0;
         exit when Done;
         Delay_For (1);
      end loop;
      Count_Unless (Done);
   end Wait_Either;

   DACK_Done_1 : constant Unsigned_32 := 2 ** 31;
   DACK_Done_2 : constant Unsigned_32 := 2 ** 2;
   DACK_Index_Mask : constant Unsigned_32 := 16#1E#;
   DACK_Indexes : constant := 16;

   procedure DACK_Backup (Debug : BB_Address; Index_1, Index_2 : BB_Address) is
   begin
      BB_Mask (Debug, 16#4000_0000#, 1);
      for I in 0 .. DACK_Indexes - 1 loop
         BB_Mask (Index_1, DACK_Index_Mask, Unsigned_32 (I));
         BB_Mask (Index_2, DACK_Index_Mask, Unsigned_32 (I));
      end loop;
   end DACK_Backup;

   procedure DAC_Calibrate is
      RF0_A : constant Unsigned_32 := RF_Read (A, RR_MOD, RF_Mask);
      RF0_B : constant Unsigned_32 := RF_Read (B, RR_MOD, RF_Mask);
      DRCK  : Unsigned_32;
      A00, A01, A10, A11 : Unsigned_32;
   begin
      Write32 (MR.PHYREG_SET, PHYREG_SET_ALL);
      Run (AFE_Init);
      --  D-die RCK.
      BB_Mask (16#C0CC#, 16#40#, 1);
      Wait_BB (16#C0D0#, 16#8#);
      BB_Mask (16#C0CC#, 16#40#, 0);
      BB_Mask (16#C094#, 16#200#, 1);
      Delay_For (1);
      BB_Mask (16#C094#, 16#200#, 0);
      DRCK := BB_Read (16#C0D0#, 16#000F_8000#);
      BB_Mask (16#C0CC#, 16#200#, 0);
      BB_Mask (16#C0CC#, 16#1F#, DRCK);
      RF_Write (A, RR_RSV1, RR_RSV1_RST, 0);
      RF_Write (B, RR_RSV1, RR_RSV1_RST, 0);
      RF_Full (A, RR_MOD, RR_MOD_DACK);
      RF_Full (B, RR_MOD, RR_MOD_DACK);
      --  ADC DC calibration.
      Run (ADDCK_S0_Start);
      Run (Check_ADDC (A));
      Run (ADDCK_S0_Trigger);
      Wait_BB (ADDCK_Result_S0, ADDCK_Done);
      Run (Check_ADDC (A));
      Run (ADDCK_S0_End);
      Run (ADDCK_S1_Start);
      Run (Check_ADDC (B));
      Run (ADDCK_S1_Trigger);
      Wait_BB (ADDCK_Result_S1, ADDCK_Done);
      Run (Check_ADDC (B));
      Run (ADDCK_S1_End);
      --  Back up and reload the ADC DC offsets as manual values.
      BB_Mask (16#C0F4#, 16#300#, 0);
      A00 := Shift_Right (Read32 (BB (ADDCK_Result_S0)), ADDCK_High_Shift) and ADDCK_Value_Mask;
      A01 := Read32 (BB (ADDCK_Result_S0)) and ADDCK_Value_Mask;
      BB_Mask (16#C1F4#, 16#300#, 0);
      A10 := Shift_Right (Read32 (BB (ADDCK_Result_S1)), ADDCK_High_Shift) and ADDCK_Value_Mask;
      A11 := Read32 (BB (ADDCK_Result_S1)) and ADDCK_Value_Mask;
      BB_Mask (16#C0F0#, 16#03FF_0000#, A00);
      BB_Mask (16#C0F4#, 16#F#, Shift_Right (A01, ADDCK_Low_Bits));
      BB_Mask (16#C0F0#, 16#FC00_0000#, A01 and ADDCK_Low_Mask);
      BB_Mask (16#C0F4#, 16#30#, 3);
      BB_Mask (16#C1F0#, 16#03FF_0000#, A10);
      BB_Mask (16#C1F4#, 16#F#, Shift_Right (A11, ADDCK_Low_Bits));
      BB_Mask (16#C1F0#, 16#FC00_0000#, A11 and ADDCK_Low_Mask);
      BB_Mask (16#C1F4#, 16#30#, 3);
      RF_Full (A, RR_MODOPT, 0);
      RF_Full (B, RR_MODOPT, 0);
      --  DAC calibration, S0 then S1.
      Run (DACK_S0_1);
      Wait_Both (16#C040#, 16#C064#, DACK_Done_1);
      Run (DACK_S0_2);
      Wait_Both (16#C05C#, 16#C080#, DACK_Done_2);
      Run (DACK_S0_3);
      DACK_Backup (16#12B8#, 16#C000#, 16#C020#);
      BB_Mask (16#12B8#, 16#4000_0000#, 0);
      Run (DACK_S1_1);
      Wait_Either (16#C140#, 16#C164#, DACK_Done_1);
      Run (DACK_S1_2);
      Wait_Either (16#C15C#, 16#C180#, DACK_Done_2);
      Run (DACK_S1_3);
      Run (DADC_Enable_B);
      Run (Check_ADDC (B));
      Run (DADC_Disable_B);
      DACK_Backup (16#32B8#, 16#C100#, 16#C120#);
      BB_Mask (16#32B8#, 16#4000_0000#, 0);
      RF_Full (A, RR_MOD, RF0_A);
      RF_Full (B, RR_MOD, RF0_B);
      RF_Write (A, RR_RSV1, RR_RSV1_RST, 1);
      RF_Write (B, RR_RSV1, RR_RSV1_RST, 1);
   end DAC_Calibrate;

   procedure RX_DCK_Path (P : RF_Path) is
      Saved : Unsigned_32;
      Tune  : Unsigned_32;
   begin
      for Attempt in 1 .. RX_Mode_Attempts loop
         exit when RF_Read (P, RR_MOD, RR_MOD_MASK) /= RR_MOD_V_TX;
         Delay_For (2);
      end loop;
      Saved := RF_Read (P, RR_RSV1, RF_Mask);
      Tune := RF_Read (P, RR_DCK, RR_DCK_FINE);
      RF_Write (P, RR_RSV1, RR_RSV1_RST, 0);
      RF_Write (P, RR_DCK, RR_DCK_FINE, 0);
      RF_Write (P, RR_MOD, RR_MOD_MASK, RR_MOD_V_RX);
      RF_Write (P, RR_DCK1, RR_DCK1_CLR, 0);
      RF_Write (P, RR_DCK, RR_DCK_LV, 0);
      RF_Write (P, RR_DCK, RR_DCK_LV, 1);
      Sleep (1);
      RF_Write (P, RR_DCK, RR_DCK_FINE, Tune);
      RF_Full (P, RR_RSV1, Saved);
   end RX_DCK_Path;

   procedure RX_DC_Calibrate is
   begin
      RX_DCK_Path (A);
      RX_DCK_Path (B);
   end RX_DC_Calibrate;

   procedure Calibrate (Timeouts : out Natural; Result : out Outcome) is
   begin
      Result := Success;
      Timeout_Count := 0;
      Rck (A);
      Rck (B);
      DAC_Calibrate;
      RX_DC_Calibrate;
      Timeouts := Timeout_Count;
   end Calibrate;

   ---------------------------------------------------------------------------
   --  RX paths and per-band settings
   ---------------------------------------------------------------------------
   R_CHBW_MOD_V1  : constant BB_Address := 16#49C4#;
   ANT_RX_SEG0    : constant Unsigned_32 := 16#F#;
   R_FC0_BW_V1    : constant BB_Address := 16#49C0#;
   ANT_RX_1RCCA_SEG0 : constant Unsigned_32 := 16#0003_C000#;
   ANT_RX_1RCCA_SEG1 : constant Unsigned_32 := 16#003C_0000#;
   RX_AB          : constant Unsigned_32 := 3;
   R_RXHT_MCS_LIMIT : constant BB_Address := 16#0D18#;
   RXHT_MAX_NSS   : constant Unsigned_32 := 16#300#;
   RXVHT_MAX_NSS  : constant Unsigned_32 := 16#0060_0000#;
   R_RXHE         : constant BB_Address := 16#0D80#;
   RXHE_USER_MAX  : constant Unsigned_32 := 16#3FC0#;
   RXHE_USER_MAX_8852B : constant Unsigned_32 := 4;
   RXHE_MAX_NSS   : constant Unsigned_32 := 16#0001_C000#;
   RXHETB_MAX_NSS : constant Unsigned_32 := 16#0380_0000#;
   NSS_TWO        : constant Unsigned_32 := 1;     --  field value: streams - 1
   R_P1_TXPW_RSTB_MANON : constant Unsigned_32 := 16#C000_0000#;
   R_P0_RFMODE    : constant BB_Address := 16#12AC#;
   R_P0_RFMODE_FTM_RX : constant BB_Address := 16#12B0#;
   R_P1_RFMODE    : constant BB_Address := 16#32AC#;
   R_P1_RFMODE_FTM_RX : constant BB_Address := 16#32B0#;
   RFMODE_MASK    : constant Unsigned_32 := 16#FFFF_FFF0#;
   RFMODE_AB      : constant Unsigned_32 := 16#123_3312#;
   RFMODE_FTM_MASK : constant Unsigned_32 := 16#FFF#;
   RFMODE_FTM_RX  : constant Unsigned_32 := 16#333#;
   R_MAC_SEL      : constant BB_Address := 16#09A4#;
   MAC_SEL_MOD    : constant Unsigned_32 := 16#1C#;

   --  rtw8852bx_set_gain_offset: the efuse RX gain offsets.
   Gain_Offset_First : constant := 16#2D4#;   --  2G OFDM, then 2G CCK, 5G L/M/H at +2
   type Offset_Slot is (OFDM_2G, CCK_2G, OFDM_5G_Low, OFDM_5G_Mid, OFDM_5G_High);
   function Offset_Byte (Card : Board; S : Offset_Slot) return Unsigned_8 is
     (Card.Logical (Gain_Offset_First + 2 * Offset_Slot'Pos (S)));
   --  The phycap compensation bytes per gain band (path A, path B).
   Comp_A : constant array (Gain_Band) of Natural := [16#5BB#, 16#5BA#, 16#5B9#, 16#5B8#];
   Comp_B : constant array (Gain_Band) of Natural := [16#590#, 16#58F#, 16#58E#, 16#58D#];
   R_P0_AGC_RSVD : constant BB_Address := 16#4ACC#;
   R_P1_AGC_RSVD : constant BB_Address := 16#4AD8#;
   R_PATH0_G_TIA1_LNA6_OP1DB_V1 : constant BB_Address := 16#4694#;
   R_PATH1_G_TIA1_LNA6_OP1DB_V1 : constant BB_Address := 16#4778#;
   G_OFST_MASK   : constant Unsigned_32 := 16#00FF_0000#;
   R_RX_RPL_OFST : constant BB_Address := 16#23AC#;
   RX_RPL_OFST_CCK : constant Unsigned_32 := 16#7F#;
   Byte_Field    : constant Unsigned_32 := 16#FF#;
   CCK_Min       : constant := -64;
   CCK_Max       : constant := 63;

   function Nibble_Signed (V : Unsigned_8) return Integer is
     (if V >= 8 then Integer (V) - 16 else Integer (V));

   --  An arithmetic right shift (C's >> on a negative s8 rounds down).
   function Floor_Div (V : Integer; Divisor : Positive) return Integer is
     (if V >= 0 then V / Divisor else -((-V + Divisor - 1) / Divisor));

   function Clamp (V, Low, High : Integer) return Integer is (Integer'Max (Low, Integer'Min (High, V)));
   function Clamp8 (V : Integer) return Integer is (Clamp (V, -128, 127));
   function Bits (V : Integer; Width_Mask : Unsigned_32) return Unsigned_32 is
     (Unsigned_32 (if V < 0 then V + 256 else V) and Width_Mask);

   procedure Set_Gain_Offset (Card : Board; Band : Gain_Band) is
      Comp_Valid : constant Boolean :=
        (for some G in Gain_Band =>
           Card.Phycap (Comp_A (G) - Phycap_First) /= RTW89_Efuse.Unprogrammed
           or else Card.Phycap (Comp_B (G) - Phycap_First) /= RTW89_Efuse.Unprogrammed);
      Offset_Valid : constant Boolean :=
        (for some S in Offset_Slot => Offset_Byte (Card, S) /= RTW89_Efuse.Unprogrammed);
      Slot : constant Offset_Slot :=
        (case Band is
           when Band_2G => OFDM_2G, when Band_5G_Low => OFDM_5G_Low,
           when Band_5G_Mid => OFDM_5G_Mid, when Band_5G_High => OFDM_5G_High);
      Low_Nibble : constant Unsigned_8 := 16#0F#;
   begin
      if Comp_Valid then
         BB_Mask (R_P0_AGC_RSVD, Byte_Field,
                  Bits (Clamp8 (4 * Nibble_Signed (Card.Phycap (Comp_A (Band) - Phycap_First) and Low_Nibble)),
                        Byte_Field));
         BB_Mask (R_P1_AGC_RSVD, Byte_Field,
                  Bits (Clamp8 (4 * Nibble_Signed (Card.Phycap (Comp_B (Band) - Phycap_First) and Low_Nibble)),
                        Byte_Field));
      end if;
      if Offset_Valid then
         declare
            Raw  : constant Unsigned_8 := Offset_Byte (Card, Slot);
            Off_A : constant Integer := Nibble_Signed (Shift_Right (Raw, 4));
            Off_B : constant Integer := Nibble_Signed (Raw and Low_Nibble);
            OA : constant Integer := -Off_A;
            OB : constant Integer := -Off_B;
            CCK : constant Integer := -Nibble_Signed (Shift_Right (Offset_Byte (Card, CCK_2G), 4));
         begin
            BB_Mask (R_PATH0_G_TIA1_LNA6_OP1DB_V1, G_OFST_MASK,
                     Bits (Clamp8 (-(4 * OA + Floor_Div (Offset_Base, 4))), Byte_Field));
            BB_Mask (R_PATH1_G_TIA1_LNA6_OP1DB_V1, G_OFST_MASK,
                     Bits (Clamp8 (-(4 * OB + Floor_Div (Offset_Base, 4))), Byte_Field));
            BB_Mask (R_P0_RPL1, RPL1_BIAS_MASK, Bits (Clamp8 (16 * OA + Offset_Base), Byte_Field));
            BB_Mask (R_P1_RPL1, RPL1_BIAS_MASK, Bits (Clamp8 (16 * OA + RSSI_Base), Byte_Field));
            if Band = Band_2G then
               BB_Mask (R_RX_RPL_OFST, RX_RPL_OFST_CCK,
                        Bits (Clamp (8 * CCK + Floor_Div (Offset_Base, 2), CCK_Min, CCK_Max), RX_RPL_OFST_CCK));
            end if;
         end;
      end if;
   end Set_Gain_Offset;

   --  Path B shares the 2.4 GHz antenna with Bluetooth (rtw89
   --  ctrl_btg_bt_rx, RFE 1 shared antenna).
   type BTG_Step is record
      Where : BB_Address;
      Mask  : Unsigned_32;
      On, Off : Unsigned_32;
   end record;
   BTG_Steps : constant array (1 .. 11) of BTG_Step :=
     [(16#4738#, 16#0008_0000#, 1, 0), (16#4738#, 16#0040_0000#, 0, 0),
      (16#476C#, 16#FF00_0000#, 16#20#, 16#1A#), (16#4778#, 16#0000_00FF#, 16#30#, 16#2A#),
      (16#4AA4#, 16#0008_0000#, 1, 0), (16#4AA4#, 16#0040_0000#, 1, 0),
      (16#0980#, 16#001E_0000#, 0, 16#C#), (16#49C4#, 16#0000_4000#, 1, 0),
      (16#49C0#, 16#03C0_0000#, 2, 0), (16#4420#, 16#8000_0000#, 1, 1),
      (16#0C6C#, 16#0020_0000#, 1, 0)];

   procedure BTG_RX (On : Boolean) is
   begin
      for S of BTG_Steps loop
         BB_Mask (S.Where, S.Mask, (if On then S.On else S.Off));
      end loop;
   end BTG_RX;

   procedure Configure_RX_Paths (Card : Board) is
   begin
      BB_Mask (R_CHBW_MOD_V1, ANT_RX_SEG0, RX_AB);
      BB_Mask (R_FC0_BW_V1, ANT_RX_1RCCA_SEG0, RX_AB);
      BB_Mask (R_FC0_BW_V1, ANT_RX_1RCCA_SEG1, RX_AB);
      BB_Mask (R_RXHT_MCS_LIMIT, RXHT_MAX_NSS, NSS_TWO);
      BB_Mask (R_RXHT_MCS_LIMIT, RXVHT_MAX_NSS, NSS_TWO);
      BB_Mask (R_RXHE, RXHE_USER_MAX, RXHE_USER_MAX_8852B);
      BB_Mask (R_RXHE, RXHE_MAX_NSS, NSS_TWO);
      BB_Mask (R_RXHE, RXHETB_MAX_NSS, NSS_TWO);
      Set_Gain_Offset (Card, Band_2G);
      BTG_RX (True);
      BB_Mask (R_P1_TXPW_RSTB, R_P1_TXPW_RSTB_MANON, 1);
      BB_Mask (R_P1_TXPW_RSTB, R_P1_TXPW_RSTB_MANON, 3);
      BB_Mask (R_P0_RFMODE, RFMODE_MASK, RFMODE_AB);
      BB_Mask (R_P0_RFMODE_FTM_RX, RFMODE_FTM_MASK, RFMODE_FTM_RX);
      BB_Mask (R_P1_RFMODE, RFMODE_MASK, RFMODE_AB);
      BB_Mask (R_P1_RFMODE_FTM_RX, RFMODE_FTM_MASK, RFMODE_FTM_RX);
      BB_Mask (R_MAC_SEL, MAC_SEL_MOD, 0);
   end Configure_RX_Paths;

   ---------------------------------------------------------------------------
   --  Channel switch
   ---------------------------------------------------------------------------
   R_ADC_FIFO   : constant BB_Address := 16#20FC#;
   ADC_FIFO_RST : constant Unsigned_32 := 16#FF00_0000#;
   ADC_FIFO_RESET_VALUE : constant Unsigned_32 := 16#F#;
   ADC_FIFO_Settle : constant Microseconds := 40;
   R_RXCCA      : constant BB_Address := 16#2344#;
   RXCCA_DIS    : constant Unsigned_32 := 16#8000_0000#;
   R_PD_CTRL    : constant BB_Address := 16#0C3C#;
   PD_HIT_DIS   : constant Unsigned_32 := 16#200#;
   WMAC_RFMOD   : constant Register := 16#C010#;
   RFMOD_MASK   : constant Unsigned_8 := 16#03#;
   TXRATE_CHK   : constant Register := 16#C628#;
   BAND_MODE    : constant Unsigned_8 := 16#10#;
   CHECK_CCK_RTS_OFDM6 : constant Unsigned_8 := 16#03#;
   R_RXSCOBC    : constant BB_Address := 16#23B0#;
   R_RXSCOCCK   : constant BB_Address := 16#23B4#;
   SCO_THRESHOLD_MASK : constant Unsigned_32 := 16#0007_FFFF#;
   R_PATH0_BAND_SEL_V1 : constant BB_Address := 16#4738#;
   R_PATH1_BAND_SEL_V1 : constant BB_Address := 16#4AA4#;
   BAND_SEL_2G  : constant Unsigned_32 := 16#0002_0000#;
   FC0_BW_INV   : constant Unsigned_32 := 16#7F#;
   --  Gain-error registers per band and path, and each entry's place.
   type Gain_Registers is array (0 .. 2) of BB_Address;
   Gain_Base : constant array (Boolean, Path) of Gain_Registers :=
     [False => [Path_A => [16#45DC#, 16#4660#, 16#4664#], Path_B => [16#4740#, 16#4744#, 16#4748#]],
      True  => [Path_A => [16#4678#, 16#467C#, 16#4680#], Path_B => [16#475C#, 16#4760#, 16#4764#]]];
   type Gain_Place is record
      Register_Index : Natural range 0 .. 2;
      Byte_Shift     : Natural;
   end record;
   Gain_Places : constant array (1 .. Gain_Entries) of Gain_Place :=
     [(0, 16), (0, 24), (1, 0), (1, 8), (1, 16), (1, 24), (2, 0), (2, 16), (2, 24)];
   R_P0_RPL2 : constant BB_Address := 16#49B4#;
   R_P0_RPL3 : constant BB_Address := 16#49B8#;
   R_P1_RPL2 : constant BB_Address := 16#4A04#;
   R_P1_RPL3 : constant BB_Address := 16#4A08#;
   RPL1_FIELD_MASK : constant Unsigned_32 := 16#FFFF_FF00#;
   FC0_BW_SET   : constant Unsigned_32 := 16#C000_0000#;
   CHBW_MOD_SBW : constant Unsigned_32 := 16#3000#;
   CHBW_MOD_PRICH : constant Unsigned_32 := 16#0F00#;
   RFMODE_ORI_RX : constant Unsigned_32 := 16#00FF_F000#;
   RFMODE_ORI_RX_AB : constant Unsigned_32 := 16#333#;
   RFMODE_ORI_RX_ONE : constant Unsigned_32 := 16#111#;
   R_P0_CFCH_BW0 : constant BB_Address := 16#C0EC#;
   R_P0_CFCH_BW1 : constant BB_Address := 16#C0E4#;
   R_P1_CFCH_BW0 : constant BB_Address := 16#C1EC#;
   R_P1_CFCH_BW1 : constant BB_Address := 16#C1E4#;
   CFCH_BW0_MASK : constant Unsigned_32 := 16#6000#;
   CFCH_BW1_MASK : constant Unsigned_32 := 16#30#;
   CFCH_BW1_20M  : constant Unsigned_32 := 2;
   RX_PATH_ONLY_A : constant Unsigned_32 := 1;
   RX_PATH_ONLY_B : constant Unsigned_32 := 2;
   R_UPD_CLK_ADC : constant BB_Address := 16#0700#;
   ENABLE_CCK    : constant Unsigned_32 := 16#20#;
   R_MAC_PIN_SEL : constant BB_Address := 16#0734#;
   CH_IDX_SEG0   : constant Unsigned_32 := 16#00FF_0000#;
   FIVE_MHZ_MASKS : constant array (1 .. 3) of BTG_Step :=
     [(16#46F8#, 16#1000#, 0, 0), (16#47B8#, 16#1000#, 0, 0), (16#4440#, 16#8000_0000#, 0, 0)];
   --  The 5 GHz BT-share clear (8852B set_channel_bb).
   BT_SHARE_CLEAR_5G : constant array (1 .. 8) of BTG_Step :=
     [(16#4738#, 16#0008_0000#, 0, 0), (16#4738#, 16#0040_0000#, 0, 0),
      (16#4AA4#, 16#0008_0000#, 0, 0), (16#4AA4#, 16#0040_0000#, 0, 0),
      (16#49C4#, 16#4000#, 0, 0), (16#49C0#, 16#03C0_0000#, 0, 0),
      (16#4420#, 16#8000_0000#, 0, 0), (16#0C6C#, 16#0020_0000#, 0, 0)];
   --  RF channel registers.
   RR_CFGCH    : constant Unsigned_32 := 16#18#;
   RR_CFGCH_V1 : constant Unsigned_32 := 16#1_0018#;
   RR_LCKST    : constant Unsigned_32 := 16#CF#;
   RR_LCKST_BIN : constant Unsigned_32 := 16#1#;
   RR_LDO      : constant Unsigned_32 := 16#B1#;
   RR_LDO_SEL  : constant Unsigned_32 := 16#1C0#;
   RR_LPF      : constant Unsigned_32 := 16#B7#;
   RR_LPF_BUSY : constant Unsigned_32 := 16#100#;
   RR_SYNFB    : constant Unsigned_32 := 16#C5#;
   RR_SYNFB_LK : constant Unsigned_32 := 16#8000#;
   RR_MMD      : constant Unsigned_32 := 16#D5#;
   RR_MMD_RST_EN : constant Unsigned_32 := 16#100#;
   RR_MMD_RST_SYN : constant Unsigned_32 := 16#40#;
   RR_LCK_TRG  : constant Unsigned_32 := 16#D3#;
   RR_LCK_TRGSEL : constant Unsigned_32 := 16#100#;
   RR_POW      : constant Unsigned_32 := 16#A0#;
   RR_POW_SYN  : constant Unsigned_32 := 16#C#;
   RR_SX       : constant Unsigned_32 := 16#AF#;
   RR_SYNLUT   : constant Unsigned_32 := 16#DD#;
   RR_SYNLUT_MOD : constant Unsigned_32 := 16#10#;
   RR_LUTWE2   : constant Unsigned_32 := 16#EE#;
   RR_LUTWE2_RTXBW : constant Unsigned_32 := 16#4#;
   RR_LUTWA_MASK : constant Unsigned_32 := 16#1F#;
   RR_LUTWA_RXBB_BW : constant Unsigned_32 := 16#12#;
   RR_LUTWD0_LB : constant Unsigned_32 := 16#3F#;
   RXBB_BW_20M : constant Unsigned_32 := 16#1B#;
   Lock_Attempts : constant := 1_000;   --  1 us apart: rtw89's 1000 us
   MMD_Settle  : constant Microseconds := 10;

   function Lock_Busy_Done return Boolean is
   begin
      for Attempt in 1 .. Lock_Attempts loop
         if RF_Read (A, RR_LPF, RR_LPF_BUSY) = 0 then
            return True;
         end if;
         Delay_For (1);
      end loop;
      return RF_Read (A, RR_LPF, RR_LPF_BUSY) = 0;
   end Lock_Busy_Done;

   --  _set_s0_arfc18: write RF A 0x18 with the LDO held, wait for the
   --  synthesizer calibration. False on timeout.
   function Set_S0_ARFC18 (Value : Unsigned_32) return Boolean is
      Saved : constant Unsigned_32 := RF_Read (A, RR_LDO, RF_Mask);
      Done : Boolean;
   begin
      RF_Write (A, RR_LDO, RR_LDO_SEL, 1);
      RF_Full (A, RR_CFGCH, Value);
      Done := Lock_Busy_Done;
      RF_Full (A, RR_LDO, Saved);
      return Done;
   end Set_S0_ARFC18;

   function Synthesizer_Locked return Boolean is (RF_Read (A, RR_SYNFB, RR_SYNFB_LK) /= 0);

   procedure Set_Channel_S0 (Value : Unsigned_32) is
      Ignore : Boolean;
      T : Unsigned_32;
   begin
      if not Set_S0_ARFC18 (Value) then
         return;
      end if;
      if not Synthesizer_Locked then
         RF_Write (A, RR_MMD, RR_MMD_RST_EN, 1);
         RF_Write (A, RR_MMD, RR_MMD_RST_SYN, 0);
         RF_Write (A, RR_MMD, RR_MMD_RST_SYN, 1);
         RF_Write (A, RR_MMD, RR_MMD_RST_EN, 0);
      end if;
      Delay_For (MMD_Settle);
      if not Synthesizer_Locked then
         RF_Write (A, RR_LCK_TRG, RR_LCK_TRGSEL, 1);
         T := RF_Read (A, RR_CFGCH, RF_Mask);
         Ignore := Set_S0_ARFC18 (T);
         RF_Write (A, RR_LCK_TRG, RR_LCK_TRGSEL, 0);
      end if;
      if not Synthesizer_Locked then
         T := RF_Read (A, RR_POW, RF_Mask);
         RF_Full (A, RR_POW, T);
         T := RF_Read (A, RR_SX, RF_Mask);
         RF_Full (A, RR_SX, T);
         RF_Write (A, RR_SYNLUT, RR_SYNLUT_MOD, 1);
         RF_Write (A, RR_POW, RR_POW_SYN, 0);
         RF_Write (A, RR_POW, RR_POW_SYN, 3);
         RF_Write (A, RR_SYNLUT, RR_SYNLUT_MOD, 0);
         RF_Write (A, RR_LCK_TRG, RR_LCK_TRGSEL, 1);
         T := RF_Read (A, RR_CFGCH, RF_Mask);
         Ignore := Set_S0_ARFC18 (T);
         RF_Write (A, RR_LCK_TRG, RR_LCK_TRGSEL, 0);
      end if;
   end Set_Channel_S0;

   type Copy is record
      P : RF_Path;
      Where : Unsigned_32;
   end record;
   Copies : constant array (1 .. 4) of Copy :=
     [(A, RR_CFGCH), (B, RR_CFGCH), (A, RR_CFGCH_V1), (B, RR_CFGCH_V1)];

   procedure RF_Channel (Band : WS.Band; Channel : WS.Channel_Number) is
      V : Unsigned_32;
   begin
      for C of Copies loop
         V := RF_Read (C.P, C.Where, RF_Mask);
         if V /= RF_Invalid then
            V := With_Channel (V and RF_Mask, Band, Channel);
            if C.P = A and then C.Where = RR_CFGCH then
               Set_Channel_S0 (V);
            else
               RF_Full (C.P, C.Where, V);
            end if;
         end if;
         RF_Write (C.P, RR_LCKST, RR_LCKST_BIN, 0);
         RF_Write (C.P, RR_LCKST, RR_LCKST_BIN, 1);
      end loop;
      for C of Copies loop
         V := RF_Read (C.P, C.Where, RF_Mask);
         if V /= RF_Invalid then
            RF_Full (C.P, C.Where, With_20_MHz (V and RF_Mask));
         end if;
      end loop;
      for P in RF_Path loop
         RF_Write (P, RR_LUTWE2, RR_LUTWE2_RTXBW, 1);
         RF_Write (P, RR_LUTWA, RR_LUTWA_MASK, RR_LUTWA_RXBB_BW);
         RF_Write (P, RR_LUTWD0, RR_LUTWD0_LB, RXBB_BW_20M);
         RF_Write (P, RR_LUTWE2, RR_LUTWE2_RTXBW, 0);
      end loop;
   end RF_Channel;

   procedure TSSI_Hold (On : Boolean) is
   begin
      for R of TSSI_Registers loop
         BB_Mask (R, TXPW_TSSI_HOLD, (if On then 1 else 0));
      end loop;
   end TSSI_Hold;

   ---------------------------------------------------------------------------
   --  Transmit power (W3)
   ---------------------------------------------------------------------------
   package TP renames RTW89_TX_Power;

   --  Thermal and PA-bias trim (phycap 0x5DF/0x5DC, 0x5DE/0x5DB).
   Thermal_Trim_Address : constant array (RF_Path) of Natural := [A => 16#5DF#, B => 16#5DC#];
   PA_Bias_Address      : constant array (RF_Path) of Natural := [A => 16#5DE#, B => 16#5DB#];
   RR_TM2      : constant Unsigned_32 := 16#43#;
   RR_TM2_OFF  : constant Unsigned_32 := 16#000F_0000#;
   RR_BIASA    : constant Unsigned_32 := 16#60#;
   RR_BIASA_TXA : constant Unsigned_32 := 16#000F_0000#;
   RR_BIASA_TXG : constant Unsigned_32 := 16#0000_F000#;
   Low_Nibble_Mask : constant Unsigned_8 := 16#0F#;

   procedure Power_Trim (Card : Board) is
      function Programmed (Address : Natural) return Boolean is
        (Card.Phycap (Address - Phycap_First) /= RTW89_Efuse.Unprogrammed);
      Thermal_PG : constant Boolean := (for some P in RF_Path => Programmed (Thermal_Trim_Address (P)));
      PA_PG : constant Boolean := (for some P in RF_Path => Programmed (PA_Bias_Address (P)));
   begin
      if Thermal_PG then
         for P in RF_Path loop
            declare
               Raw : constant Unsigned_8 := Card.Phycap (Thermal_Trim_Address (P) - Phycap_First);
               --  __thm_setting: bit 0 to bit 3, bits 4:1 to 2:0.
               Setting : constant Unsigned_32 :=
                 Shift_Left (Unsigned_32 (Raw and 1), 3) or Shift_Right (Unsigned_32 (Raw and 16#1F#), 1);
            begin
               RF_Write (P, RR_TM2, RR_TM2_OFF, Setting);
            end;
         end loop;
      end if;
      if PA_PG then
         for P in RF_Path loop
            declare
               Raw : constant Unsigned_8 := Card.Phycap (PA_Bias_Address (P) - Phycap_First);
            begin
               RF_Write (P, RR_BIASA, RR_BIASA_TXG, Unsigned_32 (Raw and Low_Nibble_Mask));
               RF_Write (P, RR_BIASA, RR_BIASA_TXA, Unsigned_32 (Shift_Right (Raw, 4)));
            end;
         end loop;
      end if;
   end Power_Trim;

   procedure Write_Reference is
      Value : constant Unsigned_32 := TP.Reference (0);
   begin
      BB_Mask (TP.Reference_Path_A + TP.Reference_OFDM, TP.BB_Reference_Mask, Value);
      BB_Mask (TP.Reference_Path_B + TP.Reference_OFDM, TP.BB_Reference_Mask, Value);
      BB_Mask (TP.Reference_Path_A + TP.Reference_CCK, TP.BB_Reference_Mask, Value);
      BB_Mask (TP.Reference_Path_B + TP.Reference_CCK, TP.BB_Reference_Mask, Value);
   end Write_Reference;

   procedure TX_Power_Control is
   begin
      Field32 (Register (TP.PWR_RATE_CTRL), TP.PWR_REF_MASK, Shift_Of (TP.PWR_REF_MASK), 0);
      Write_Reference;
   end TX_Power_Control;

   procedure TX_Power (Band : WS.Band; Channel : WS.Channel_Number) is
      Rates : constant TP.Byrate_Array := TP.Byrate (Band);
   begin
      if not TP.Supported (Band, Channel) then
         return;
      end if;
      for I in Rates'Range loop
         Write32 (Register (TP.PWR_BY_RATE + 4 * I), Rates (I));
      end loop;
      Field32 (Register (TP.PWR_RATE_OFST_CTRL), TP.PWR_OFFSET_MASK, 0, TP.Offset (Band));
      if Band = WS.Band_2_4_GHz then
         declare
            DFIR : constant TP.DFIR_Array := (if Channel = 14 then TP.DFIR_Sharp_14 else TP.DFIR_Flat);
         begin
            for I in DFIR'Range loop
               BB_Write (BB_Address (TP.TXFIR0 + 4 * I), DFIR (I));
            end loop;
         end;
      end if;
      BB_Mask (TP.DCFO_OPT, TP.TXSHAPE_TRIANGULAR_MASK, TP.Triangular_Worldwide);
      declare
         L : constant TP.Limit_Array := TP.Limits (Band, Channel);
      begin
         for I in L'Range loop
            Write32 (Register (TP.PWR_LMT + 4 * I), L (I));
         end loop;
      end;
      for I in 0 .. TP.Paths * TP.RU_Limit_Words_Per_Path - 1 loop
         Write32 (Register (TP.PWR_RU_LMT + 4 * I), 0);
      end loop;
      Write_Reference;   --  set_txpwr_diff with no antenna gain or SAR offset
   end TX_Power;

   procedure Set_Channel (Card : Board; Band : WS.Band; Channel : WS.Channel_Number;
                          Locked : out Boolean; Result : out Outcome) is
      Two_G : constant Boolean := Band = WS.Band_2_4_GHz;
      GB : constant Gain_Band := Gain_Band_Of (Band, Channel);
      RPL : constant RPL_Values := (if Two_G then RPL_2G else RPL_5G);
      RX_Path_0 : Unsigned_32;
   begin
      Locked := False;
      if not Supported (Band, Channel) then
         Result := Failure (Synthesizer_Unlocked, 0, Unsigned_32 (Channel));
         return;
      end if;
      Result := Success;
      --  Enter (set_channel_help): PPDU status off, TSSI held, ADC FIFO
      --  reset, BB held in reset.
      Clear32 (MR.PPDU_STAT, 1);
      TSSI_Hold (True);
      BB_Mask (R_ADC_FIFO, ADC_FIFO_RST, ADC_FIFO_RESET_VALUE);
      Delay_For (ADC_FIFO_Settle);
      BB_Mask (R_RXCCA, RXCCA_DIS, 1);
      BB_Mask (R_PD_CTRL, PD_HIT_DIS, 1);
      BB_Mask (R_S0_HW_SI_DIS, HW_SI_TRIG, HW_SI_DISABLE);
      BB_Mask (R_S1_HW_SI_DIS, HW_SI_TRIG, HW_SI_DISABLE);
      Delay_For (1);
      BB_Mask (R_RSTB_ASYNC, RSTB_ASYNC_ALL, 0);
      --  MAC: 20 MHz, band mode.
      Clear8 (WMAC_RFMOD, RFMOD_MASK);
      Write32 (MR.TX_SUB_CARRIER_VALUE, 0);
      if Two_G then
         Set8 (TXRATE_CHK, BAND_MODE);
         Clear8 (TXRATE_CHK, CHECK_CCK_RTS_OFDM6);
      else
         Clear8 (TXRATE_CHK, BAND_MODE);
         Set8 (TXRATE_CHK, CHECK_CCK_RTS_OFDM6);
      end if;
      --  BB.
      if Two_G then
         BB_Mask (R_RXSCOBC, SCO_THRESHOLD_MASK, Barker_Thresholds (Channel));
         BB_Mask (R_RXSCOCCK, SCO_THRESHOLD_MASK, CCK_Thresholds (Channel));
      end if;
      BB_Mask (R_PATH0_BAND_SEL_V1, BAND_SEL_2G, (if Two_G then 1 else 0));
      BB_Mask (R_PATH1_BAND_SEL_V1, BAND_SEL_2G, (if Two_G then 1 else 0));
      BB_Mask (R_FC0_BW_V1, FC0_BW_INV, SCO (Band, Channel));
      for P in Path loop
         for I in Gain_Places'Range loop
            BB_Mask (Gain_Base (Two_G, P) (Gain_Places (I).Register_Index),
                     Shift_Left (Byte_Field, Gain_Places (I).Byte_Shift),
                     Unsigned_32 (Gain_Errors (GB, P) (I)));
         end loop;
      end loop;
      Set_Gain_Offset (Card, GB);
      BB_Mask (R_P0_RPL1, RPL1_FIELD_MASK, RPL.RPL1_Field);
      BB_Mask (R_P1_RPL1, RPL1_FIELD_MASK, RPL.RPL1_Field);
      BB_Write (R_P0_RPL2, RPL.RPL2);
      BB_Write (R_P1_RPL2, RPL.RPL2);
      BB_Write (R_P0_RPL3, RPL.RPL3);
      BB_Write (R_P1_RPL3, RPL.RPL3);
      --  Bandwidth 20 MHz, CCK.
      RX_Path_0 := BB_Read (R_CHBW_MOD_V1, ANT_RX_SEG0);
      BB_Mask (R_FC0_BW_V1, FC0_BW_SET, 0);
      BB_Mask (R_CHBW_MOD_V1, CHBW_MOD_SBW, 0);
      BB_Mask (R_CHBW_MOD_V1, CHBW_MOD_PRICH, 0);
      BB_Mask (R_P0_RFMODE, RFMODE_ORI_RX, RFMODE_ORI_RX_AB);
      BB_Mask (R_P1_RFMODE, RFMODE_ORI_RX, RFMODE_ORI_RX_AB);
      BB_Mask (R_P0_CFCH_BW0, CFCH_BW0_MASK, 0);
      BB_Mask (R_P0_CFCH_BW1, CFCH_BW1_MASK, CFCH_BW1_20M);
      BB_Mask (R_P1_CFCH_BW0, CFCH_BW0_MASK, 0);
      BB_Mask (R_P1_CFCH_BW1, CFCH_BW1_MASK, CFCH_BW1_20M);
      if RX_Path_0 = RX_PATH_ONLY_A then
         BB_Mask (R_P1_RFMODE, RFMODE_ORI_RX, RFMODE_ORI_RX_ONE);
      elsif RX_Path_0 = RX_PATH_ONLY_B then
         BB_Mask (R_P0_RFMODE, RFMODE_ORI_RX, RFMODE_ORI_RX_ONE);
      end if;
      BB_Mask (R_UPD_CLK_ADC, ENABLE_CCK, (if Two_G then 1 else 0));
      BB_Mask (R_RXCCA, RXCCA_DIS, (if Two_G then 0 else 1));
      if Two_G then
         BTG_RX (True);
      else
         for S of BT_SHARE_CLEAR_5G loop
            BB_Mask (S.Where, S.Mask, S.Off);
         end loop;
      end if;
      BB_Mask (R_MAC_PIN_SEL, CH_IDX_SEG0, Channel_Index (Band, Channel));
      for S of FIVE_MHZ_MASKS loop
         BB_Mask (S.Where, S.Mask, S.Off);
      end loop;
      Reset_All;
      --  RF.
      RF_Channel (Band, Channel);
      --  Exit: PPDU status on, ADC FIFO released, TSSI released, BB out of reset.
      Write32 (MR.PPDU_STAT, MR.PPDU_STAT_ENABLES);
      Field32 (MR.HW_RPT_FWD, MR.FWD_PPDU_STAT_MASK, 0, MR.PRPT_DEST_HOST);
      BB_Mask (R_ADC_FIFO, ADC_FIFO_RST, 0);
      TSSI_Hold (False);
      BB_Mask (R_S0_HW_SI_DIS, HW_SI_TRIG, 0);
      BB_Mask (R_S1_HW_SI_DIS, HW_SI_TRIG, 0);
      BB_Mask (R_RSTB_ASYNC, RSTB_ASYNC_ALL, 1);
      if Two_G then
         BB_Mask (R_RXCCA, RXCCA_DIS, 0);
      end if;
      BB_Mask (R_PD_CTRL, PD_HIT_DIS, 0);
      TX_Power (Band, Channel);
      Locked := Synthesizer_Locked;
      if not Locked then
         Result := Failure (Synthesizer_Unlocked, Register (BB_Base + RF_Direct_Base (A)),
                            RF_Read (A, RR_CFGCH, RF_Mask));
      end if;
   end Set_Channel;

end RTW89_PHY;
