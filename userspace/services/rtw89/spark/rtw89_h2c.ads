------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The host-to-firmware commands rtw89.drv sends for a receive-only
--  station (NET-WIFI-001 W1): their category/class/function and payloads.
--
--  @description
--  Layouts from rtw89 fw.h / fw.c / cam.c (H2C_CAT_*, H2C_CL_*,
--  H2C_FUNC_*; rtw89_fw_h2c_set_ofld_cfg, rtw89_fw_h2c_fw_log,
--  rtw89_fw_h2c_role_maintain, rtw89_fw_h2c_join_info,
--  rtw89_cam_fill_addr_cam_info (v0), rtw89_fw_h2c_default_cmac_tbl,
--  rtw89_fw_h2c_macid_pause), GPL-2.0 OR BSD-3-Clause, taken under
--  BSD-3-Clause; values for macid 0 on port 0, a client that is not
--  associated. Payloads are little-endian byte arrays. Pure functions,
--  proved free of run-time errors; the hosted tests compare each payload
--  with the bytes worked out from rtw89 (seq-rx.md 4.6, seq-macinit.md F).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package RTW89_H2C with Pure, SPARK_Mode is

   type Bytes is array (Positive range <>) of Unsigned_8;
   subtype Address is Bytes (1 .. 6);

   --  Categories, classes and functions.
   Category_MAC        : constant Unsigned_8 := 1;    --  H2C_CAT_MAC
   Class_FW_Info       : constant Unsigned_8 := 0;    --  H2C_CL_FW_INFO
   Function_Log_Config : constant Unsigned_8 := 0;    --  H2C_FUNC_LOG_CFG
   Class_Frame_Exchange : constant Unsigned_8 := 5;   --  H2C_CL_MAC_FR_EXCHG
   Function_CMAC_Table : constant Unsigned_8 := 2;    --  H2C_FUNC_MAC_CCTLINFO_UD
   Class_Address_CAM   : constant Unsigned_8 := 6;    --  H2C_CL_MAC_ADDR_CAM_UPDATE
   Function_Address_CAM : constant Unsigned_8 := 0;   --  H2C_FUNC_MAC_ADDR_CAM_UPD
   Class_Media_Report  : constant Unsigned_8 := 8;    --  H2C_CL_MAC_MEDIA_RPT
   Function_Join_Info  : constant Unsigned_8 := 0;    --  H2C_FUNC_MAC_JOININFO
   Function_Role_Maintain : constant Unsigned_8 := 4; --  H2C_FUNC_MAC_FWROLE_MAINTAIN
   Class_FW_Offload    : constant Unsigned_8 := 9;    --  H2C_CL_MAC_FW_OFLD
   Function_MACID_Pause : constant Unsigned_8 := 8;   --  H2C_FUNC_MAC_MACID_PAUSE
   Function_Offload_Config : constant Unsigned_8 := 16#14#;  --  H2C_FUNC_OFLD_CFG

   Own_MACID : constant := 0;
   Own_Port  : constant := 0;

   --  rtw89_fw_h2c_set_ofld_cfg: an opaque literal in rtw89 (fw.c).
   Offload_Config : constant Bytes (1 .. 8) := [16#09#, 0, 0, 0, 16#5E#, 0, 0, 0];

   --  rtw89_fw_h2c_fw_log (disabled): level LOUD (4) [7:0], path C2H
   --  (bit 1) [15:8], no components.
   Log_Level_Loud : constant Unsigned_8 := 4;
   Log_Path_C2H   : constant Unsigned_8 := 2 ** 1;
   Log_Config_Off : constant Bytes (1 .. 12) := [Log_Level_Loud, Log_Path_C2H, others => 0];

   --  Role and join words (fw.h H2C_ROLE_MAINTAIN_*, H2C_JOININFO_*).
   Self_Role_Client   : constant := 0;
   Update_Mode_Create : constant := 0;
   Wifi_Role_Station  : constant := 1;
   Net_Type_No_Link   : constant := 0;
   Role_Wifi_Role_Shift : constant := 13;       --  [16:13]
   Join_Disconnect_Bit  : constant := 2 ** 8;   --  OP: dis_conn
   Join_Net_Type_Shift  : constant := 24;       --  [25:24]
   Join_Wifi_Role_Shift : constant := 26;       --  [29:26]

   function Word_Bytes (W : Unsigned_32) return Bytes is
     [Unsigned_8 (W and 16#FF#), Unsigned_8 (Shift_Right (W, 8) and 16#FF#),
      Unsigned_8 (Shift_Right (W, 16) and 16#FF#), Unsigned_8 (Shift_Right (W, 24))]
   with Post => Word_Bytes'Result'First = 1 and then Word_Bytes'Result'Length = 4;

   function Role_Maintain_Create return Bytes is
     (Word_Bytes (Unsigned_32 (Own_MACID)
                 or Shift_Left (Unsigned_32'(Wifi_Role_Station), Role_Wifi_Role_Shift)));

   function Join_Info_Disconnected return Bytes is
     (Word_Bytes (Unsigned_32 (Own_MACID) or Join_Disconnect_Bit
                 or Shift_Left (Unsigned_32'(Net_Type_No_Link), Join_Net_Type_Shift)
                 or Shift_Left (Unsigned_32'(Wifi_Role_Station), Join_Wifi_Role_Shift)));

   --  The address CAM entry (v0, 15 words) for our own address with no
   --  peer: entry 0, 0x40 bytes, valid; BSSID entry 0 (8 bytes, mask
   --  "match all"), security entries in normal mode.
   Address_CAM_Bytes : constant := 60;
   function Address_CAM (Own : Address) return Bytes
   with Post => Address_CAM'Result'First = 1 and then Address_CAM'Result'Length = Address_CAM_Bytes;

   --  The default CMAC table for macid 0 (17 words: values 0 .. 7, write
   --  masks 8 .. 15, word 16). Two TX paths (A and B).
   CMAC_Table_Bytes : constant := 68;
   function Default_CMAC_Table return Bytes
   with Post => Default_CMAC_Table'Result'First = 1
                and then Default_CMAC_Table'Result'Length = CMAC_Table_Bytes;

   --  Unpause macid 0: pause groups 0 .. 3 all zero, mask group 0 bit 0.
   MACID_Pause_Bytes : constant := 32;
   MACID_Unpause_Own : constant Bytes (1 .. MACID_Pause_Bytes) :=
     [17 => 2 ** Own_MACID, others => 0];

end RTW89_H2C;
