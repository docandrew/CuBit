------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body RTW89_H2C with SPARK_Mode is

   subtype Word_Index is Natural range 0 .. 16;
   type Words is array (Word_Index range <>) of Unsigned_32;

   function To_Bytes (W : Words) return Bytes
   with Pre  => W'First = 0 and then W'Length <= 17,
        Post => To_Bytes'Result'First = 1 and then To_Bytes'Result'Length = 4 * W'Length
   is
      Result : Bytes (1 .. 4 * W'Length) := [others => 0];
   begin
      for I in W'Range loop
         Result (4 * I + 1 .. 4 * I + 4) := Word_Bytes (W (I));
      end loop;
      return Result;
   end To_Bytes;

   function Pack (B0, B1, B2, B3 : Unsigned_8) return Unsigned_32 is
     (Unsigned_32 (B0) or Shift_Left (Unsigned_32 (B1), 8) or
      Shift_Left (Unsigned_32 (B2), 16) or Shift_Left (Unsigned_32 (B3), 24));

   --  Address CAM fields (cam.h, v0).
   CAM_Entry_Index   : constant := 0;
   CAM_Entry_Length  : constant := 16#40#;         --  ADDR_CAM_ENT_SIZE
   CAM_Length_Shift  : constant := 16;
   CAM_Valid         : constant Unsigned_32 := 2 ** 0;
   CAM_SMA_Hash_Shift : constant := 16;
   Security_Normal   : constant Unsigned_32 := 2;   --  RTW89_ADDR_CAM_SEC_NORMAL
   Security_Mode_Shift : constant := 16;
   BSSID_CAM_Length  : constant Unsigned_32 := 8;
   BSSID_Length_Shift : constant := 16;
   BSSID_Valid       : constant Unsigned_32 := 2 ** 0;
   BSSID_Mask_All    : constant Unsigned_32 := 16#3F#;  --  BSSID_MATCH_ALL
   BSSID_Mask_Shift  : constant := 2;

   function Address_CAM (Own : Address) return Bytes is
      Hash : constant Unsigned_8 := Own (1) xor Own (2) xor Own (3) xor Own (4) xor Own (5) xor Own (6);
      W : constant Words (0 .. 14) :=
        [1  => Unsigned_32'(CAM_Entry_Index) or Shift_Left (Unsigned_32'(CAM_Entry_Length), CAM_Length_Shift),
         2  => CAM_Valid or Shift_Left (Unsigned_32 (Hash), CAM_SMA_Hash_Shift),
         4  => Pack (Own (1), Own (2), Own (3), Own (4)),
         5  => Pack (Own (5), Own (6), 0, 0),
         9  => Shift_Left (Security_Normal, Security_Mode_Shift),
         12 => Shift_Left (BSSID_CAM_Length, BSSID_Length_Shift),
         13 => BSSID_Valid or Shift_Left (BSSID_Mask_All, BSSID_Mask_Shift),
         others => 0];
   begin
      return To_Bytes (W);
   end Address_CAM;

   --  CMAC table fields (fw.h FCTRL / CCTRL).
   CMAC_Operation   : constant Unsigned_32 := 2 ** 7;
   NTX_Path_AB      : constant Unsigned_32 := 3;
   NTX_Path_Shift   : constant := 16;
   Path_Map_B_One   : constant Unsigned_32 := 1;
   Path_Map_B_Shift : constant := 22;
   Mask_MGQ_Report  : constant Unsigned_32 := 2 ** 21;
   Mask_TX_Power_Mode : constant Unsigned_32 := 16#0000_0E00#;
   Mask_Paths_Antennas : constant Unsigned_32 := 16#FFFF_0000#;
   Mask_Doppler_Tolerance : constant Unsigned_32 := 16#0F0C_0000#;

   function Default_CMAC_Table return Bytes is
      W : constant Words (0 .. 16) :=
        [0  => CMAC_Operation or Unsigned_32'(Own_MACID),
         6  => Shift_Left (NTX_Path_AB, NTX_Path_Shift) or Shift_Left (Path_Map_B_One, Path_Map_B_Shift),
         9  => Mask_MGQ_Report,
         13 => Mask_TX_Power_Mode,
         14 => Mask_Paths_Antennas,
         15 => Mask_Doppler_Tolerance,
         others => 0];
   begin
      return To_Bytes (W);
   end Default_CMAC_Table;

end RTW89_H2C;
