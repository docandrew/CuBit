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

   Net_Type_Shift    : constant := 1;               --  ADDR_CAM_W2_NET_TYPE [2:1]
   CAM_TMA_Hash_Shift : constant := 24;
   AID_Mask          : constant Unsigned_32 := 16#FFF#;
   Key_ID_Shift      : constant := 18;              --  SEC_ENT0_KEYID [19:18], 2 bits a slot
   Key_ID_Width      : constant := 2;
   Entry_Valid_Mask  : constant Unsigned_32 := 16#FF#;  --  W10 SEC_ENT_VALID [7:0]
   Entry_Shift       : constant := 8;               --  W10 SEC_ENT0 [15:8]
   Entry_Width       : constant := 8;
   BSSID_Byte_Shift  : constant := 16;              --  W13 BSSID0 [23:16]

   function Hash (A : Address) return Unsigned_8 is
     (A (1) xor A (2) xor A (3) xor A (4) xor A (5) xor A (6));

   function Station_CAM (S : Station) return Bytes is
      Own : Address renames S.Own;
      Peer : Address renames S.Peer;
      Valid_Bits : Unsigned_32 := 0;
      Key_IDs : Unsigned_32 := 0;
      Entries : array (Security_Slot) of Unsigned_32 := [others => 0];
   begin
      for Slot in Security_Slot loop
         if S.Keys (Slot).Valid then
            Valid_Bits := Valid_Bits or Shift_Left (1, Slot);
            Key_IDs := Key_IDs or Shift_Left (Unsigned_32 (S.Keys (Slot).Key), Key_ID_Shift + Key_ID_Width * Slot);
            Entries (Slot) := Unsigned_32 (S.Keys (Slot).Index);
         end if;
      end loop;
      declare
         W : constant Words (0 .. 14) :=
           [1  => Unsigned_32'(CAM_Entry_Index) or Shift_Left (Unsigned_32'(CAM_Entry_Length), CAM_Length_Shift),
            2  => CAM_Valid or Shift_Left (Unsigned_32 (Net_Type'Enum_Rep (S.Kind)), Net_Type_Shift)
                  or Shift_Left (Unsigned_32 (Hash (Own)), CAM_SMA_Hash_Shift)
                  or Shift_Left (Unsigned_32 (Hash (Peer)), CAM_TMA_Hash_Shift),
            4  => Pack (Own (1), Own (2), Own (3), Own (4)),
            5  => Pack (Own (5), Own (6), Peer (1), Peer (2)),
            6  => Pack (Peer (3), Peer (4), Peer (5), Peer (6)),
            9  => (if S.Kind = Infrastructure then Unsigned_32 (S.AID) and AID_Mask else 0)
                  or Shift_Left (Security_Normal, Security_Mode_Shift) or Key_IDs,
            10 => (Valid_Bits and Entry_Valid_Mask) or Shift_Left (Entries (0), Entry_Shift)
                  or Shift_Left (Entries (1), Entry_Shift + Entry_Width)
                  or Shift_Left (Entries (2), Entry_Shift + 2 * Entry_Width),
            11 => Entries (3) or Shift_Left (Entries (4), Entry_Width)
                  or Shift_Left (Entries (5), 2 * Entry_Width) or Shift_Left (Entries (6), 3 * Entry_Width),
            12 => Shift_Left (BSSID_CAM_Length, BSSID_Length_Shift),
            13 => BSSID_Valid or Shift_Left (BSSID_Mask_All, BSSID_Mask_Shift)
                  or Shift_Left (Unsigned_32 (Peer (1)), BSSID_Byte_Shift)
                  or Shift_Left (Unsigned_32 (Peer (2)), BSSID_Byte_Shift + 8),
            14 => Pack (Peer (3), Peer (4), Peer (5), Peer (6)),
            others => 0];
      begin
         return To_Bytes (W);
      end;
   end Station_CAM;

   Security_Length_Shift : constant := 16;

   function Security_CAM (Index : Security_CAM_Index; Key : Key_16) return Bytes is
      W : constant Words (0 .. 5) :=
        [0 => Unsigned_32 (Index) or Shift_Left (Unsigned_32'(Security_CAM_Length), Security_Length_Shift),
         1 => Key_Type_CCMP_128,
         2 => Pack (Key (1), Key (2), Key (3), Key (4)),
         3 => Pack (Key (5), Key (6), Key (7), Key (8)),
         4 => Pack (Key (9), Key (10), Key (11), Key (12)),
         5 => Pack (Key (13), Key (14), Key (15), Key (16))];
   begin
      return To_Bytes (W);
   end Security_CAM;

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
