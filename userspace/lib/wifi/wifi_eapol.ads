------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  EAPOL-Key frames (IEEE 802.1X-2020 11.3, IEEE 802.11-2020 12.7.2) for
--  the WPA2-PSK 4-way and group-key handshakes (NET-WIFI-001 W4): the
--  decoder for what the AP sends and the builders for messages 2 and 4.
--
--  @description
--  The frame, from the EAPOL header: Protocol Version (1), Packet Type (1,
--  3 = Key), Packet Body Length (2, big endian); the RSN Key Descriptor:
--  Descriptor Type (1, 2 = RSN), Key Information (2), Key Length (2), Key
--  Replay Counter (8), Key Nonce (32), EAPOL-Key IV (16), Key RSC (8),
--  reserved (8), Key MIC (16 for AKM 2), Key Data Length (2), Key Data.
--  All multi-octet fields are big endian. The decoder is total and proved
--  free of run-time errors: a body length or key data length past the
--  frame refuses it, and the key data always lies inside the frame.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Wifi_EAPOL with Pure, SPARK_Mode is

   Maximum_Frame : constant := 2048;
   subtype Frame_Index is Natural range 0 .. Maximum_Frame - 1;
   subtype Octet_Index is Natural range 0 .. Maximum_Frame;
   type Octets is array (Octet_Index range <>) of Unsigned_8;

   --  Offsets from the EAPOL header's first octet.
   Version_At        : constant := 0;
   Packet_Type_At    : constant := 1;
   Body_Length_At    : constant := 2;
   Descriptor_At     : constant := 4;
   Key_Information_At : constant := 5;
   Key_Length_At     : constant := 7;
   Replay_At         : constant := 9;
   Nonce_At          : constant := 17;
   IV_At             : constant := 49;
   RSC_At            : constant := 65;
   MIC_At            : constant := 81;
   Key_Data_Length_At : constant := 97;
   Key_Data_At       : constant := 99;
   Header_Bytes      : constant := 4;
   Replay_Bytes      : constant := 8;
   Nonce_Bytes       : constant := 32;
   RSC_Bytes         : constant := 8;
   MIC_Bytes         : constant := 16;

   Packet_Type_Key   : constant Unsigned_8 := 3;
   Descriptor_RSN    : constant Unsigned_8 := 2;
   EAPOL_Version_2004 : constant Unsigned_8 := 2;

   --  Key Information bits (12.7.2, Figure 12-33).
   Descriptor_Version_Mask : constant Unsigned_16 := 16#0007#;
   Version_HMAC_SHA1_AES   : constant Unsigned_16 := 2;
   Key_Type_Pairwise       : constant Unsigned_16 := 2 ** 3;
   Install_Bit             : constant Unsigned_16 := 2 ** 6;
   Key_Ack_Bit             : constant Unsigned_16 := 2 ** 7;
   Key_MIC_Bit             : constant Unsigned_16 := 2 ** 8;
   Secure_Bit              : constant Unsigned_16 := 2 ** 9;
   Error_Bit               : constant Unsigned_16 := 2 ** 10;
   Request_Bit             : constant Unsigned_16 := 2 ** 11;
   Encrypted_Data_Bit      : constant Unsigned_16 := 2 ** 12;
   CCMP_Key_Length         : constant := 16;

   type Replay_Counter is mod 2 ** 64;
   type Nonce is array (0 .. Nonce_Bytes - 1) of Unsigned_8;
   type MIC_Value is array (0 .. MIC_Bytes - 1) of Unsigned_8;
   type RSC_Value is array (0 .. RSC_Bytes - 1) of Unsigned_8;

   type Message_Kind is (Pairwise_1, Pairwise_3, Group_1, Other);

   type Key_Frame is record
      Information : Unsigned_16 := 0;
      Key_Length  : Unsigned_16 := 0;
      Replay      : Replay_Counter := 0;
      Key_Nonce   : Nonce := [others => 0];
      RSC         : RSC_Value := [others => 0];
      MIC         : MIC_Value := [others => 0];
      Data_First  : Octet_Index := Key_Data_At;
      Data_Length : Octet_Index := 0;
      Frame_Length : Octet_Index := 0;     --  header + body (octets the MIC covers)
      Kind        : Message_Kind := Other;
   end record;

   type Status is
     (Decoded, Too_Short, Not_Key, Not_RSN_Descriptor, Body_Overrun, Key_Data_Overrun,
      Wrong_Descriptor_Version);

   function Valid (K : Key_Frame) return Boolean is
     (K.Data_First = Key_Data_At
      and then K.Data_First + K.Data_Length <= K.Frame_Length);

   procedure Decode (Frame : Octets; Item : out Key_Frame; Result : out Status)
   with Pre  => Frame'First = 0 and then Frame'Length <= Maximum_Frame,
        Post => (if Result = Decoded then Valid (Item) and then Item.Frame_Length <= Frame'Length);

   --  Message 2 (pairwise, MIC set) and message 4 (pairwise, MIC, secure),
   --  MIC field zero for the caller to compute over Frame (0 .. Length - 1).
   --  Message 2 carries Key_Data (CuBit's RSN element).
   Maximum_Key_Data : constant := 256;
   procedure Build_Message_2
     (Replay : Replay_Counter; S_Nonce : Nonce; Key_Data : Octets;
      Frame : out Octets; Length : out Natural)
   with Pre  => Key_Data'First = 0 and then Key_Data'Length <= Maximum_Key_Data
                and then Frame'First = 0 and then Frame'Length >= Key_Data_At + Maximum_Key_Data,
        Post => Length = Key_Data_At + Key_Data'Length;

   procedure Build_Message_4
     (Replay : Replay_Counter; Frame : out Octets; Length : out Natural)
   with Pre  => Frame'First = 0 and then Frame'Length >= Key_Data_At,
        Post => Length = Key_Data_At;

   --  Group-key handshake message 2 (MIC, secure, group key type).
   procedure Build_Group_2
     (Replay : Replay_Counter; Frame : out Octets; Length : out Natural)
   with Pre  => Frame'First = 0 and then Frame'Length >= Key_Data_At,
        Post => Length = Key_Data_At;

   --  Store a MIC into a built frame.
   procedure Put_MIC (Frame : in out Octets; MIC : MIC_Value)
   with Pre => Frame'First = 0 and then Frame'Length >= Key_Data_At;

   --  The frame with its MIC field zero (what the MIC is computed over).
   procedure Zero_MIC (Frame : in out Octets)
   with Pre => Frame'First = 0 and then Frame'Length >= Key_Data_At;

end Wifi_EAPOL;
