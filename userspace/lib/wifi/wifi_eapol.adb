------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Wifi_EAPOL with SPARK_Mode is

   function BE16 (F : Octets; At_Byte : Octet_Index) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (F (At_Byte)), 8) or Unsigned_16 (F (At_Byte + 1)))
   with Pre => F'First = 0 and then At_Byte + 2 <= F'Length;

   procedure Put_BE16 (F : in out Octets; At_Byte : Octet_Index; V : Unsigned_16)
   with Pre => F'First = 0 and then At_Byte + 2 <= F'Length
   is
   begin
      F (At_Byte) := Unsigned_8 (Shift_Right (V, 8));
      F (At_Byte + 1) := Unsigned_8 (V and 16#FF#);
   end Put_BE16;

   function Classify (Info : Unsigned_16) return Message_Kind is
     (if (Info and Key_Type_Pairwise) /= 0 and then (Info and Key_Ack_Bit) /= 0
         and then (Info and Key_MIC_Bit) = 0 and then (Info and Install_Bit) = 0
      then Pairwise_1
      elsif (Info and Key_Type_Pairwise) /= 0 and then (Info and Key_Ack_Bit) /= 0
         and then (Info and Key_MIC_Bit) /= 0 and then (Info and Install_Bit) /= 0
      then Pairwise_3
      elsif (Info and Key_Type_Pairwise) = 0 and then (Info and Key_Ack_Bit) /= 0
         and then (Info and Key_MIC_Bit) /= 0
      then Group_1
      else Other);

   procedure Decode (Frame : Octets; Item : out Key_Frame; Result : out Status) is
      Body_Length : Natural;
      Data_Length : Natural;
   begin
      Item := (others => <>);
      if Frame'Length < Key_Data_At then
         Result := Too_Short;
         return;
      elsif Frame (Packet_Type_At) /= Packet_Type_Key then
         Result := Not_Key;
         return;
      elsif Frame (Descriptor_At) /= Descriptor_RSN then
         Result := Not_RSN_Descriptor;
         return;
      end if;
      Body_Length := Natural (BE16 (Frame, Body_Length_At));
      if Body_Length > Frame'Length - Header_Bytes or else Body_Length < Key_Data_At - Header_Bytes then
         Result := Body_Overrun;
         return;
      end if;
      Item.Frame_Length := Header_Bytes + Body_Length;
      Item.Information := BE16 (Frame, Key_Information_At);
      if (Item.Information and Descriptor_Version_Mask) /= Version_HMAC_SHA1_AES then
         Result := Wrong_Descriptor_Version;
         return;
      end if;
      Data_Length := Natural (BE16 (Frame, Key_Data_Length_At));
      if Data_Length > Item.Frame_Length - Key_Data_At then
         Result := Key_Data_Overrun;
         return;
      end if;
      Item.Data_Length := Data_Length;
      Item.Key_Length := BE16 (Frame, Key_Length_At);
      for I in 0 .. Replay_Bytes - 1 loop
         Item.Replay := Item.Replay * 256 + Replay_Counter (Frame (Replay_At + I));
      end loop;
      Item.Key_Nonce := [for I in Nonce'Range => Frame (Nonce_At + I)];
      Item.RSC := [for I in RSC_Value'Range => Frame (RSC_At + I)];
      Item.MIC := [for I in MIC_Value'Range => Frame (MIC_At + I)];
      Item.Kind := Classify (Item.Information);
      Result := Decoded;
   end Decode;

   procedure Common
     (Replay : Replay_Counter; Information : Unsigned_16; Key_Data_Length : Natural;
      Frame : out Octets; Length : out Natural)
   with Pre  => Frame'First = 0 and then Key_Data_Length <= Maximum_Key_Data
                and then Frame'Length >= Key_Data_At + Key_Data_Length,
        Post => Length = Key_Data_At + Key_Data_Length
   is
      R : Replay_Counter := Replay;
   begin
      Frame := [others => 0];
      Length := Key_Data_At + Key_Data_Length;
      Frame (Version_At) := EAPOL_Version_2004;
      Frame (Packet_Type_At) := Packet_Type_Key;
      Put_BE16 (Frame, Body_Length_At, Unsigned_16 (Length - Header_Bytes));
      Frame (Descriptor_At) := Descriptor_RSN;
      Put_BE16 (Frame, Key_Information_At, Information);
      Put_BE16 (Frame, Key_Length_At, CCMP_Key_Length);
      for I in reverse 0 .. Replay_Bytes - 1 loop
         Frame (Replay_At + I) := Unsigned_8 (R mod 256);
         R := R / 256;
      end loop;
      Put_BE16 (Frame, Key_Data_Length_At, Unsigned_16 (Key_Data_Length));
   end Common;

   procedure Build_Message_2
     (Replay : Replay_Counter; S_Nonce : Nonce; Key_Data : Octets;
      Frame : out Octets; Length : out Natural) is
   begin
      Common (Replay, Version_HMAC_SHA1_AES or Key_Type_Pairwise or Key_MIC_Bit,
              Key_Data'Length, Frame, Length);
      for I in Nonce'Range loop
         Frame (Nonce_At + I) := S_Nonce (I);
      end loop;
      for I in Key_Data'Range loop
         Frame (Key_Data_At + I) := Key_Data (I);
      end loop;
   end Build_Message_2;

   procedure Build_Message_4
     (Replay : Replay_Counter; Frame : out Octets; Length : out Natural) is
   begin
      Common (Replay, Version_HMAC_SHA1_AES or Key_Type_Pairwise or Key_MIC_Bit or Secure_Bit,
              0, Frame, Length);
   end Build_Message_4;

   procedure Build_Group_2
     (Replay : Replay_Counter; Frame : out Octets; Length : out Natural) is
   begin
      Common (Replay, Version_HMAC_SHA1_AES or Key_MIC_Bit or Secure_Bit, 0, Frame, Length);
   end Build_Group_2;

   procedure Put_MIC (Frame : in out Octets; MIC : MIC_Value) is
   begin
      for I in MIC_Value'Range loop
         Frame (MIC_At + I) := MIC (I);
      end loop;
   end Put_MIC;

   procedure Zero_MIC (Frame : in out Octets) is
   begin
      for I in MIC_Value'Range loop
         Frame (MIC_At + I) := 0;
      end loop;
   end Zero_MIC;

end Wifi_EAPOL;
