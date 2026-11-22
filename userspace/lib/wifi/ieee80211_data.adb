------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body IEEE80211_Data with SPARK_Mode is

   Byte_Mask : constant := 16#FF#;

   function Address_At (F : Octets; At_Byte : Octet_Index) return Address is
     [for I in Address'Range => F (At_Byte + I)]
   with Pre => F'First = 0 and then At_Byte + 6 <= F'Length;

   procedure Put_Address (F : in out Octets; At_Byte : Octet_Index; A : Address)
   with Pre => F'First = 0 and then At_Byte + 6 <= F'Length
   is
   begin
      for I in Address'Range loop
         F (At_Byte + I) := A (I);
      end loop;
   end Put_Address;

   function PN_Byte (PN : Packet_Number; Index : Natural) return Unsigned_8 is
     (Unsigned_8 (Shift_Right (Unsigned_64 (PN), 8 * Index) and Byte_Mask))
   with Pre => Index <= 5;

   procedure Build_Header
     (BSSID, Own, Destination : Address; Ether_Type : Unsigned_16; Sequence : Sequence_Number;
      P : Protection; Frame : in out Octets)
   is
      Sequence_Control : constant Unsigned_16 := Shift_Left (Unsigned_16 (Sequence), Sequence_Shift);
      SNAP_At : constant Natural := Header_Bytes + (if P.Encrypted then CCMP_Header_Bytes else 0);
   begin
      Frame (0) := FC_Data;
      Frame (1) := Flag_To_DS or (if P.Encrypted then Flag_Protected else 0);
      Frame (2) := 0;
      Frame (3) := 0;
      Put_Address (Frame, Address_1_At, BSSID);
      Put_Address (Frame, Address_2_At, Own);
      Put_Address (Frame, Address_3_At, Destination);
      Frame (Sequence_At) := Unsigned_8 (Sequence_Control and Byte_Mask);
      Frame (Sequence_At + 1) := Unsigned_8 (Shift_Right (Sequence_Control, 8));
      if P.Encrypted then
         Frame (Header_Bytes) := PN_Byte (P.PN, 0);
         Frame (Header_Bytes + 1) := PN_Byte (P.PN, 1);
         Frame (Header_Bytes + 2) := 0;
         Frame (Header_Bytes + 3) := ExtIV_Bit or Shift_Left (Unsigned_8 (P.Key), Key_ID_Shift);
         for I in 2 .. 5 loop
            Frame (Header_Bytes + 2 + I) := PN_Byte (P.PN, I);
         end loop;
      end if;
      for I in RFC_1042_Prefix'Range loop
         Frame (SNAP_At + I) := RFC_1042_Prefix (I);
      end loop;
      Frame (SNAP_At + 6) := Unsigned_8 (Shift_Right (Ether_Type, 8));
      Frame (SNAP_At + 7) := Unsigned_8 (Ether_Type and Byte_Mask);
   end Build_Header;

   procedure Decode (Frame : Octets; BSSID, Own : Address; Protected_Link : Boolean;
                     Item : out Received)
   is
      Control, Flags : Unsigned_8;
      At_Byte : Natural;
      Tail : Natural := 0;
      To : Address;
   begin
      Item := (others => <>);
      if Frame'Length < Header_Bytes + SNAP_Bytes then
         return;
      end if;
      Control := Frame (0);
      Flags := Frame (1);
      if (Control and Type_Mask) /= Type_Data then
         Item.Why := Not_Data;
         return;
      elsif Control /= FC_Data and then Control /= FC_QoS_Data then
         --  Null data, CF variants and QoS null carry no MSDU.
         Item.Why := Null_Data;
         return;
      elsif (Flags and (Flag_To_DS or Flag_From_DS)) /= Flag_From_DS then
         Item.Why := Wrong_Direction;
         return;
      elsif Address_At (Frame, Address_2_At) /= BSSID then
         Item.Why := Not_From_BSS;
         return;
      end if;
      To := Address_At (Frame, Address_1_At);
      Item.Group := (To (0) and Group_Bit) /= 0;
      if not Item.Group and then To /= Own then
         Item.Why := Not_For_Us;
         return;
      end if;
      At_Byte := Header_Bytes;
      if Control = FC_QoS_Data then
         Item.QoS := True;
         if (Frame (At_Byte) and QoS_AMSDU_Bit) /= 0 then
            Item.Why := Aggregate;
            return;
         end if;
         Item.Traffic := TID (Frame (At_Byte) and QoS_TID_Mask);
         At_Byte := At_Byte + QoS_Control_Bytes
           + (if (Flags and Flag_Order) /= 0 then HT_Control_Bytes else 0);
      end if;
      if (Flags and Flag_Protected) /= 0 then
         if Frame'Length - At_Byte < CCMP_Header_Bytes + SNAP_Bytes + CCMP_MIC_Bytes
           or else (Frame (At_Byte + 3) and ExtIV_Bit) = 0
         then
            Item.Why := Bad_CCMP_Header;
            return;
         end if;
         Item.Encrypted := True;
         Item.Key := Key_ID (Shift_Right (Frame (At_Byte + 3), Key_ID_Shift));
         Item.PN := Packet_Number (Frame (At_Byte))
           + Packet_Number (Frame (At_Byte + 1)) * 2 ** 8
           + Packet_Number (Frame (At_Byte + 4)) * 2 ** 16
           + Packet_Number (Frame (At_Byte + 5)) * 2 ** 24
           + Packet_Number (Frame (At_Byte + 6)) * 2 ** 32
           + Packet_Number (Frame (At_Byte + 7)) * 2 ** 40;
         At_Byte := At_Byte + CCMP_Header_Bytes;
         Tail := CCMP_MIC_Bytes;
      end if;
      if Frame'Length - At_Byte < SNAP_Bytes + Tail then
         Item.Why := Too_Short;
         return;
      end if;
      if (for some I in RFC_1042_Prefix'Range => Frame (At_Byte + I) /= RFC_1042_Prefix (I))
        and then (for some I in Bridge_Tunnel_Prefix'Range =>
                    Frame (At_Byte + I) /= Bridge_Tunnel_Prefix (I))
      then
         Item.Why := Not_SNAP;
         return;
      end if;
      Item.Ether_Type := Shift_Left (Unsigned_16 (Frame (At_Byte + 6)), 8)
        or Unsigned_16 (Frame (At_Byte + 7));
      --  On a protected link only EAPOL may arrive in the clear (the
      --  4-way rekey runs over the installed key, but the first exchange
      --  and an AP that restarts it do not).
      if Protected_Link and then not Item.Encrypted and then Item.Ether_Type /= EtherType_EAPOL then
         Item.Why := Unprotected_On_Protected_Link;
         return;
      end if;
      Item.Destination := To;
      Item.Source := Address_At (Frame, Address_3_At);
      Item.Payload_First := At_Byte + SNAP_Bytes;
      Item.Payload_Length := Frame'Length - Item.Payload_First - Tail;
      Item.Why := None;
   end Decode;

end IEEE80211_Data;
