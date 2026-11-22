------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

with Interfaces; use Interfaces;

package body Wifi_Station_Path with SPARK_Mode is


   --  Frame Control octet 0 (802.11-2020 9.2.4.1): type and subtype.
   Type_Mask         : constant Unsigned_8 := 16#0C#;
   Type_Management   : constant Unsigned_8 := 16#00#;
   Type_Data         : constant Unsigned_8 := 16#08#;
   Subtype_Mask      : constant Unsigned_8 := 16#F0#;
   Subtype_Assoc_Response   : constant Unsigned_8 := 16#10#;
   Subtype_Reassoc_Response : constant Unsigned_8 := 16#30#;
   Subtype_Probe_Response   : constant Unsigned_8 := 16#50#;
   Subtype_Beacon           : constant Unsigned_8 := 16#80#;
   Subtype_Disassociation   : constant Unsigned_8 := 16#A0#;
   Subtype_Authentication   : constant Unsigned_8 := 16#B0#;
   Subtype_Deauthentication : constant Unsigned_8 := 16#C0#;

   function Address_At (F : Octets; At_Byte : Natural) return Address is
     [for I in Address'Range => F (At_Byte + I)]
   with Pre => F'First = 0 and then At_Byte <= DT.Maximum_Octets and then At_Byte + 6 <= F'Length;

   procedure Classify (L : in out Link; Frame : Octets; Decrypted, Decrypt_Failed : Boolean;
                       V : out Verdict) is
      Control : Unsigned_8;
      Accepted : Boolean;
   begin
      V := (others => <>);
      if not L.Joined then
         return;
      elsif Frame'Length < DT.Header_Bytes then
         V.Why := Too_Short;
         return;
      end if;
      Control := Frame (0);
      if (Control and Type_Mask) = Type_Management then
         if Address_At (Frame, DT.Address_2_At) /= L.BSSID
           or else Address_At (Frame, DT.Address_3_At) /= L.BSSID
         then
            V.Why := Other_BSS;
            return;
         end if;
         case Control and Subtype_Mask is
            when Subtype_Beacon | Subtype_Probe_Response =>
               if L.Forward_Beacon then
                  L.Forward_Beacon := False;
                  V.Kind := To_Wifi_Management;
               else
                  V.Why := Beacon_Already_Forwarded;
               end if;
            when Subtype_Authentication | Subtype_Assoc_Response | Subtype_Reassoc_Response
               | Subtype_Deauthentication | Subtype_Disassociation =>
               V.Kind := To_Wifi_Management;
            when others =>
               V.Why := Other_Management;
         end case;
         return;
      elsif (Control and Type_Mask) /= Type_Data then
         V.Why := Other_Type;
         return;
      end if;

      DT.Decode (Frame, L.BSSID, L.Own, L.Pairwise_Installed, V.Data);
      if V.Data.Why /= DT.None then
         V.Why := Data_Refused;
         V.Refusal := V.Data.Why;
         return;
      elsif V.Data.Source = L.Own then
         V.Why := Own_Echo;
         return;
      end if;
      if V.Data.Encrypted then
         if not Decrypted or else Decrypt_Failed then
            V.Why := Not_Decrypted;
            return;
         end if;
         declare
            Class : constant Wifi_Replay.Traffic_Class :=
              (if V.Data.QoS then V.Data.Traffic else Wifi_Replay.Non_QoS);
         begin
            if V.Data.Group then
               if not L.Group_Installed (V.Data.Key) then
                  V.Why := Unknown_Group_Key;
                  return;
               end if;
               Wifi_Replay.Accept_Frame (L.Group_Replay (V.Data.Key), Class, V.Data.PN, Accepted);
            else
               Wifi_Replay.Accept_Frame (L.Pairwise_Replay, Class, V.Data.PN, Accepted);
            end if;
         end;
         if not Accepted then
            V.Why := Replayed;
            return;
         end if;
      end if;
      if V.Data.Ether_Type = DT.EtherType_EAPOL then
         V.Kind := To_Wifi_EAPOL;
      elsif L.Port_Open then
         V.Kind := To_Netstack;
      else
         V.Why := Port_Closed;
      end if;
   end Classify;

   function EAPOL_PDU (Frame : Octets; V : Verdict) return Octets is
     [for I in 0 .. V.Data.Payload_Length - 1 => Frame (V.Data.Payload_First + I)];

   function Ethernet (Frame : Octets; V : Verdict) return Octets is
      Byte_Mask : constant := 16#FF#;
      E : Octets (0 .. Ethernet_Header + V.Data.Payload_Length - 1) := [others => 0];
   begin
      for I in Address'Range loop
         E (I) := V.Data.Destination (I);
         E (6 + I) := V.Data.Source (I);
      end loop;
      E (12) := Unsigned_8 (Shift_Right (V.Data.Ether_Type, 8));
      E (13) := Unsigned_8 (V.Data.Ether_Type and Byte_Mask);
      for I in 0 .. V.Data.Payload_Length - 1 loop
         E (Ethernet_Header + I) := Frame (V.Data.Payload_First + I);
      end loop;
      return E;
   end Ethernet;

end Wifi_Station_Path;
