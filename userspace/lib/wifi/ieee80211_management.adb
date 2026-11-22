------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body IEEE80211_Management with SPARK_Mode is

   AID_Mask : constant Unsigned_16 := 16#3FFF#;
   Broadcast : constant Address := [others => 16#FF#];

   procedure Put16 (F : in out Octets; At_Byte : Octet_Index; V : Unsigned_16)
   with Pre => F'First = 0 and then At_Byte + 2 <= F'Length
   is
   begin
      F (At_Byte) := Unsigned_8 (V and 16#FF#);
      F (At_Byte + 1) := Unsigned_8 (Shift_Right (V, 8));
   end Put16;

   function Get16 (F : Octets; At_Byte : Octet_Index) return Unsigned_16 is
     (Unsigned_16 (F (At_Byte)) or Shift_Left (Unsigned_16 (F (At_Byte + 1)), 8))
   with Pre => F'First = 0 and then At_Byte + 2 <= F'Length;

   function Address_At (F : Octets; At_Byte : Octet_Index) return Address is
     [for I in Address'Range => F (At_Byte + I)]
   with Pre => F'First = 0 and then At_Byte + 6 <= F'Length;

   procedure Header (F : in out Octets; Control : Unsigned_8; BSSID, Own : Address)
   with Pre => F'First = 0 and then F'Length >= Header_Bytes
   is
   begin
      F (0) := Control;
      F (Control_Flags_At) := 0;
      for I in Address'Range loop
         F (Address_1_At + I) := BSSID (I);
         F (Address_2_At + I) := Own (I);
         F (Address_3_At + I) := BSSID (I);
      end loop;
   end Header;

   procedure Build_Authentication (BSSID, Own : Address; Frame : out Octets; Length : out Natural) is
   begin
      Frame := [others => 0];
      Header (Frame, FC_Authentication, BSSID, Own);
      Put16 (Frame, Header_Bytes, Algorithm_Open_System);
      Put16 (Frame, Header_Bytes + 2, Authentication_Request_Sequence);
      Put16 (Frame, Header_Bytes + 4, Status_Success);
      Length := Authentication_Bytes;
   end Build_Authentication;

   procedure Build_Association_Request
     (BSSID, Own : Address; Capability : Unsigned_16;
      SSID : Octets; Rates : Octets; Extended_Rates : Octets; Extra : Octets;
      Frame : out Octets; Length : out Natural)
   is
      Position : Natural := Header_Bytes + 4;

      procedure Element (ID : Unsigned_8; Data : Octets)
      with Pre => Data'Length <= 255 and then Position <= Maximum_Frame
                  and then Frame'First = 0 and then Frame'Length = Maximum_Frame,
           Post => Position <= Maximum_Frame
      is
      begin
         if Maximum_Frame - Position < 2 + Data'Length then
            return;
         end if;
         Frame (Position) := ID;
         Frame (Position + 1) := Unsigned_8 (Data'Length);
         for I in 0 .. Data'Length - 1 loop
            Frame (Position + 2 + I) := Data (Data'First + I);
         end loop;
         Position := Position + 2 + Data'Length;
      end Element;
   begin
      Frame := [others => 0];
      Header (Frame, FC_Association_Request, BSSID, Own);
      Put16 (Frame, Header_Bytes, Capability);
      Put16 (Frame, Header_Bytes + 2, Listen_Interval);
      Element (Element_SSID, SSID);
      if Rates'Length > 0 then
         Element (Element_Rates, Rates);
      end if;
      if Extended_Rates'Length > 0 then
         Element (Element_Extended_Rates, Extended_Rates);
      end if;
      --  Extra is already a whole element.
      if Extra'Length > 0 and then Maximum_Frame - Position >= Extra'Length then
         for I in 0 .. Extra'Length - 1 loop
            Frame (Position + I) := Extra (Extra'First + I);
         end loop;
         Position := Position + Extra'Length;
      end if;
      Length := Position;
   end Build_Association_Request;

   procedure Decode (Frame : Octets; BSSID, Own : Address; Item : out Answer) is
      Control : Unsigned_8;
      To : Address;
   begin
      Item := (others => <>);
      if Frame'Length < Header_Bytes + 2 then
         return;
      end if;
      Control := Frame (0);
      To := Address_At (Frame, Address_1_At);
      if Address_At (Frame, Address_2_At) /= BSSID or else Address_At (Frame, Address_3_At) /= BSSID
        or else (To /= Own and then To /= Broadcast)
        or else (Frame (Control_Flags_At) and Flag_Protected) /= 0   --  not decoded here
      then
         return;
      end if;
      if Control = FC_Authentication and then Frame'Length >= Authentication_Bytes and then To = Own then
         Item := (Kind => Authentication_Answer, Algorithm => Get16 (Frame, Header_Bytes),
                  Sequence => Get16 (Frame, Header_Bytes + 2), Status => Get16 (Frame, Header_Bytes + 4),
                  AID => 0);
      elsif Control = FC_Association_Response and then Frame'Length >= Header_Bytes + 6 and then To = Own then
         Item := (Kind => Association_Answer, Status => Get16 (Frame, Header_Bytes + 2),
                  AID => Get16 (Frame, Header_Bytes + 4) and AID_Mask, Sequence => 0, Algorithm => 0);
      elsif Control = FC_Deauthentication then
         Item := (Kind => Deauthenticated, Status => Get16 (Frame, Header_Bytes), others => 0);
      elsif Control = FC_Disassociation then
         Item := (Kind => Disassociated, Status => Get16 (Frame, Header_Bytes), others => 0);
      end if;
   end Decode;

end IEEE80211_Management;
