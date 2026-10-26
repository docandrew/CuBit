------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body IEEE80211_Beacons with SPARK_Mode is

   function U16 (Frame : Frame_Bytes; At_Byte : Natural) return Unsigned_16 is
     (Unsigned_16 (Frame (At_Byte)) or Shift_Left (Unsigned_16 (Frame (At_Byte + 1)), 8))
   with Pre => Frame'First = 0 and then At_Byte < Frame'Length - 1;

   function Address_At (Frame : Frame_Bytes; At_Byte : Natural) return Address is
     [for I in Address'Range => Frame (At_Byte + I)]
   with Pre => Frame'First = 0 and then Frame'Length >= Address_Bytes
               and then At_Byte <= Frame'Length - Address_Bytes;

   procedure Decode (Frame : Frame_Bytes; Info : out Decoded; Result : out Status) is
      Control   : Unsigned_8;
      Flags     : Unsigned_8;
      Body_At   : Natural;
      Position  : Natural;
      Last      : Natural;   --  one past the frame's last octet
      ID        : Unsigned_8;
      Length    : Natural;
   begin
      Info := (others => <>);
      if Frame'Length < Minimum_Frame then
         Result := Too_Short;
         return;
      end if;
      Control := Frame (0);
      Flags := Frame (1);
      if (Control and Version_Mask) /= 0 then
         Result := Not_Version_0;
         return;
      elsif (Control and Type_Mask) /= Type_Management then
         Result := Not_Management;
         return;
      elsif (Control and Subtype_Mask) /= Subtype_Beacon
        and then (Control and Subtype_Mask) /= Subtype_Probe_Response
      then
         Result := Not_Beacon;
         return;
      elsif (Flags and Flag_Protected) /= 0 then
         Result := Protected_Frame;
         return;
      end if;

      Body_At := Header_Bytes + (if (Flags and Flag_Order) /= 0 then HT_Control_Bytes else 0);
      if Frame'Length < Body_At + Elements_Offset then
         Result := Too_Short;
         return;
      end if;

      Info.Kind := (if (Control and Subtype_Mask) = Subtype_Beacon then Beacon else Probe_Response);
      Info.Transmitter := Address_At (Frame, Address_2_At);
      Info.BSSID := Address_At (Frame, Address_3_At);
      Info.Interval := U16 (Frame, Body_At + Interval_Offset);
      Info.Capability := U16 (Frame, Body_At + Capability_Offset);

      Last := Frame'Length;
      Position := Body_At + Elements_Offset;
      while Position < Last loop
         pragma Loop_Invariant
           (Position >= Minimum_Frame and then Last = Frame'Length and then Normal (Info)
            and then Info.Elements <= (Position - Minimum_Frame) / Element_Header_Bytes);
         pragma Loop_Variant (Increases => Position);
         if Last - Position < Element_Header_Bytes then
            Result := Element_Overrun;
            return;
         end if;
         ID := Frame (Position);
         Length := Natural (Frame (Position + 1));
         if Length > Last - Position - Element_Header_Bytes then
            Result := Element_Overrun;
            return;
         end if;
         declare
            Data : constant Natural := Position + Element_Header_Bytes;
            First_Of_Kind : Boolean;
         begin
            Info.Elements := Info.Elements + 1;
            case ID is
               when Element_SSID =>
                  if Length > SSID_Octets then
                     Result := SSID_Too_Long;
                     return;
                  end if;
                  First_Of_Kind := not Info.Has_SSID;
                  if First_Of_Kind then
                     Info.Has_SSID := True;
                     Info.Length := Length;
                     for I in 1 .. Length loop
                        pragma Loop_Invariant
                          (Info.Length = Length and then Info.Has_SSID
                           and then Info.Skipped < Info.Elements
                           and then (for all J in 1 .. SSID_Octets =>
                                       (if J > Length then Info.SSID (J) = 0)));
                        Info.SSID (I) := Frame (Data + I - 1);
                     end loop;
                  end if;
               when Element_DS_Params =>
                  if Length /= DS_Params_Length then
                     Result := DS_Params_Malformed;
                     return;
                  end if;
                  First_Of_Kind := Info.DS_Channel = No_Channel;
                  if First_Of_Kind then
                     Info.DS_Channel := Frame (Data);
                  end if;
               when Element_HT_Operation =>
                  First_Of_Kind := Length >= HT_Operation_Length and then Info.HT_Primary = No_Channel;
                  if First_Of_Kind then
                     Info.HT_Primary := Frame (Data);
                  end if;
               when Element_RSN =>
                  First_Of_Kind := Length >= RSN_Minimum_Length and then not Info.RSN;
                  if First_Of_Kind then
                     Info.RSN := True;
                  end if;
               when others =>
                  First_Of_Kind := False;
            end case;
            if not First_Of_Kind then
               Info.Skipped := Info.Skipped + 1;
            end if;
            Position := Data + Length;
         end;
      end loop;
      Result := Accepted;
   end Decode;

end IEEE80211_Beacons;
