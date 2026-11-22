------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body RTW89_RX with SPARK_Mode is

   procedure Decode (B : Buffer; D : out Descriptor; Write_Size : out Payload_Length;
                     Result : out Status)
   is
      Info : constant Unsigned_32 := U32 (B, 0);
      Size : constant Natural := Natural (Info and Write_Size_Mask);
      W0, W1, W3, W4 : Unsigned_32;
      Descriptor_Length : Natural;
      Offset : Natural;
   begin
      D := (others => <>);
      Write_Size := 0;
      if Size > Buffer_Bytes then
         Result := Size_Too_Large;
         return;
      end if;
      Write_Size := Size;
      if (Info and First_Segment) = 0 then
         Result := Not_First_Segment;
         return;
      elsif (Info and Last_Segment) = 0 then
         Result := Multi_Buffer;
         return;
      elsif Size < Info_Bytes + Short_Descriptor then
         Result := Size_Too_Small;
         return;
      end if;
      W0 := U32 (B, Descriptor_At);
      W1 := U32 (B, Descriptor_At + 4);
      W3 := U32 (B, Descriptor_At + 12);
      D.Long := (W0 and Long_Bit) /= 0;
      Descriptor_Length := (if D.Long then Long_Descriptor else Short_Descriptor);
      if Size < Info_Bytes + Descriptor_Length then
         Result := Size_Too_Small;
         return;
      end if;
      D.Single := True;
      D.Shift := Shift_Right (W0, Shift_Shift) and Two_Bit_Mask;
      D.MAC_Info := (W0 and MAC_Info_Valid_Bit) /= 0;
      D.Kind := Packet_Type'Val (Natural (Shift_Right (W0, Packet_Type_Shift) and Nibble_Mask));
      D.PPDU_Count := Shift_Right (W1, PPDU_Count_Shift) and Three_Bit_Mask;
      D.Rate := Shift_Right (W1, Rate_Shift) and Rate_Mask;
      D.GI_LTF := Shift_Right (W1, GI_LTF_Shift) and Three_Bit_Mask;
      D.Bandwidth := Shift_Right (W1, Bandwidth_Shift) and Two_Bit_Mask;
      D.A1_Match := (W3 and A1_Match_Bit) /= 0;
      D.CRC_Error := (W3 and CRC32_Error_Bit) /= 0;
      D.ICV_Error := (W3 and ICV_Error_Bit) /= 0;
      D.HW_Decrypted := (W3 and HW_Decrypt_Bit) /= 0;
      D.SW_Decrypt := (W3 and SW_Decrypt_Bit) /= 0;
      if D.Long then
         W4 := U32 (B, Descriptor_At + 16);
         D.Frame := Frame_Type'Val (Natural (W4 and Frame_Type_Mask));
      end if;
      if D.Shift /= 0 then
         Result := Shift_Nonzero;
         return;
      end if;
      Offset := Info_Bytes + Descriptor_Length
        + Natural (Shift_Right (W0, Driver_Info_Shift) and Three_Bit_Mask) * Driver_Info_Unit;
      if Offset > Size or else Natural (W0 and Packet_Length_Mask) > Size - Offset then
         Result := Payload_Overrun;
         return;
      end if;
      D.Length := Natural (W0 and Packet_Length_Mask);
      if Offset = Buffer_Bytes then
         --  Only an empty payload can start at the buffer's end.
         D.Payload_First := 0;
         D.Length := 0;
      else
         D.Payload_First := Offset;
      end if;
      Result := Decoded;
   end Decode;

   ---------------------------------------------------------------------------
   procedure Decode_PPDU (B : Buffer; D : Descriptor; Report : out PPDU_Report;
                          Result : out PPDU_Result)
   is
      First : constant Natural := D.Payload_First;
      Last  : constant Natural := D.Payload_First + D.Length;   --  one past
      PHY   : Natural := First;
      H0, H1 : Unsigned_32;
      PHY_Length : Natural;
   begin
      Report := (others => <>);
      if D.MAC_Info then
         if D.Length < MAC_Info_Bytes then
            Result := MAC_Info_Short;
            return;
         end if;
         declare
            M0 : constant Unsigned_32 := U32 (B, First);
            M1 : constant Unsigned_32 := U32 (B, First + 4);
            Users : constant Natural := Natural (M0 and User_Count_Mask);
            Block : Natural;
         begin
            if Users > Maximum_Users then
               Result := Too_Many_Users;
               return;
            end if;
            Block := MAC_Info_Bytes + Users * MAC_Info_User_Bytes
              + (if Users mod 2 = 1 then MAC_Info_User_Bytes else 0)
              + (if (M0 and RX_Counter_Valid_Bit) /= 0 then RX_Counter_Bytes else 0)
              + Natural (Shift_Right (M1, PLCP_Length_Shift) and Byte_Mask) * PLCP_Unit;
            if Block > D.Length then
               Result := MAC_Info_Short;
               return;
            end if;
            PHY := First + Block;
         end;
      end if;
      PHY_Length := Last - PHY;
      if PHY_Length < PHY_Header_Bytes then
         Result := PHY_Too_Short;
         return;
      end if;
      H0 := U32 (B, PHY);
      H1 := U32 (B, PHY + 4);
      if (H0 and Valid_Bit) = 0 then
         Result := PHY_Not_Valid;
         return;
      elsif Natural (Shift_Right (H0, Length_Shift) and Byte_Mask) * Status_Unit /= PHY_Length then
         Result := PHY_Length_Mismatch;
         return;
      end if;
      Report.Class := Unsigned_8 (H0 and Class_Mask);
      Report.RSSI_Average := Unsigned_8 (Shift_Right (H0, RSSI_Average_Shift) and Byte_Mask);
      Report.RSSI_A := Unsigned_8 (H1 and Byte_Mask);
      Report.RSSI_B := Unsigned_8 (Shift_Right (H1, RSSI_B_Shift) and Byte_Mask);
      Result := Taken;

      --  The IE walk: only packet classes (not failure/break reports).
      if Report.Class < First_Packet_Class or else (H0 and Header_2_Bit) /= 0 then
         return;
      end if;
      declare
         Position : Natural := PHY + PHY_Header_Bytes;
         Header : Unsigned_32;
         Kind : Natural;
         Length : Natural;
      begin
         while Position < Last loop
            pragma Loop_Invariant (Position >= PHY + PHY_Header_Bytes and then Last <= Buffer_Bytes);
            pragma Loop_Variant (Increases => Position);
            if Last - Position < 4 then
               return;   --  a truncated IE header: the walk fails
            end if;
            Header := U32 (B, Position);
            Kind := Natural (Header and IE_Type_Mask);
            Length := (if Variable_IE_Types (Kind)
                       then Natural (Shift_Right (Header, IE_Length_Shift) and IE_Length_Mask) * Status_Unit
                       else IE_Lengths (Kind));
            if Length = 0 or else Length > Last - Position then
               return;   --  malformed: the walk fails, the header RSSIs stand
            end if;
            if Length >= IE_Read_Bytes then
               if Kind = IE_CMN_CCK and then D.A1_Match then
                  Report.RPL_Average :=
                    Unsigned_8 (Shift_Right (Shift_Right (Header, CCK_RPL_Shift) and CCK_RPL_Mask, 1));
                  Report.Has_RPL := True;
               elsif Kind = IE_CMN_OFDM then
                  Report.Channel_Index :=
                    Unsigned_8 (Shift_Right (Header, OFDM_Channel_Shift) and Byte_Mask);
                  if D.A1_Match and then (D.Rate and Rate_Mask) >= OFDM_6_Rate then
                     Report.RPL_Average :=
                       Unsigned_8 (Shift_Right (Header, OFDM_RSSI_Shift) and Byte_Mask);
                     Report.Has_RPL := True;
                  end if;
               end if;
            end if;
            Position := Position + Length;
         end loop;
         Report.IEs_Walked := True;
      end;
   end Decode_PPDU;

   ---------------------------------------------------------------------------
   Rate_Mode_Shift : constant := 7;
   Mode_HT         : constant := 1;
   Legacy_Index    : constant Unsigned_32 := 16#F#;
   HT_Index        : constant Unsigned_32 := 16#1F#;

   function Rate_Index (Rate : Rate_Code) return Unsigned_32 is
     (if Shift_Right (Rate, Rate_Mode_Shift) = Mode_HT then Rate and HT_Index
      else Rate and Legacy_Index);

   --  The GI class (rtw89's HE GI map): 0 = 3.2 us, 1 = 0.8 us, 2 = 1.6 us,
   --  3 = invalid.
   function GI_Class (GI : Small_Field) return Natural is
     (case GI is
        when 1 | 3 | 5 => 1,
        when 2 | 4     => 2,
        when 0         => 0,
        when others    => 3);

   function Matches (Frame, Status : Descriptor) return Boolean is
     (Frame.PPDU_Count = Status.PPDU_Count
      and then Shift_Right (Frame.Rate, Rate_Mode_Shift) = Shift_Right (Status.Rate, Rate_Mode_Shift)
      and then Rate_Index (Frame.Rate) = Rate_Index (Status.Rate)
      and then GI_Class (Status.GI_LTF) /= 3
      and then (if GI_Class (Frame.GI_LTF) = 3 then 0 else GI_Class (Frame.GI_LTF))
               = GI_Class (Status.GI_LTF)
      and then Frame.Bandwidth = Status.Bandwidth);

   ---------------------------------------------------------------------------
   Base_Shift : constant := 4;
   Two_GHz_Base : constant := 0;
   type Base_Table is array (Unsigned_8 range 0 .. 15) of Unsigned_8;
   Invalid_Base : constant Unsigned_8 := 16#FF#;
   Bases : constant Base_Table :=
     [1, Invalid_Base, 36, 100, 132, 149, Invalid_Base, 1, 33, 65, 97, 129, 161, 193, 225,
      Invalid_Base];
   Channel_Step : constant := 2;
   First_5_GHz_Base : constant := 2;
   Last_5_GHz_Base  : constant := 5;

   function Decode_Channel_Index (Index : Unsigned_8) return Decoded_Channel is
      Base   : constant Unsigned_8 := Shift_Right (Index, Base_Shift);
      Offset : constant Unsigned_8 := Index and 16#0F#;
   begin
      if Index = 0 then
         return (others => <>);
      elsif Base = Two_GHz_Base then
         return (if Offset in 1 .. 14 then (Band_2_4, Offset) else (others => <>));
      elsif Base in First_5_GHz_Base .. Last_5_GHz_Base then
         declare
            C : constant Natural := Natural (Bases (Base)) + Channel_Step * Natural (Offset);
         begin
            return (if C <= 177 then (Band_5, Unsigned_8 (C)) else (others => <>));
         end;
      else
         return (others => <>);
      end if;
   end Decode_Channel_Index;

   ---------------------------------------------------------------------------
   procedure Decode_C2H (B : Buffer; D : Descriptor; H : out C2H_Header; Valid : out Boolean) is
      W0, W1 : Unsigned_32;
      Length : Natural;
   begin
      H := (others => <>);
      Valid := False;
      if D.Length < C2H_Header_Bytes then
         return;
      end if;
      W0 := U32 (B, D.Payload_First);
      W1 := U32 (B, D.Payload_First + 4);
      Length := Natural (W1 and C2H_Length_Mask);
      if Length < C2H_Header_Bytes or else Length > D.Length then
         return;
      end if;
      H := (Category => Unsigned_8 (W0 and Two_Bit_Mask),
            Class    => Unsigned_8 (Shift_Right (W0, C2H_Class_Shift) and C2H_Class_Mask),
            Func     => Unsigned_8 (Shift_Right (W0, C2H_Function_Shift) and Byte_Mask),
            Length   => Length);
      Valid := True;
   end Decode_C2H;

end RTW89_RX;
