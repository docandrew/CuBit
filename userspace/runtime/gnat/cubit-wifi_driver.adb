------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Wifi_Driver with SPARK_Mode is

   function Operation_Of (Value : Unsigned_32) return Operation is
   begin
      for Op in Operation loop
         if Label (Op) = Value then
            return Op;
         end if;
         pragma Loop_Invariant (for all P in Operation'First .. Op => Label (P) /= Value);
      end loop;
      raise Program_Error;   --  unreachable: Is_Operation (Value)
   end Operation_Of;

   function Command_Of (Value : Unsigned_32) return Command is
   begin
      for C in Command loop
         if Label (C) = Value then
            return C;
         end if;
         pragma Loop_Invariant (for all P in Command'First .. C => Label (P) /= Value);
      end loop;
      raise Program_Error;   --  unreachable: Is_Command (Value)
   end Command_Of;

   procedure Decode_Port (W : Unsigned_64; State : out Port_State; Valid : out Boolean) is
   begin
      State := Closed;
      Valid := W = Unsigned_64 (Port_State'Enum_Rep (Closed));
      if W = Unsigned_64 (Port_State'Enum_Rep (Open_Port)) then
         State := Open_Port;
         Valid := True;
      end if;
   end Decode_Port;

   function Decode (W : Words2) return Decoded_Join is
      Band_Value : constant Unsigned_64 := W (1) and Byte_Field;
      Channel_Value : constant Unsigned_64 := Shift_Right (W (1), Channel_Shift);
      J : Join_Request;
   begin
      if not NL.Valid_Address_Word (W (0)) then
         return (Valid => False);
      elsif Band_Value = Unsigned_64 (WS.Band'Enum_Rep (WS.Band_2_4_GHz)) then
         J.On_Band := WS.Band_2_4_GHz;
      elsif Band_Value = Unsigned_64 (WS.Band'Enum_Rep (WS.Band_5_GHz)) then
         J.On_Band := WS.Band_5_GHz;
      else
         return (Valid => False);
      end if;
      if Channel_Value not in Unsigned_64 (WS.Channel_Number'First) .. Unsigned_64 (WS.Channel_Number'Last) then
         return (Valid => False);
      end if;
      J.Channel := WS.Channel_Number (Channel_Value);
      J.BSSID := NL.Address_Of (W (0));
      if not Valid (J) or else Encode (J) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => J);
   end Decode;

   function Decode (W : Words4) return Decoded_Key is
      Kind_Value : constant Unsigned_64 := W (0) and Byte_Field;
      Index_Value : constant Unsigned_64 := Shift_Right (W (0), Index_Shift) and Byte_Field;
      Cipher_Value : constant Unsigned_64 := Shift_Right (W (0), Cipher_Shift);
      K : Key_Install;
   begin
      if Kind_Value = Unsigned_64 (Key_Kind'Enum_Rep (Pairwise)) then
         K.Kind := Pairwise;
      elsif Kind_Value = Unsigned_64 (Key_Kind'Enum_Rep (Group)) then
         K.Kind := Group;
      else
         return (Valid => False);
      end if;
      if Index_Value > Unsigned_64 (Key_Index'Last)
        or else Cipher_Value /= Unsigned_64 (Cipher'Enum_Rep (CCMP_128))
        or else W (3) > Packet_Number'Last
      then
         return (Valid => False);
      end if;
      K.Index := Key_Index (Index_Value);
      K.Suite := CCMP_128;
      for I in 0 .. 7 loop
         K.Key (I) := Unsigned_8 (Shift_Right (W (1), 8 * I) and Byte_Field);
         K.Key (8 + I) := Unsigned_8 (Shift_Right (W (2), 8 * I) and Byte_Field);
      end loop;
      K.RSC := W (3);
      if Encode (K) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => K);
   end Decode;

end CuBit.Wifi_Driver;
