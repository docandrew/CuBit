------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body RTW89_TX with SPARK_Mode is

   Byte_Mask : constant := 16#FF#;
   Header_Unit : constant := 2;

   procedure Put32 (P : in out Page; At_Byte : Natural; V : Unsigned_32)
   with Pre => At_Byte <= Page_Bytes - 4
   is
   begin
      P (At_Byte) := Unsigned_8 (V and Byte_Mask);
      P (At_Byte + 1) := Unsigned_8 (Shift_Right (V, 8) and Byte_Mask);
      P (At_Byte + 2) := Unsigned_8 (Shift_Right (V, 16) and Byte_Mask);
      P (At_Byte + 3) := Unsigned_8 (Shift_Right (V, 24));
   end Put32;

   procedure Put16 (P : in out Page; At_Byte : Natural; V : Unsigned_16)
   with Pre => At_Byte <= Page_Bytes - 2
   is
   begin
      P (At_Byte) := Unsigned_8 (V and Byte_Mask);
      P (At_Byte + 1) := Unsigned_8 (Shift_Right (V, 8));
   end Put16;

   function Fill (R : Request) return Page is
      P : Page := [others => 0];
      Management : constant Boolean := R.On = B0MG;
      Body0 : Unsigned_32 :=
        Body0_WD_Info_Enable or Body0_WD_Page
        or Shift_Left (Unsigned_32 (Channel'Enum_Rep (R.On)), Body0_Channel_Shift);
      Info2 : Unsigned_32 := 0;
      Info_At : constant := Body_Bytes;
      WP_At : constant := Body_Bytes + Info_Bytes;
      Address_At : constant := WP_At + WP_Bytes;
   begin
      if Management then
         Body0 := Body0 or Shift_Left (Unsigned_32'(Management_SSN_Select), Body0_SSN_Select_Shift)
                        or Unsigned_32'(Management_Sequence_Mode);
      else
         Body0 := Body0 or Shift_Left (Unsigned_32 (R.Header_Bytes / Header_Unit), Body0_Header_Shift);
      end if;
      Put32 (P, 0, Body0);
      Put32 (P, 8, Shift_Left (Unsigned_32 (R.Station), Body2_MACID_Shift)
                   or Shift_Left (Unsigned_32 (if Management then QSel_Management else QSel_Best_Effort),
                                  Body2_QSel_Shift)
                   or (Unsigned_32 (R.Size) and Body2_Size_Mask));
      if not Management then
         Put32 (P, 12, Unsigned_32 (R.SW_Sequence) and Body3_Sequence_Mask);
      end if;
      Put32 (P, Info_At, Info0_Use_Rate or Info0_Disable_Fallback or Shift_Left (R.Rate, Info0_Rate_Shift));
      Put32 (P, Info_At + 4, Shift_Left (R.Rate, Info1_Lowest_Rate_Shift));
      if R.Encrypt then
         Info2 := Shift_Left (Unsigned_32'(Key_Type_CCMP_128), Info2_Key_Type_Shift)
                  or Info2_Hardware_Encrypt or Unsigned_32 (R.Security_Entry);
      end if;
      Put32 (P, Info_At + 8, Info2);
      Put32 (P, Info_At + 16, Info4_HW_RTS_Enable or (if R.Group then 0 else Info4_RTS_Enable));
      Put16 (P, WP_At, Unsigned_16 (R.Page_Sequence) or WP_Valid);
      Put16 (P, Address_At, Unsigned_16 (R.Size));
      Put16 (P, Address_At + 2, Address_MSDU_Last or Address_Count_One
                                or RTW89_Descriptors.DMA_Hi (R.Frame_Bus));
      Put32 (P, Address_At + 4, Unsigned_32 (R.Frame_Bus and 16#FFFF_FFFF#));
      return P;
   end Fill;

end RTW89_TX;
