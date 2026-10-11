pragma Ada_2022;
package body Intel_GPU_SLPC with SPARK_Mode is

   function RP0_Request (Cap : Unsigned_32) return Software_Request is
      Field : constant Unsigned_32 := Cap and Freq.Cap_Field_Mask;
      Ratio : Unsigned_32;
      Value : Unsigned_32;
   begin
      pragma Assert (Field <= Freq.Cap_Field_Mask);
      Ratio := Field * Freq.Scaler;
      if Ratio = 0 or else Ratio > Freq.Request_Mask then
         return (Valid => False, Value => 0);
      end if;
      Value := Shift_Left (Ratio, Freq.Request_Shift);
      pragma Assert ((Shift_Right (Value, Freq.Request_Shift) and Freq.Request_Mask) = Ratio);
      pragma Assert (Freq.Requested_MHz (Value) = Freq.Ratio_MHz (Ratio));
      pragma Assert (Unsigned_32'Min (Ratio, Freq.Request_Mask) = Ratio);
      pragma Assert (Ratio * Freq.Multiplier_MHz + Freq.Scaler / 2 =
                     Field * (Freq.Scaler * Freq.Multiplier_MHz) + 1);
      pragma Assert ((Field * (Freq.Scaler * Freq.Multiplier_MHz) + 1) / Freq.Scaler =
                     Field * Freq.Multiplier_MHz);
      pragma Assert (Freq.Ratio_MHz (Ratio) = Field * Freq.Multiplier_MHz);
      pragma Assert (Freq.RP0_MHz (Cap) = Field * Freq.Multiplier_MHz);
      return (Valid => True, Value => Value);
   end RP0_Request;

end Intel_GPU_SLPC;
