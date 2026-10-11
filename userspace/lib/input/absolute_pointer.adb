package body Absolute_Pointer with SPARK_Mode is
   use Interfaces;

   procedure Lemma_Encode_Decode (R : Report) is
      W : constant Word := Encode (R);
   begin
      pragma Assert ((W and Byte_Mask) = Word (R.Buttons));
      pragma Assert ((Shift_Right (W, X_Shift) and Axis_Mask) = Word (R.X));
      pragma Assert ((Shift_Right (W, Y_Shift) and Axis_Mask) = Word (R.Y));
      pragma Assert ((Shift_Right (W, Wheel_Shift) and Byte_Mask) =
                     Word (Wheel_Field (R.Wheel)));
      pragma Assert ((Shift_Right (W, Flags_Shift) and Byte_Mask) = Word (R.Flags));
      pragma Assert (Shift_Right (W, Reserved_Shift) = 0);
      pragma Assert (Valid_Payload (W));
   end Lemma_Encode_Decode;

   procedure Lemma_Decode_Encode (W : Word) is
      R : constant Report := Decode (W);
      Field : constant Unsigned_8 :=
        Unsigned_8 (Shift_Right (W, Wheel_Shift) and Byte_Mask);
   begin
      pragma Assert (Wheel_Field (Wheel_Value (Field)) = Field);
      pragma Assert (Word (R.Buttons) = (W and Byte_Mask));
      pragma Assert (Word (R.X) = (Shift_Right (W, X_Shift) and Axis_Mask));
      pragma Assert (Word (R.Y) = (Shift_Right (W, Y_Shift) and Axis_Mask));
      pragma Assert (Word (R.Flags) = (Shift_Right (W, Flags_Shift) and Byte_Mask));
      pragma Assert (Word (Wheel_Field (R.Wheel)) = (Shift_Right (W, Wheel_Shift) and Byte_Mask));
      --  W is its fields put back in place (reserved bits are zero).
      pragma Assert
        (W = ((W and Byte_Mask)
              or Shift_Left (Shift_Right (W, X_Shift) and Axis_Mask, X_Shift)
              or Shift_Left (Shift_Right (W, Y_Shift) and Axis_Mask, Y_Shift)
              or Shift_Left (Shift_Right (W, Wheel_Shift) and Byte_Mask, Wheel_Shift)
              or Shift_Left (Shift_Right (W, Flags_Shift) and Byte_Mask, Flags_Shift)
              or Shift_Left (Shift_Right (W, Reserved_Shift), Reserved_Shift)));
      pragma Assert (Encode (R) = W);
   end Lemma_Decode_Encode;

   procedure Lemma_Exact_Inverse (Pixel : Natural; Extent : Pixel_Extent) is
      K : constant Long_Long_Integer := Axis_Last;
      E : constant Long_Long_Integer := Long_Long_Integer (Extent);
      P : constant Long_Long_Integer := Long_Long_Integer (Pixel);
      Position : constant Axis_Position := Host_Position (Pixel, Extent);
      V : constant Long_Long_Integer :=
        Long_Long_Integer (Position);
   begin
      --  V = floor (P * K / E): V * E <= P * K < (V + 1) * E.
      pragma Assert (V = P * K / E);
      pragma Assert (V * E <= P * K);
      pragma Assert (P * K < (V + 1) * E);
      --  Extent <= K, so V * E > P * K - E >= (P - 1) * K.
      pragma Assert (V * E + K - 1 >= P * K);
      pragma Assert (V * E + K - 1 < (P + 1) * K);
      pragma Assert ((V * E + K - 1) / K = P);
   end Lemma_Exact_Inverse;

   procedure Lemma_To_Pixel_Monotone
     (A, B : Axis_Position; Extent : Pixel_Extent) is
   begin
      pragma Assert (Natural (A) * Extent <= Natural (B) * Extent);
      pragma Assert ((Natural (A) * Extent + Axis_Last - 1) / Axis_Last <=
                     (Natural (B) * Extent + Axis_Last - 1) / Axis_Last);
   end Lemma_To_Pixel_Monotone;
end Absolute_Pointer;
