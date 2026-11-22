with Intel_GPU_Submission_Backing;
with Intel_GPU_VA_Encoding;
package body Intel_GPU_ADLN_Offscreen_Surface with SPARK_Mode is
   function Build (MOCS : Unsigned_32) return Image is
      Result : Image;
      Address : constant Unsigned_64 := Intel_GPU_VA_Encoding.Canonical
        (Intel_GPU_Submission_Backing.Offscreen_GPU_VA);
   begin
      if MOCS = 0 or else MOCS > 126 or else MOCS mod 2 /= 0 then return Result; end if;
      Result.Words (0) := Encode (DW0_Fields'
        (Horizontal_Alignment => 1, Vertical_Alignment => 1,
         Surface_Format => 16#C0#, Surface_Type => 1, others => <>));
      Result.Words (1) := Encode (DW1_Fields'
        (QPitch => 16, MOCS => Bits_7 (MOCS), Unorm_Path => 1, others => <>));
      Result.Words (2) := Encode (DW2_Fields'
        (Width_Minus_One => 63, Height_Minus_One => 63, others => <>));
      Result.Words (3) := Encode (DW3_Fields'(Pitch_Minus_One => 255, others => <>));
      Result.Words (4) := Encode (DW4_Fields'(others => <>));
      Result.Words (5) := Encode (DW5_Fields'(Mip_Tail_Start => 1, others => <>));
      Result.Words (6) := Encode (DW6_Fields'(others => <>));
      Result.Words (7) := Encode (DW7_Fields'
        (Channel_Alpha => 7, Channel_Blue => 6, Channel_Green => 5,
         Channel_Red => 4, others => <>));
      Result.Words (8) := Unsigned_32 (Address and 16#FFFF_FFFF#);
      Result.Words (9) := Unsigned_32 (Shift_Right (Address, 32));
      -- DW10..15: no auxiliary/clear-address state; retained as zero.
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_Offscreen_Surface;
