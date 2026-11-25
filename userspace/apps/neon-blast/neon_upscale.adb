------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Neon_Upscale with SPARK_Mode is

   --  Blend weights in 256ths.
   Fraction_Bits : constant := 8;
   One : constant := 2 ** Fraction_Bits;
   pragma Compile_Time_Error (Weight'Last /= One - 1, "a weight is a fraction of One");

   --  Sample positions in 32.32 fixed point.
   Position_Bits : constant := 32;
   Unit : constant := 2 ** Position_Bits;
   Half_Unit : constant := Unit / 2;
   --  Below the fraction kept: 32 - 8 bits.
   Fraction_Shift : constant := 2 ** (Position_Bits - Fraction_Bits);
   type Position is range -(2 ** 62) .. 2 ** 62;
   subtype Step_Size is Position range 0 .. Position (Maximum_Extent) * Unit;

   Red_Blue : constant Unsigned_32 := 16#00FF_00FF#;
   Green : constant Unsigned_32 := 16#0000_FF00#;

   --  (A * (256 - F) + B * F) / 256 per channel. The terms cannot carry
   --  into a neighbouring channel: each channel's sum is at most 255 * 256.
   function Blend (A, B : Unsigned_32; F : Weight) return Unsigned_32 is
     ((Shift_Right ((A and Red_Blue) * (One - F) + (B and Red_Blue) * F, Fraction_Bits) and Red_Blue)
      or (Shift_Right ((A and Green) * (One - F) + (B and Green) * F, Fraction_Bits) and Green));

   function Darken (P : Unsigned_32) return Unsigned_32 is
     ((Shift_Right ((P and Red_Blue) * Scanline_Weight, Fraction_Bits) and Red_Blue)
      or (Shift_Right ((P and Green) * Scanline_Weight, Fraction_Bits) and Green));

   --  Target coordinate T samples Start + T * Step on the source axis.
   function Step_Of (Source, Target : Extent) return Step_Size is
     (Position (Source) * Unit / Position (Target));
   function Start_Of (Step : Step_Size) return Position is (Step / 2 - Half_Unit);

   --  A sample position as a source index and a blend weight toward the
   --  next index (zero at the last one).
   procedure Split (P : Position; Source : Extent; Index : out Natural; F : out Weight)
     with Post => Index < Source and then (if F /= 0 then Index < Source - 1)
   is
   begin
      if P <= 0 then
         Index := 0;
         F := 0;
      elsif P / Unit >= Position (Source - 1) then
         Index := Source - 1;
         F := 0;
      else
         Index := Natural (P / Unit);
         F := Weight ((P / Fraction_Shift) mod One);
      end if;
   end Split;

   procedure Scale
     (Source : Pixel_Array; Source_Width, Source_Height : Extent;
      Target : in out Pixel_Array; Width, Height, Pitch : Extent;
      Rows : in out Pixel_Array; Left, Right : in out Index_Array; Weights : in out Weight_Array;
      Scanlines : Boolean)
   is
      Step_X : constant Step_Size := Step_Of (Source_Width, Width);
      Step_Y : constant Step_Size := Step_Of (Source_Height, Height);
      Start_X : constant Position := Start_Of (Step_X);
      Start_Y : constant Position := Start_Of (Step_Y);
      subtype Slot is Natural range 0 .. 1;
      type Held_Rows is array (Slot) of Integer;
      Held : Held_Rows := [others => -1];

      --  Source row Source_Row widened into Rows' slot Into.
      procedure Widen (Source_Row : Natural; Into : Slot)
        with Pre => Source_Row < Source_Height
                    and then Source'First = 0 and then Source'Last >= Source_Width * Source_Height - 1
                    and then Rows'First = 0 and then Rows'Last >= 2 * Width - 1
                    and then Left'First = 0 and then Left'Last >= Width - 1
                    and then Right'First = 0 and then Right'Last >= Width - 1
                    and then Weights'First = 0 and then Weights'Last >= Width - 1
                    and then (for all X in 0 .. Width - 1 =>
                                Left (X) < Source_Width and then Right (X) < Source_Width)
      is
         Base : constant Natural := Source_Row * Source_Width;
         Out_Base : constant Natural := Into * Width;
      begin
         pragma Assert (Base <= (Source_Height - 1) * Source_Width);
         for X in 0 .. Width - 1 loop
            --  Rows never overlaps Source or the table: no dependence
            --  between iterations, so GCC may gather and vectorise.
            pragma Warnings (GNATprove, Off, "pragma ""Loop_Optimize"" ignored*",
                             Reason => "a code generation hint; nothing to prove");
            pragma Loop_Optimize (Ivdep);
            pragma Warnings (GNATprove, On, "pragma ""Loop_Optimize"" ignored*");
            Rows (Out_Base + X) := Blend (Source (Base + Left (X)), Source (Base + Right (X)), Weights (X));
         end loop;
      end Widen;

      Y0 : Natural;
      FY : Weight;
      Upper, Lower : Natural;
      Row : Natural;
   begin
      --  The column table: where each window column samples.
      for X in 0 .. Width - 1 loop
         Split (Start_X + Position (X) * Step_X, Source_Width, Left (X), Weights (X));
         Right (X) := (if Weights (X) = 0 then Left (X) else Left (X) + 1);
         pragma Loop_Invariant
           (for all J in 0 .. X => Left (J) < Source_Width and then Right (J) < Source_Width);
      end loop;

      for Y in 0 .. Height - 1 loop
         Split (Start_Y + Position (Y) * Step_Y, Source_Height, Y0, FY);
         if Held (Y0 mod 2) /= Y0 then
            Widen (Y0, Y0 mod 2);
            Held (Y0 mod 2) := Y0;
         end if;
         if FY /= 0 and then Held ((Y0 + 1) mod 2) /= Y0 + 1 then
            Widen (Y0 + 1, (Y0 + 1) mod 2);
            Held ((Y0 + 1) mod 2) := Y0 + 1;
         end if;
         Upper := (Y0 mod 2) * Width;
         Lower := ((Y0 + 1) mod 2) * Width;
         Row := Y * Pitch;
         pragma Assert (Row <= (Height - 1) * Pitch);
         --  Four plain loops, so each vectorises.
         if FY = 0 then
            if Scanlines and then Y mod 2 = 1 then
               for X in 0 .. Width - 1 loop
                  Target (Row + X) := Darken (Rows (Upper + X));
               end loop;
            else
               for X in 0 .. Width - 1 loop
                  Target (Row + X) := Rows (Upper + X);
               end loop;
            end if;
         elsif Scanlines and then Y mod 2 = 1 then
            for X in 0 .. Width - 1 loop
               Target (Row + X) := Darken (Blend (Rows (Upper + X), Rows (Lower + X), FY));
            end loop;
         else
            for X in 0 .. Width - 1 loop
               Target (Row + X) := Blend (Rows (Upper + X), Rows (Lower + X), FY);
            end loop;
         end if;
      end loop;
   end Scale;

end Neon_Upscale;
