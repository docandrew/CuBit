with Interfaces; use Interfaces;
package body Servo_Frame_Copy with SPARK_Mode is
   function Offset (Length : Natural; Pitch : Positive; Area : Rectangle; X, Y : Natural)
     return Natural
     with Pre => Client_Glyph_Blend.Fits (Length, Pitch, Area) and then
       X < Area.Width and then Y < Area.Height,
       Post => Offset'Result < Length and then Offset'Result / Pitch = Area.Y + Y and then
         Offset'Result mod Pitch = Area.X + X
   is
   begin return (Area.Y + Y) * Pitch + Area.X + X; end Offset;

   procedure Store (Target : in out Pixels; I : Natural; Value : Unsigned_32)
     with Pre => I in Target'Range,
       Post => Target (I) = Value and
         (for all J in Target'Range => (if J /= I then Target (J) = Target'Old (J)))
   is
   begin Target (I) := Value; end Store;

   procedure Paint
     (Source : Bytes; Target : in out Pixels; Pitch : Positive; Area : Rectangle)
   is
      Row : constant Positive := Area.Width * 4;
      S, D : Natural;
   begin
      for Y in 0 .. Area.Height - 1 loop
         pragma Loop_Invariant
           (for all I in Target'Range =>
              (if not Client_Glyph_Blend.Inside (I, Pitch, Area)
               then Target (I) = Target'Loop_Entry (I)));
         for X in 0 .. Area.Width - 1 loop
            pragma Loop_Invariant
              (for all I in Target'Range =>
                 (if not Client_Glyph_Blend.Inside (I, Pitch, Area)
                  then Target (I) = Target'Loop_Entry (I)));
            S := (Area.Height - 1 - Y) * Row + X * 4;
            D := Offset (Target'Length, Pitch, Area, X, Y);
            pragma Assert (Client_Glyph_Blend.Inside (D, Pitch, Area));
            Store (Target, D, 16#FF00_0000# or
              Shift_Left (Unsigned_32 (Source (S)), 16) or
              Shift_Left (Unsigned_32 (Source (S + 1)), 8) or
              Unsigned_32 (Source (S + 2)));
         end loop;
      end loop;
   end Paint;
   procedure Paint_BGRA
     (Source : Bytes; Source_Pitch : Positive;
      Target : in out Pixels; Pitch : Positive; Area : Rectangle)
   is
      S, D : Natural;
   begin
      for Y in 0 .. Area.Height - 1 loop
         pragma Loop_Invariant
           (for all I in Target'Range =>
              (if not Client_Glyph_Blend.Inside (I, Pitch, Area)
               then Target (I) = Target'Loop_Entry (I)));
         for X in 0 .. Area.Width - 1 loop
            pragma Loop_Invariant
              (for all I in Target'Range =>
                 (if not Client_Glyph_Blend.Inside (I, Pitch, Area)
                  then Target (I) = Target'Loop_Entry (I)));
            S := (Area.Height - 1 - Y) * Source_Pitch + X * 4;
            D := Offset (Target'Length, Pitch, Area, X, Y);
            pragma Assert (Client_Glyph_Blend.Inside (D, Pitch, Area));
            Store (Target, D, 16#FF00_0000# or
              Shift_Left (Unsigned_32 (Source (S + 2)), 16) or
              Shift_Left (Unsigned_32 (Source (S + 1)), 8) or
              Unsigned_32 (Source (S)));
         end loop;
      end loop;
   end Paint_BGRA;
end Servo_Frame_Copy;
