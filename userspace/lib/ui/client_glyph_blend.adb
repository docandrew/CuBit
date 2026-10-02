package body Client_Glyph_Blend with SPARK_Mode is
   use Interfaces;
   function Channel (Foreground, Background, Alpha : Channel_Value) return Channel_Value is
     ((Foreground * Alpha + Background * (255 - Alpha) + 127) / 255);
   function RGB (Foreground, Background : Word; Alpha : Byte) return Word is
      A : constant Channel_Value := Natural (Alpha);
      R : constant Channel_Value := Channel
        (Natural (Shift_Right (Foreground, 16) and 255), Natural (Shift_Right (Background, 16) and 255), A);
      G : constant Channel_Value := Channel
        (Natural (Shift_Right (Foreground, 8) and 255), Natural (Shift_Right (Background, 8) and 255), A);
      B : constant Channel_Value := Channel (Natural (Foreground and 255), Natural (Background and 255), A);
   begin
      return Word (R) * 65_536 + Word (G) * 256 + Word (B);
   end RGB;
   function Offset (Length : Natural; Pitch : Positive; Area : Rectangle; X, Y : Natural) return Natural
     with Pre => Fits (Length, Pitch, Area) and then X < Area.Width and then Y < Area.Height,
       Post => Offset'Result < Length and then Offset'Result / Pitch = Area.Y + Y and then
         Offset'Result mod Pitch = Area.X + X
   is
   begin
      return (Area.Y + Y) * Pitch + Area.X + X;
   end Offset;
   procedure Store (Target : in out Pixels; I : Natural; Value : Word)
     with Pre => I in Target'Range,
       Post => Target (I) = Value and
         (for all J in Target'Range => (if J /= I then Target (J) = Target'Old (J)))
   is
   begin Target (I) := Value; end Store;
   procedure Paint
     (Mask : Bytes; Target : in out Pixels; Source_Pitch, Target_Pitch : Positive;
      Source, Destination : Rectangle; Tint : Word)
   is
   begin
      for Y in 0 .. Destination.Height - 1 loop
         pragma Loop_Invariant (for all I in Target'Range =>
           (if not Inside (I, Target_Pitch, Destination) then Target (I) = Target'Loop_Entry (I)));
         for X in 0 .. Destination.Width - 1 loop
            pragma Loop_Invariant (for all I in Target'Range =>
              (if not Inside (I, Target_Pitch, Destination) then Target (I) = Target'Loop_Entry (I)));
            declare
               S : constant Natural := Offset (Mask'Length, Source_Pitch, Source, X, Y);
               T : constant Natural := Offset (Target'Length, Target_Pitch, Destination, X, Y);
            begin
               pragma Assert (Inside (T, Target_Pitch, Destination));
               if Mask (S) /= 0 then Store (Target, T, Mix (Tint, Target (T), Mask (S))); end if;
            end;
         end loop;
      end loop;
   end Paint;
end Client_Glyph_Blend;
