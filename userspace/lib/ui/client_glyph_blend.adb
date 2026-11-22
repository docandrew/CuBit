package body Client_Glyph_Blend with SPARK_Mode is
   --  Proved free of run-time errors; tests/ui-raster/run.sh re-proves every
   --  unit carrying this pragma and fails on any unproved check.
   pragma Suppress (All_Checks);
   use Interfaces;
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
      Ink_Row : constant Glyph_Gamma.Bucket := Glyph_Gamma.Bucket_Of (Tint);
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
               if Mask (S) /= 0 then Store (Target, T, Ink (Tint, Target (T), Ink_Row, Mask (S))); end if;
            end;
         end loop;
      end loop;
   end Paint;
end Client_Glyph_Blend;
