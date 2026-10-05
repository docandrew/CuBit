pragma Assertion_Policy (Post => Ignore, Loop_Invariant => Ignore, Assert => Ignore);
package body Client_Raster with SPARK_Mode is
   --  Proved free of run-time errors; tests/ui-raster/run.sh re-proves every
   --  unit carrying this pragma and fails on any unproved check.
   pragma Suppress (All_Checks);

   --  Row Line of R starts at this index.
   function Row_Start (Length : Natural; Pitch : Positive; R : Area; Line : Natural) return Natural
     with Pre => Fits (Length, Pitch, R) and then R.Width > 0 and then Line < R.Height,
          Post => Row_Start'Result + R.Width <= Length and then
                  Row_Start'Result / Pitch = R.Y + Line and then Row_Start'Result mod Pitch = R.X
   is
   begin
      return (R.Y + Line) * Pitch + R.X;
   end Row_Start;

   --  The pixel at column X of row Line of R.
   function Offset (Length : Natural; Pitch : Positive; R : Area; Line, X : Natural) return Natural
     with Pre => Fits (Length, Pitch, R) and then Line < R.Height and then X < R.Width,
          Post => Offset'Result < Length and then Offset'Result / Pitch = R.Y + Line and then
                  Offset'Result mod Pitch = R.X + X
   is
   begin
      return (R.Y + Line) * Pitch + R.X + X;
   end Offset;

   procedure Fill (Target : in out Pixels; Pitch : Positive; R : Area; Value : Word) is
      Start : Natural;
   begin
      if R.Width = 0 or else R.Height = 0 then
         return;
      end if;
      for Line in 0 .. R.Height - 1 loop
         Start := Row_Start (Target'Length, Pitch, R, Line);
         --  One store per row: the compiler widens it.
         Target (Start .. Start + R.Width - 1) := [others => Value];
         pragma Loop_Invariant
           (for all I in Target'Range =>
              (if Inside (I, Pitch, R) and then I / Pitch - R.Y <= Line then Target (I) = Value
               elsif Inside (I, Pitch, R) then Target (I) = Target'Loop_Entry (I)
               else Target (I) = Target'Loop_Entry (I)));
      end loop;
   end Fill;

   procedure Blit_Mask
     (Target : in out Pixels; Pitch : Positive; R : Area; Source : Mask;
      Row : Mask_Row; Column : Mask_Column; Ink : Word;
      Opaque : Boolean; Background : Word)
   is
      At_Pixel : Natural;
      Alpha : Byte;
   begin
      if R.Width = 0 or else R.Height = 0 then
         return;
      end if;
      for Line in 0 .. R.Height - 1 loop
         for X in 0 .. R.Width - 1 loop
            Alpha := Source (Row + Line, Column + X);
            At_Pixel := Offset (Target'Length, Pitch, R, Line, X);
            if Opaque then
               Target (At_Pixel) := Mix (Ink, Background, Alpha);
            elsif Alpha /= 0 then
               Target (At_Pixel) := Mix (Ink, Target (At_Pixel), Alpha);
            end if;
            pragma Loop_Invariant
              (for all I in Target'Range =>
                 (if not Inside (I, Pitch, R) then Target (I) = Target'Loop_Entry (I)));
         end loop;
         pragma Loop_Invariant
           (for all I in Target'Range =>
              (if not Inside (I, Pitch, R) then Target (I) = Target'Loop_Entry (I)));
      end loop;
   end Blit_Mask;

   procedure Copy_Block
     (Target : in out Pixels; Pitch : Positive; R : Area;
      Source : Pixels; Source_Pitch : Positive; Source_X, Source_Y : Natural)
   is
      From : constant Area := (Source_X, Source_Y, R.Width, R.Height);
      To_Start, From_Start : Natural;
   begin
      if R.Width = 0 or else R.Height = 0 then
         return;
      end if;
      for Line in 0 .. R.Height - 1 loop
         To_Start := Row_Start (Target'Length, Pitch, R, Line);
         From_Start := Row_Start (Source'Length, Source_Pitch, From, Line);
         Target (To_Start .. To_Start + R.Width - 1) := Source (From_Start .. From_Start + R.Width - 1);
         pragma Loop_Invariant
           (for all I in Target'Range =>
              (if not Inside (I, Pitch, R) then Target (I) = Target'Loop_Entry (I)));
      end loop;
   end Copy_Block;
end Client_Raster;
