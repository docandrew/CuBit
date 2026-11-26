------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body Client_Thumbnails with SPARK_Mode is

   function Fit (Source, Bound : Size) return Size is
      Width, Height : Natural;
   begin
      --  Height binds when Source is relatively taller than Bound.
      if Source.Width * Bound.Height <= Source.Height * Bound.Width then
         Height := Natural'Min (Source.Height, Bound.Height);
         Width := Source.Width * Height / Source.Height;
      else
         Width := Natural'Min (Source.Width, Bound.Width);
         Height := Source.Height * Width / Source.Width;
      end if;
      Width := Natural'Max (1, Natural'Min (Width, Natural'Min (Bound.Width, Source.Width)));
      Height := Natural'Max (1, Natural'Min (Height, Natural'Min (Bound.Height, Source.Height)));
      return (Width, Height);
   end Fit;

   --  Samples sit on a rotated grid (as N-rooks supersampling places
   --  them): sample (SX, SY) of a target pixel lies in sub-column
   --  Samples_Per_Axis * SX + SY and sub-row Samples_Per_Axis * SY + SX of
   --  its source box, each axis cut into Sub_Cells. No two samples share a
   --  sub-column or a sub-row, so a pattern as fine as the grid's spacing
   --  (a one-pixel checkerboard, hairlines, dithering) averages out instead
   --  of aliasing into a solid colour.
   Sub_Cells : constant := Samples_Per_Axis * Samples_Per_Axis;
   subtype Sample_Index is Natural range 0 .. Samples_Per_Axis - 1;
   subtype Sub_Index is Natural range 0 .. Sub_Cells - 1;
   subtype Source_Coordinate is Natural range 0 .. Maximum_Source_Extent - 1;

   --  The centre of sub-cell Index of the box [Low, High) that target
   --  position Position covers on an axis Extent long, as a pixel there.
   function Sample
     (Position : Natural; Target : Target_Extent; Extent : Source_Extent; Index : Sub_Index)
      return Source_Coordinate
     with Pre => Position < Target,
          Post => Sample'Result < Extent
   is
      Low : constant Natural := Position * Extent / Target;
      High : constant Natural := (Position + 1) * Extent / Target;
      Span : constant Natural := (if High > Low then High - Low else 0);
      Point : constant Natural := Low + (2 * Index + 1) * Span / (2 * Sub_Cells);
   begin
      return Natural'Min (Point, Extent - 1);
   end Sample;

   procedure Scale
     (Source : Pixel_Array; Source_Size : Size; Source_Pitch : Source_Extent;
      Target : in out Pixel_Array; Target_Pitch : Source_Extent;
      Target_X, Target_Y : Natural; Target_Width, Target_Height : Target_Extent;
      First_Row : Natural; Row_Count : Natural)
   is
      Samples : constant := Sub_Cells;
      Opaque : constant Pixel := 16#FF00_0000#;
      subtype Column is Natural range 0 .. Target_Width - 1;
      type Column_Samples is array (Column, Sub_Index) of Source_Coordinate;
      Columns : Column_Samples := [others => [others => 0]];
      Rows : array (Sub_Index) of Source_Coordinate := [others => 0];
   begin
      --  Every column's sample points once; each row's per target row.
      for X in Column loop
         for I in Sub_Index loop
            Columns (X, I) := Sample (X, Target_Width, Source_Size.Width, I);
            pragma Loop_Invariant
              (for all J in Sub_Index'First .. I => Columns (X, J) < Source_Size.Width);
         end loop;
         pragma Loop_Invariant
           (for all C in Column'First .. X =>
              (for all J in Sub_Index => Columns (C, J) < Source_Size.Width));
      end loop;

      for Y in First_Row .. First_Row + Row_Count - 1 loop
         for I in Sub_Index loop
            Rows (I) := Sample (Y, Target_Height, Source_Size.Height, I);
            pragma Loop_Invariant
              (for all J in Sub_Index'First .. I => Rows (J) < Source_Size.Height);
         end loop;
         declare
            Target_Row : constant Natural := (Target_Y + Y) * Target_Pitch;
         begin
            pragma Assert (Target_Y + Y <= Target_Y + Target_Height - 1);
            pragma Assert ((Target_Y + Y) * Target_Pitch <= (Target_Y + Target_Height - 1) * Target_Pitch);
            for X in Column loop
               pragma Loop_Invariant
                 (for all C in Column =>
                    (for all J in Sub_Index => Columns (C, J) < Source_Size.Width));
               pragma Loop_Invariant
                 (for all J in Sub_Index => Rows (J) < Source_Size.Height);
               declare
                  Blue, Green, Red : Natural range 0 .. Samples * 255 := 0;
               begin
                  for SY in Sample_Index loop
                     pragma Loop_Invariant (Blue <= SY * Samples_Per_Axis * 255);
                     pragma Loop_Invariant (Green <= SY * Samples_Per_Axis * 255);
                     pragma Loop_Invariant (Red <= SY * Samples_Per_Axis * 255);
                     for SX in Sample_Index loop
                        pragma Loop_Invariant
                          (Blue <= (SY * Samples_Per_Axis + SX) * 255);
                        pragma Loop_Invariant
                          (Green <= (SY * Samples_Per_Axis + SX) * 255);
                        pragma Loop_Invariant
                          (Red <= (SY * Samples_Per_Axis + SX) * 255);
                        declare
                           Row_Y : constant Source_Coordinate := Rows (Samples_Per_Axis * SY + SX);
                           Column_X : constant Source_Coordinate := Columns (X, Samples_Per_Axis * SX + SY);
                        begin
                           pragma Assert (Row_Y <= Source_Size.Height - 1);
                           pragma Assert (Row_Y * Source_Pitch <= (Source_Size.Height - 1) * Source_Pitch);
                           declare
                              P : constant Pixel := Source (Row_Y * Source_Pitch + Column_X);
                           begin
                              Blue := Blue + Natural (P and 16#FF#);
                              Green := Green + Natural (Shift_Right (P, 8) and 16#FF#);
                              Red := Red + Natural (Shift_Right (P, 16) and 16#FF#);
                           end;
                        end;
                     end loop;
                  end loop;
                  Target (Target_Row + Target_X + X) :=
                    Opaque
                    or Shift_Left (Pixel ((Red + Samples / 2) / Samples), 16)
                    or Shift_Left (Pixel ((Green + Samples / 2) / Samples), 8)
                    or Pixel ((Blue + Samples / 2) / Samples);
               end;
            end loop;
         end;
      end loop;
   end Scale;
end Client_Thumbnails;
