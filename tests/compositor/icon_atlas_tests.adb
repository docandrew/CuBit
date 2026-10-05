with Ada.Text_IO;
with Interfaces;
with Desktop_Icons;
with Desktop_Cursors;
with Desktop_Window_Icons;
with Desktop_Icon_Pixels.Atlases;
with Desktop_Icon_Mapping;
with Compositor_Upload;
procedure Icon_Atlas_Tests is
   package P renames Desktop_Icon_Pixels;
   package A renames P.Atlases;
   package U renames Compositor_Upload;
   use type Interfaces.Unsigned_32, P.Pixels, P.Family;
   Sentinel : constant Interfaces.Unsigned_32 := 16#DEADBEEF#;
   Checks : Natural := 0;
   procedure Check_Region (Item : P.Asset) is
      R : constant A.R.Rectangle := A.Region (Item);
   begin
      pragma Assert (A.R.Valid (R) and R.Width = Interfaces.Unsigned_32 (P.Size (Item)) and
        R.Image_Width = Interfaces.Unsigned_32 (A.Width (Item.Kind)) and
        R.Image_Height = Interfaces.Unsigned_32 (A.Height (Item.Kind)));
      for Y in 0 .. P.Size (Item) - 1 loop
         for X in 0 .. P.Size (Item) - 1 loop
            pragma Assert (A.Pixel (Item.Kind, X + Natural (R.X), Y + Natural (R.Y)) = P.Pixel (Item, X, Y));
         end loop;
      end loop;
   end Check_Region;
   procedure Check (Kind : P.Family) is
      Target : P.Pixels (0 .. 8193) := (others => Sentinel);
      W : constant Positive := A.Width (Kind);
      H : constant Positive := A.Height (Kind);
      Plan : U.Plan;
      OK, Done : Boolean;
      function Reference (X, Y : Natural) return Interfaces.Unsigned_32 is
         Rows : constant array (Desktop_Cursors.Cursor_ID) of Natural := (45, 73, 97, 115, 140);
      begin
         if Kind = P.Application then
            return Desktop_Icons.Pixels (Desktop_Icons.Icon_ID'Val (Y / 24)) ((Y mod 24) * 24 + X);
         elsif Y < 45 then
            return (if X < 9 then Desktop_Window_Icons.Pixels (Desktop_Window_Icons.Icon_ID'Val (Y / 9)) ((Y mod 9) * 9 + X) else 0);
         else
            for C in Desktop_Cursors.Cursor_ID loop
               declare M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (C); begin
                  if Y >= Rows (C) and then Y < Rows (C) + M.Height and then X < M.Width then
                     return Desktop_Cursors.Pixels (M.Offset + (Y - Rows (C)) * M.Width + X);
                  end if;
               end;
            end loop;
            return 0;
         end if;
      end Reference;
   begin
      for First in 0 .. H - 1 loop
         for Padding in 0 .. 2 loop
            declare
               Rows : constant Positive := Positive'Min (21, H - First);
               Stride : constant Positive := W + Padding;
            begin
               Target := (others => Sentinel);
               U.Make (W, H, 8192 * 4, (0, First, W, Rows), 12, Stride, U.BGRA8, Plan, OK);
               pragma Assert (OK);
               P.Atlases.Copy_Chunk (Kind, Target, Plan, Done); pragma Assert (Done);
               -- Typed target begins at word zero; only region data changes.
               for I in Target'Range loop
                  if I >= 3 and then (I - 3) / Stride < Rows and then (I - 3) mod Stride < W then
                     pragma Assert (Target (I) = Reference ((I - 3) mod Stride, First + (I - 3) / Stride));
                  else pragma Assert (Target (I) = Sentinel); end if;
                  Checks := Checks + 1;
               end loop;
               Target := (others => Sentinel);
               Desktop_Icon_Mapping.Copy_Atlas (Kind, Target (1)'Address, 8192 * 4, Plan, Done);
               pragma Assert (Done);
               for I in Target'Range loop
                  if I >= 4 and then (I - 4) / Stride < Rows and then (I - 4) mod Stride < W then
                     pragma Assert (Target (I) = Reference ((I - 4) mod Stride, First + (I - 4) / Stride));
                  else pragma Assert (Target (I) = Sentinel); end if;
                  Checks := Checks + 1;
               end loop;
            end;
         end loop;
      end loop;
      Target := (others => Sentinel);
      U.Make (W, W, 8192 * 4, (0, 0, W, W), 0, 0, U.BGRA8, Plan, OK);
      pragma Assert (OK); A.Copy_Chunk (Kind, Target, Plan, Done);
      pragma Assert (not Done and Target = P.Pixels'(0 .. 8193 => Sentinel));
   end Check;
begin
   pragma Assert (A.Width (P.Application) = 24 and A.Height (P.Application) = 192);
   pragma Assert (A.Width (P.Window_Control) = 25 and A.Height (P.Window_Control) = 161);
   for C in Desktop_Cursors.Cursor_ID loop
      declare
         R : constant A.R.Rectangle := A.Cursor_Region (C);
         M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (C);
      begin
         pragma Assert (A.R.Valid (R) and Natural (R.Width) = M.Width and Natural (R.Height) = M.Height);
         for Y in 0 .. M.Height - 1 loop
            for X in 0 .. M.Width - 1 loop
               pragma Assert (A.Pixel (P.Window_Control, X, Natural (R.Y) + Y) = Desktop_Cursors.Pixels (M.Offset + Y * M.Width + X));
            end loop;
         end loop;
      end;
   end loop;
   for I in Desktop_Icons.Icon_ID loop Check_Region ((P.Application, I)); end loop;
   for I in Desktop_Window_Icons.Icon_ID loop Check_Region ((P.Window_Control, I)); end loop;
   for Kind in P.Family loop Check (Kind); end loop;
   Ada.Text_IO.Put_Line ("PASS two packed icon atlases: all 13 source windows, cross-icon row chunks, direct mappings, offsets/padding/guards; pixel checks" & Checks'Image);
end Icon_Atlas_Tests;
