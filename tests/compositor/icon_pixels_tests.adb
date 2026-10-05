with Ada.Text_IO;
with Interfaces;
with Desktop_Icon_Pixels;
with Desktop_Icons;
with Desktop_Window_Icons;
with Compositor_Upload;
procedure Icon_Pixels_Tests is
   package P renames Desktop_Icon_Pixels;
   package U renames Compositor_Upload;
   use type Interfaces.Unsigned_32, P.Pixels, P.Family;
   Sentinel : constant Interfaces.Unsigned_32 := 16#DEADBEEF#;
   Checks : Natural := 0;
   procedure Check (Item : P.Asset) is
      N : constant Positive := P.Size (Item);
      Target : P.Pixels (0 .. 1023) := (others => Sentinel);
      Plan : U.Plan;
      OK, Done : Boolean;
      function Reference (X, Y : Natural) return Interfaces.Unsigned_32 is
        (if Item.Kind = P.Application then Desktop_Icons.Pixels (Item.Icon) (Y * N + X)
         else Desktop_Window_Icons.Pixels (Item.Control) (Y * N + X));
   begin
      for First in 0 .. N - 1 loop
         for Height in 1 .. N - First loop
            for Padding in 0 .. 2 loop
               Target := (others => Sentinel);
               U.Make (N, N, Target'Length * 4, (1, First, N - 1, Height), 12,
                 N - 1 + Padding, U.BGRA8, Plan, OK);
               pragma Assert (OK);
               P.Copy_Chunk (Item, Target, Plan, Done); pragma Assert (Done);
               for I in Target'Range loop
                  if I >= 3 and then (I - 3) / (N - 1 + Padding) < Height and then
                    (I - 3) mod (N - 1 + Padding) < N - 1 then
                     pragma Assert (Target (I) = Reference (1 + (I - 3) mod (N - 1 + Padding),
                       First + (I - 3) / (N - 1 + Padding)));
                  else pragma Assert (Target (I) = Sentinel); end if;
                  Checks := Checks + 1;
               end loop;
            end loop;
         end loop;
      end loop;
      -- Full image/tight rows includes the otherwise omitted first column.
      U.Row_Chunk (N, N, 0, N * N * 4, U.BGRA8, Plan, OK); pragma Assert (OK);
      P.Copy_Chunk (Item, Target, Plan, Done); pragma Assert (Done);
      for Y in 0 .. N - 1 loop
         for X in 0 .. N - 1 loop pragma Assert (Target (Y * N + X) = Reference (X, Y)); end loop;
      end loop;
      Target := (others => Sentinel);
      U.Make (N, N, Target'Length * 4, (0, 0, N, N), 0, 0, U.R8, Plan, OK);
      pragma Assert (OK); P.Copy_Chunk (Item, Target, Plan, Done);
      pragma Assert (not Done and Target = P.Pixels'(0 .. 1023 => Sentinel));
      U.Make (N + 1, N, Target'Length * 4, (0, 0, N, N), 0, 0, U.BGRA8, Plan, OK);
      pragma Assert (OK); P.Copy_Chunk (Item, Target, Plan, Done);
      pragma Assert (not Done and Target = P.Pixels'(0 .. 1023 => Sentinel));
      U.Make (N, N, Target'Length * 4 + 4, (0, 0, N, N), 0, 0, U.BGRA8, Plan, OK);
      pragma Assert (OK); P.Copy_Chunk (Item, Target, Plan, Done);
      pragma Assert (not Done and Target = P.Pixels'(0 .. 1023 => Sentinel));
      U.Make (N, N, 0, (0, 0, N, N), 0, 0, U.BGRA8, Plan, OK);
      pragma Assert (not OK); P.Copy_Chunk (Item, Target, Plan, Done);
      pragma Assert (not Done and Target = P.Pixels'(0 .. 1023 => Sentinel));
   end Check;
begin
   for I in Desktop_Icons.Icon_ID loop Check ((P.Application, I)); end loop;
   for I in Desktop_Window_Icons.Icon_ID loop Check ((P.Window_Control, I)); end loop;
   Ada.Text_IO.Put_Line ("PASS icon staging: all 13 assets, straight-alpha bytes, row slices, offsets, padding guards, invalid plans; checks" & Checks'Image);
end Icon_Pixels_Tests;
