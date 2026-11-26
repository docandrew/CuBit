------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Desktop_Icon_Pixels;

package body Desktop_Canvas_Icons is
   procedure Draw (C : CuBit.UI.Canvas; Id : Desktop_Icons.Icon_ID; X, Y : Natural; Bg : CuBit.UI.Color) is
      Edge : Desktop_Icons.Edge_Index := Desktop_Icons.Edge_Index'First;
   begin
      for E in Desktop_Icons.Edge_Index loop
         if Desktop_Icons.Edges (E) = Desktop_Icons.ICON_SIZE then
            Edge := E;
         end if;
      end loop;
      declare
         Item : constant Desktop_Icon_Pixels.Asset := (Desktop_Icon_Pixels.Application, Id, Edge);
         Size : constant Positive := Desktop_Icons.Edges (Edge);
      begin
         for YY in 0 .. Size - 1 loop
            for XX in 0 .. Size - 1 loop
               declare
                  P : constant Unsigned_32 := Desktop_Icon_Pixels.Pixel (Item, XX, YY);
                  A : constant Unsigned_32 := Shift_Right (P, 24);
                  function Mix (Shift : Natural) return Unsigned_32 is
                    ((((Shift_Right (P, Shift) and 16#FF#) * A
                       + (Shift_Right (Bg, Shift) and 16#FF#) * (255 - A)) / 255) and 16#FF#);
               begin
                  if A /= 0 then
                     CuBit.UI.Set_Pixel (C, X + XX, Y + YY,
                                         Shift_Left (Mix (16), 16) or Shift_Left (Mix (8), 8) or Mix (0));
                  end if;
               end;
            end loop;
         end loop;
      end;
   end Draw;
end Desktop_Canvas_Icons;
