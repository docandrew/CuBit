------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Task_Cards with SPARK_Mode is

   function Plan (C : Content) return Layout is
      Result : Layout;
      Y : Natural := Stripe_Height;
      Inner : constant := Width - 2 * Padding;
   begin
      Result.Header := (Padding, Y, Inner, Header_Height);
      Y := Y + Header_Height;
      if C.Note then
         Result.Note := (Padding, Y, Inner, Note_Height);
         Y := Y + Note_Height;
      else
         Result.Note := (Padding, Y, Inner, 0);
      end if;
      if C.Has_Thumbnail then
         --  Centred across the inner width.
         Result.Thumbnail :=
           (Padding + (Inner - C.Thumbnail.Width) / 2, Y, C.Thumbnail.Width, C.Thumbnail.Height);
         Y := Y + C.Thumbnail.Height + Thumbnail_Gap;
      else
         Result.Thumbnail := (Padding, Y, Inner, 0);
      end if;
      pragma Assert (Y <= Stripe_Height + Header_Height + Note_Height
                       + Thumbnail_Height_Bound + Thumbnail_Gap);
      Result.Shown := Natural'Min (C.Windows, Maximum_Rows);
      for R in 1 .. Result.Shown loop
         Result.Rows (R) := (Padding, Y, Inner, Row_Height);
         Y := Y + Row_Height + Row_Gap;
         pragma Loop_Invariant
           (Y = Stripe_Height + Header_Height + (if C.Note then Note_Height else 0)
                + (if C.Has_Thumbnail then C.Thumbnail.Height + Thumbnail_Gap else 0)
                + R * (Row_Height + Row_Gap));
         pragma Loop_Invariant
           (for all Q in 1 .. R =>
              Result.Rows (Q) =
                (Padding,
                 Stripe_Height + Header_Height + (if C.Note then Note_Height else 0)
                   + (if C.Has_Thumbnail then C.Thumbnail.Height + Thumbnail_Gap else 0)
                   + (Q - 1) * (Row_Height + Row_Gap),
                 Inner, Row_Height));
      end loop;
      if C.Windows > Maximum_Rows then
         Result.More := (Padding, Y, Inner, Row_Height);
         Y := Y + Row_Height + Row_Gap;
      else
         Result.More := (Padding, Y, Inner, 0);
      end if;
      Result.Height := Y + Padding;
      return Result;
   end Plan;

   function Place
     (Button_X, Screen_Left, Screen_Right, Bar_Top : Coordinate; Height : Coordinate)
      return Placement
   is
   begin
      if Screen_Right < Screen_Left + Width or else Bar_Top < Height + Margin then
         return (0, 0, False);
      end if;
      return (X => Natural'Max (Screen_Left, Natural'Min (Button_X, Screen_Right - Width)),
              Y => Bar_Top - Margin - Height,
              Fits => True);
   end Place;
end Desktop_Task_Cards;
