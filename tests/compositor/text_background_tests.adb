with Ada.Text_IO;
with Compositor_Text;
procedure Text_Background_Tests is
   package T renames Compositor_Text;
   package G renames T.G;
   use type G.Logical_Coordinate, G.Pixel_Edge, G.Logical_Rectangle;
   Items, Saved : T.Glyphs;
   type Pixels is array (0 .. 71, 0 .. 79) of Boolean;
   Old_Pixels, New_Pixels : Pixels;
   Screen : G.Output := (80, 72, G.Unrotated, (1, 1), -3, 7);
   Scales : constant array (1 .. 6) of G.UI_Scale :=
     ((1, 1), (5, 4), (3, 2), (7, 4), (2, 1), (3, 1));
   Lengths : constant array (1 .. 4) of T.Count := (1, 2, 31, 32);
   Starts : constant array (1 .. 4) of G.Logical_Point :=
     ((-12, -4), (-3, 7), (11, 13), (65, 55));
   Clips : constant array (1 .. 4) of G.Logical_Rectangle :=
     ((-100, -100, 200, 200), (0, 12, 28, 27),
      (17, 5, 48, 29), (90, 90, 100, 100));
   Cases, Checked_Pixels : Natural := 0;
   procedure Paint (Target : in out Pixels; Area, Clip : G.Logical_Rectangle) is
      Logical : constant G.Logical_Rectangle :=
        (G.Logical_Coordinate'Max (Area.Left, Clip.Left),
         G.Logical_Coordinate'Max (Area.Top, Clip.Top),
         G.Logical_Coordinate'Min (Area.Right, Clip.Right),
         G.Logical_Coordinate'Min (Area.Bottom, Clip.Bottom));
      Physical : constant G.Physical_Rectangle := G.Damage (Screen, Logical);
   begin
      for Y in Integer (Physical.Top) .. Integer (Physical.Bottom) - 1 loop
         for X in Integer (Physical.Left) .. Integer (Physical.Right) - 1 loop
            Target (Y, X) := True;
         end loop;
      end loop;
   end Paint;
begin
   pragma Assert (not T.Can_Join (Items, 0));
   pragma Assert (not T.Can_Join (Items, 1));
   --  Check exact pixel coverage against the previous individual fills,
   --  including logical clipping before outward rounding into each output.
   for Scale of Scales loop
      Screen.Scale := Scale;
      for Rotation in G.Orientation loop
         Screen.Rotation := Rotation;
         for Start of Starts loop
            for Length of Lengths loop
               declare X : G.Logical_Coordinate := Start.X; begin
                  for I in 1 .. Length loop
                     Items (I).Cell := (X, Start.Y, X + G.Logical_Coordinate (I mod 7 + 1), Start.Y + 17);
                     X := Items (I).Cell.Right;
                  end loop;
               end;
               pragma Assert (T.Can_Join (Items, Length));
               for Clip of Clips loop
                  Old_Pixels := (others => (others => False));
                  New_Pixels := (others => (others => False));
                  for I in 1 .. Length loop Paint (Old_Pixels, Items (I).Cell, Clip); end loop;
                  Paint (New_Pixels, T.Background (Items, Length), Clip);
                  pragma Assert (Old_Pixels = New_Pixels);
                  Cases := Cases + 1;
                  Checked_Pixels := Checked_Pixels + 80 * 72;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   --  Malformed cells cannot authorize painting a gap or another text row.
   for I in T.Index loop Items (I).Cell := (G.Logical_Coordinate (I - 1), 0, G.Logical_Coordinate (I), 17); end loop;
   Saved := Items;
   for I in T.Index loop
      Items := Saved; Items (I).Cell.Left := Items (I).Cell.Right;
      pragma Assert (not T.Can_Join (Items, 32));
      Items := Saved; Items (I).Cell.Top := 18;
      pragma Assert (not T.Can_Join (Items, 32));
      Items := Saved; Items (I).Cell.Bottom := 16;
      pragma Assert (not T.Can_Join (Items, 32));
      if I > 1 then
         Items := Saved; Items (I).Cell.Left := Items (I).Cell.Left - 1;
         pragma Assert (not T.Can_Join (Items, 32));
         Items := Saved; Items (I).Cell.Left := Items (I).Cell.Left + 1;
         Items (I).Cell.Right := Items (I).Cell.Right + 1;
         pragma Assert (not T.Can_Join (Items, 32));
      end if;
   end loop;
   Items := Saved; Items (32).Cell := (others => 0);
   pragma Assert (T.Can_Join (Items, 31));
   Items (1).Cell := (G.Logical_Coordinate'First, G.Logical_Coordinate'First,
                     0, G.Logical_Coordinate'Last);
   Items (2).Cell := (0, G.Logical_Coordinate'First,
                     G.Logical_Coordinate'Last, G.Logical_Coordinate'Last);
   pragma Assert (T.Can_Join (Items, 2));
   pragma Assert (T.Background (Items, 2) =
     (G.Logical_Coordinate'First, G.Logical_Coordinate'First,
      G.Logical_Coordinate'Last, G.Logical_Coordinate'Last));
   Ada.Text_IO.Put_Line ("PASS text background:" & Natural'Image (Cases) &
     " scaled/rotated/clipped batches," & Natural'Image (Checked_Pixels) & " exact pixel checks; malformed/extreme cells");
end Text_Background_Tests;
