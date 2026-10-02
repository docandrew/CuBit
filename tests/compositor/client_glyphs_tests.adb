with Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with Client_Glyphs;
with Compositor_Glyph_FFI;
procedure Client_Glyphs_Tests is
   package G renames Client_Glyphs;
   use type System.Address;
   S : G.State;
   Held, V : G.View;
   Key : G.C.Key;
   OK : Boolean;
   Held_Address : System.Address;
   Checks : Natural := 0;
   type Bytes is array (Natural range <>) of Unsigned_8;
   Reference : aliased Bytes (0 .. G.L.Maximum_Bytes - 1);
   Advance : Natural;
   function Hash (V : G.View) return Unsigned_64 is
      L : constant G.L.Layout := G.Raster (V);
      Mask : Bytes (0 .. L.Bytes - 1) with Import, Address => G.Pixels (V);
      H : Unsigned_64 := 0;
   begin
      for Y in 0 .. L.Height - 1 loop
         for X in 0 .. L.Width - 1 loop H := H * 31 + Unsigned_64 (Mask (Y * L.Pitch + X)); end loop;
      end loop;
      return H;
   end Hash;
   Held_Hash : Unsigned_64;
begin
   Key := (0, Character'Pos ('A'), (5, 4));
   G.Read (S, Key, Held); pragma Assert (G.Ready (Held));
   Held_Address := G.Pixels (Held); Held_Hash := Hash (Held);
   pragma Assert (Held_Hash /= 0);
   for Pass in 1 .. 3 loop
      for Face in 0 .. 1 loop
         for Code in 32 .. 126 loop
            Key := (Face, Code, (if Pass = 1 then (3, 2) elsif Pass = 2 then (2, 1) else (16, 1)));
            G.Read (S, Key, V); pragma Assert (G.Ready (V));
            declare
               L : constant G.L.Layout := G.Raster (V);
               Mask : Bytes (0 .. L.Bytes - 1) with Import, Address => G.Pixels (V);
               Address : constant System.Address := G.Pixels (V);
            begin
               Compositor_Glyph_FFI.Rasterize (Unsigned_32 (Face), Unsigned_32 (Code), L,
                 Reference'Address, Unsigned_64 (Reference'Length), Advance, OK);
               pragma Assert (OK);
               for Y in 0 .. L.Height - 1 loop
                  for X in 0 .. L.Width - 1 loop
                     pragma Assert (Mask (Y * L.Pitch + X) = Reference (Y * L.Pitch + X));
                  end loop;
               end loop;
               G.Finish (S, V);
               G.Read (S, Key, V);
               pragma Assert (G.Ready (V) and G.Pixels (V) = Address);
            end;
            pragma Assert (G.Charged (S) <= G.Budget and G.Readers (S) = 2);
            pragma Assert (G.Pixels (Held) = Held_Address and Hash (Held) = Held_Hash);
            G.Finish (S, V); Checks := Checks + 1;
         end loop;
      end loop;
   end loop;
   declare
      Other : G.State;
      Other_View : G.View;
   begin
      G.Read (Other, (0, 65, (5, 4)), Other_View);
      pragma Assert (G.Ready (Other_View) and not G.Belongs (Other, Held));
      G.Finish (Other, Held);
      pragma Assert (G.Ready (Held) and G.Readers (Other) = 1 and G.Readers (S) = 1);
      G.Finish (Other, Other_View); G.Close (Other, OK); pragma Assert (OK);
   end;
   -- Saturating the fixed reader table cannot invalidate outstanding views.
   declare Views : array (1 .. 32) of G.View; begin
      for I in 1 .. 31 loop G.Read (S, (0, 65, (5, 4)), Views (I)); pragma Assert (G.Ready (Views (I))); end loop;
      G.Read (S, (0, 65, (5, 4)), Views (32)); pragma Assert (not G.Ready (Views (32)));
      pragma Assert (G.Readers (S) = 32 and Hash (Held) = Held_Hash);
      G.Close (S, OK); pragma Assert (not OK and G.Charged (S) > 0);
      G.Read (S, (0, 65, (5, 4)), Views (32)); pragma Assert (not G.Ready (Views (32)));
      for I in 1 .. 31 loop G.Finish (S, Views (I)); end loop;
   end;
   G.Close (S, OK); pragma Assert (not OK and Hash (Held) = Held_Hash);
   G.Finish (S, Held); G.Finish (S, Held);
   G.Close (S, OK); pragma Assert (OK and G.Charged (S) = 0 and G.Readers (S) = 0);
   Ada.Text_IO.Put_Line ("PASS client glyph cache: masks" & Natural'Image (Checks) &
     ", warm reuse, pinned stability, reader exhaustion and delayed close");
end Client_Glyphs_Tests;
