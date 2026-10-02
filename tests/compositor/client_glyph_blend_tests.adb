with Ada.Text_IO; with Interfaces; with Client_Glyph_Blend;
procedure Client_Glyph_Blend_Tests is
   package B renames Client_Glyph_Blend;
   use type B.Word, B.Byte;
   Count : Natural := 0;
   function Oracle (F, V : B.Word; A : B.Byte) return B.Word is
      use Interfaces;
      Result : B.Word := 0;
      Weight : constant Natural := Natural (A);
   begin
      if A = 0 then return V; elsif A = 255 then return F; end if;
      for C in 0 .. 2 loop
         Result := Result or Shift_Left
           (B.Word ((Natural (Shift_Right (F, C * 8) and 255) * Weight +
             Natural (Shift_Right (V, C * 8) and 255) * (255 - Weight) + 127) / 255), C * 8);
      end loop;
      return Result;
   end Oracle;
begin
   for F in B.Channel_Value loop
      for V in B.Channel_Value loop
         for A in B.Channel_Value loop
            pragma Assert (B.Channel (F, V, A) = (F * A + V * (255 - A) + 127) / 255);
         end loop;
      end loop;
   end loop;
   for Pitch in 1 .. 12 loop
      for Rows in 1 .. 7 loop
         for Tail in 1 .. Pitch loop
            declare
               Size : constant Positive := (Rows - 1) * Pitch + Tail;
               Mask : B.Bytes (0 .. 159);
               Target, Expected : B.Pixels (0 .. Size - 1);
               Src : B.Rectangle;
               Dest : B.Rectangle;
               Tint : constant B.Word := 16#FA314F97#;
            begin
               for I in Mask'Range loop Mask (I) := B.Byte (I mod 256); end loop;
               Mask (0) := 0; Mask (1) := 255;
               for Y in 0 .. Rows - 1 loop
                  for X in 0 .. Pitch - 1 loop
                     for W in 1 .. Pitch - X loop
                        Dest := (X, Y, W, Rows - Y);
                        if B.Fits (Size, Pitch, Dest) then
                           Src := (1, 1, W, Rows - Y);
                           for I in Target'Range loop Target (I) := 16#AB204060# + B.Word (I); end loop;
                           Expected := Target;
                           for DY in 0 .. Dest.Height - 1 loop
                              for DX in 0 .. Dest.Width - 1 loop
                                 declare
                                    T : constant Natural := (Y + DY) * Pitch + X + DX;
                                    S : constant Natural := (1 + DY) * 16 + 1 + DX;
                                 begin Expected (T) := Oracle (Tint, Expected (T), Mask (S)); end;
                              end loop;
                           end loop;
                           B.Paint (Mask, Target, 16, Pitch, Src, Dest, Tint);
                           for I in Target'Range loop pragma Assert (Target (I) = Expected (I)); end loop;
                           Count := Count + 1;
                        end if;
                     end loop;
                  end loop;
               end loop;
            end;
         end loop;
      end loop;
   end loop;
   pragma Assert (not B.Fits (Natural'Last, 1, (0, Natural'Last, 1, 2)));
   pragma Assert (B.Fits (Natural'Last, Natural'Last, (0, 0, Natural'Last, 1)));
   pragma Assert (not B.Fits (0, 1, (0, 0, 1, 1)));
   for A in B.Byte loop
      pragma Assert (B.Mix (16#FE8095FF#, 16#BE126534#, A) = Oracle (16#FE8095FF#, 16#BE126534#, A));
   end loop;
   Ada.Text_IO.Put_Line ("PASS exhaustive 16777216 channel combinations and" & Count'Image & " clipped/padded/partial-row rectangles");
end Client_Glyph_Blend_Tests;
