with Ada.Text_IO;
with Interfaces; use Interfaces;
with Servo_Frame_Copy;
procedure Frame_Copy_Tests is
   package Copy renames Servo_Frame_Copy;
   Cases : Natural := 0;
   Sentinel : constant Unsigned_32 := 16#A56B_74C1#;
begin
   if not Copy.Accepts (16, 2, 2, 2, 2, 12, 5, 1) then
      raise Program_Error with "valid padded frame rejected";
   end if;
   pragma Assert (Copy.Accepts (16, 2, 2, 2, 2, 16, 5, 1, 2));
   pragma Assert (not Copy.Accepts (16, 2, 2, 2, 2, 16, 5, 1, 3));
   pragma Assert (not Copy.Accepts (16, 2, 2, 2, 2, 16, 5, 1, Natural'Last));
   -- Foreign edge cases: no pointer is dereferenced until this admission.
   if Copy.Accepts (0, 0, 2, 0, 2, 12, 5, 1) or else
     Copy.Accepts (16, 2, 2, 3, 2, 12, 5, 1) or else
     Copy.Accepts (16, 2, 2, 2, 2, 7, 5, 1) or else
     Copy.Accepts (16, 2, 2, 2, 2, 4, 5, 1) or else
     Copy.Accepts (16, 2, 2, 2, 2, 12, 5, 4) or else
     Copy.Accepts (16, 2, 2, 2, 2, 12, 5, 5) or else
     Copy.Accepts (15, 2, 2, 2, 2, 12, 5, 1) or else
     Copy.Accepts (Unsigned_64'Last, Unsigned_32'Last, Unsigned_32'Last,
       Unsigned_32'Last, Unsigned_32'Last, Natural'Last, Natural'Last, Natural'Last)
   then raise Program_Error with "malformed frame accepted"; end if;
   pragma Assert (Copy.Accepts_BGRA (24, 2, 2, 2, 2, 12, 16, 5, 1, 2));
   pragma Assert (not Copy.Accepts_BGRA (16, 2, 2, 2, 2, 4, 16, 5, 1));
   pragma Assert (not Copy.Accepts_BGRA (24, 2, 2, 2, 2, 10, 16, 5, 1));
   pragma Assert (not Copy.Accepts_BGRA (23, 2, 2, 2, 2, 12, 16, 5, 1));
   pragma Assert (not Copy.Accepts_BGRA (24, 2, 2, 3, 2, 12, 16, 5, 1));
   pragma Assert (not Copy.Accepts_BGRA (0, 0, 0, 0, 0, 4, 16, 5, 1));
   pragma Assert (not Copy.Accepts_BGRA (Unsigned_64'Last,
     Unsigned_32'Last, Unsigned_32'Last, Unsigned_32'Last,
     Unsigned_32'Last, Unsigned_32'Last, Natural'Last,
     Natural'Last, Natural'Last));
   for W in 1 .. 31 loop
      for H in 1 .. 15 loop
         for Padding in 0 .. 5 loop
            for Top in 0 .. 4 loop
               declare
                  Pitch : constant Positive := W + Padding + 2;
                  Source : Copy.Bytes (0 .. W * H * 4 - 1);
                  Target : Copy.Pixels (0 .. (Top + H + 2) * Pitch - 1) := [others => Sentinel];
                  Area : constant Copy.Rectangle := (1, Top, W, H);
                  Source_Pitch : constant Positive := (W + Padding) * 4;
                  BGRA : Copy.Bytes (0 .. Source_Pitch * H - 1) := [others => 16#D3#];
                  Direct : Copy.Pixels (Target'Range) := [others => Sentinel];
                  Expected : Unsigned_32;
                  X, Y, S : Natural;
               begin
                  for I in Source'Range loop Source (I) := Unsigned_8 ((I * 37 + Cases) mod 256); end loop;
                  Copy.Paint (Source, Target, Pitch, Area);
                  for I in Target'Range loop
                     X := I mod Pitch; Y := I / Pitch;
                     if X in 1 .. W and then Y in Top .. Top + H - 1 then
                        S := ((H - 1 - (Y - Top)) * W + X - 1) * 4;
                        Expected := 16#FF00_0000# + Unsigned_32 (Source (S)) * 65536 +
                          Unsigned_32 (Source (S + 1)) * 256 + Unsigned_32 (Source (S + 2));
                     else Expected := Sentinel; end if;
                     if Target (I) /= Expected then raise Program_Error with "pixel/padding mismatch"; end if;
                  end loop;
                  for Row in 0 .. H - 1 loop
                     for Col in 0 .. W - 1 loop
                        declare
                           A : constant Natural := (Row * W + Col) * 4;
                           B : constant Natural := Row * Source_Pitch + Col * 4;
                        begin
                           BGRA (B) := Source (A + 2);
                           BGRA (B + 1) := Source (A + 1);
                           BGRA (B + 2) := Source (A);
                           BGRA (B + 3) := Source (A + 3);
                        end;
                     end loop;
                  end loop;
                  Copy.Paint_BGRA (BGRA, Source_Pitch, Direct, Pitch, Area);
                  for I in Target'Range loop
                     if Target (I) /= Direct (I) then
                        raise Program_Error with "BGRA/RGBA equivalence mismatch";
                     end if;
                  end loop;
                  Cases := Cases + 1;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("SERVO-FRAME-COPY: PASS cases=" & Natural'Image (Cases));
end Frame_Copy_Tests;
