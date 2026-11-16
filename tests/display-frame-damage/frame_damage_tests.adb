--  LAT-002 multi-rect submit (tests/display-frame-damage): the frame's
--  damage record round-trips and refuses what display.svc must not trust,
--  and copying a frame's boxes touches only those boxes.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol;
with CuBit.Display_Frame_Damage; use CuBit.Display_Frame_Damage;
with CuBit.Display_Box_Copy;
procedure Frame_Damage_Tests is
   package D renames CuBit.Desktop_Protocol;
   use type D.Rectangle;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Ada.Text_IO.Put_Line ("FRAME-DAMAGE: FAIL " & What);
         raise Program_Error;
      end if;
   end Check;
   Area : constant D.Rectangle := (10, 20, 900, 700);
   Caret : constant D.Rectangle := (700, 400, 2, 20);
   Canvas : constant D.Rectangle := (10, 20, 300, 200);
   Item : Damage;
   W : Word_Array;
   R : Decoding;
begin
   Item.Count := 2;
   Item.Boxes (1) := Canvas;
   Item.Boxes (2) := Caret;
   W := Encode (42, Item);
   R := Decode (W, 42, Area);
   Check (R.Valid and then R.Value.Count = 2 and then R.Value.Boxes (1) = Canvas and then
          R.Value.Boxes (2) = Caret, "round trip");
   Check (not Decode (W, 43, Area).Valid, "a record of another frame is refused");
   declare
      Bad : Word_Array := W;
   begin
      Bad (0) := 0;
      Check (not Decode (Bad, 42, Area).Valid, "no magic, no record");
      Bad := W; Bad (2) := 0;
      Check (not Decode (Bad, 42, Area).Valid, "no boxes is refused");
      Bad := W; Bad (2) := Maximum_Boxes + 1;
      Check (not Decode (Bad, 42, Area).Valid, "more than the ceiling is refused");
      Bad := W; Bad (Header_Words + 1) := Pack ((905, 400, 10, 20));
      Check (not Decode (Bad, 42, Area).Valid, "a box leaving the area is refused");
      Bad := W; Bad (Header_Words) := Pack ((10, 20, 0, 20));
      Check (not Decode (Bad, 42, Area).Valid, "an empty box is refused");
   end;
   --  Copying the boxes touches only them.
   declare
      Width : constant := 64;
      Height : constant := 48;
      Pitch : constant := Width * 4 + 32;   --  padded rows
      type Image is array (0 .. Pitch / 4 * Height - 1) of Unsigned_32;
      Source, Target : Image;
      Boxes : Damage;
      Copied : Unsigned_64;
      function Inside (X, Y : Natural; B : D.Rectangle) return Boolean is
        (X >= Natural (B.X) and then X < Natural (B.X) + Natural (B.Width) and then
         Y >= Natural (B.Y) and then Y < Natural (B.Y) + Natural (B.Height));
      Wrong : Natural := 0;
   begin
      for I in Source'Range loop
         Source (I) := Unsigned_32 (I) + 1;
         Target (I) := 16#DEAD_BEEF#;
      end loop;
      Boxes.Count := 3;
      Boxes.Boxes (1) := (2, 3, 10, 5);
      Boxes.Boxes (2) := (50, 40, 14, 8);
      Boxes.Boxes (3) := (0, 10, Width, 2);   --  a whole-row span
      CuBit.Display_Box_Copy.Copy_Boxes (Target'Address, Pitch, Source'Address, Pitch, Boxes, Copied);
      for Y in 0 .. Height - 1 loop
         for X in 0 .. Pitch / 4 - 1 loop
            declare
               I : constant Natural := Y * (Pitch / 4) + X;
               Boxed : constant Boolean := X < Width and then
                 (for some K in 1 .. Boxes.Count => Inside (X, Y, Boxes.Boxes (K)));
            begin
               if (Boxed and then Target (I) /= Source (I)) or else
                 (not Boxed and then Target (I) /= 16#DEAD_BEEF#)
               then
                  Wrong := Wrong + 1;
               end if;
            end;
         end loop;
      end loop;
      Check (Wrong = 0, "only the boxes' pixels change, all of them");
      Check (Copied = (10 * 5 + 14 * 8 + Width * 2) * 4, "the bytes copied are the boxes'");
   end;
   Ada.Text_IO.Put_Line ("FRAME-DAMAGE: PASS checks=" & Natural'Image (Checks));
end Frame_Damage_Tests;
