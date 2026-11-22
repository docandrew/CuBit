------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Remote_PNG (screenshots, REMOTE-001): encodes synthetic BGRA frames
--  shaped like screens (flat panels, gradients, a noisy photo strip, thin
--  text-like strokes, sizes not a multiple of anything) to build/png-N.png
--  with the expected RGB beside them (build/png-N.rgb); check_png.py
--  decodes each PNG independently (zlib, chunk CRCs) and compares.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with Remote_PNG;
with Remote_Wire;

procedure PNG_Tests is
   package SIO renames Ada.Streams.Stream_IO;

   type Frame is array (Natural range <>) of Unsigned_8;
   type Frame_Access is access Frame;

   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Unsigned_8 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Unsigned_8 (Seed and 16#FF#);
   end Next;

   procedure One (Number, Width, Height : Positive) is
      Pixels : constant Frame_Access := new Frame (0 .. Width * Height * 4 - 1);
      Out_File, RGB_File : SIO.File_Type;
      Name : constant String := "build/png-" & Number'Image (2 .. Number'Image'Last);
      Written : Natural := 0;

      function Source (Index : Natural) return Unsigned_8 is (Pixels (Index));
      procedure Emit (Data : Remote_Wire.Byte_Array) is
         Bytes : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Data'Length));
      begin
         for I in Data'Range loop
            Bytes (Ada.Streams.Stream_Element_Offset (I - Data'First + 1)) := Ada.Streams.Stream_Element (Data (I));
         end loop;
         SIO.Write (Out_File, Bytes);
         Written := Written + Data'Length;
      end Emit;
      procedure Encode is new Remote_PNG.Encode (Source, Emit);
   begin
      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            declare
               At_Index : constant Natural := (Y * Width + X) * 4;
               R, G, B : Unsigned_8;
            begin
               if Y < Height / 4 then           --  a flat panel with a stroke
                  R := 16#20#; G := 16#30#; B := 16#40#;
                  if X mod 17 = 3 and then Y mod 5 /= 0 then R := 255; G := 255; B := 255; end if;
               elsif Y < Height / 2 then        --  a gradient
                  R := Unsigned_8 (X mod 256); G := Unsigned_8 (Y mod 256); B := Unsigned_8 ((X + Y) mod 256);
               elsif Y < 3 * Height / 4 then    --  noise (incompressible)
                  R := Next; G := Next; B := Next;
               else                              --  repeated tiles
                  R := Unsigned_8 ((X / 8) mod 2 * 200); G := 90; B := Unsigned_8 ((Y / 8) mod 2 * 200);
               end if;
               Pixels (At_Index) := B;
               Pixels (At_Index + 1) := G;
               Pixels (At_Index + 2) := R;
               Pixels (At_Index + 3) := 16#FF#;
            end;
         end loop;
      end loop;
      SIO.Create (Out_File, SIO.Out_File, Name & ".png");
      Encode (Width, Height);
      SIO.Close (Out_File);
      SIO.Create (RGB_File, SIO.Out_File, Name & ".rgb");
      for P in 0 .. Width * Height - 1 loop
         SIO.Write (RGB_File, [Ada.Streams.Stream_Element (Pixels (P * 4 + 2)),
                               Ada.Streams.Stream_Element (Pixels (P * 4 + 1)),
                               Ada.Streams.Stream_Element (Pixels (P * 4))]);
      end loop;
      SIO.Close (RGB_File);
      Put_Line ("png" & Number'Image & ":" & Width'Image & "x" & Height'Image & " ->" & Written'Image &
                " bytes (raw" & Natural'Image (Width * Height * 3) & ")");
   end One;
begin
   --  CRC-32 check value ("123456789" -> CBF43926).
   if Remote_PNG.CRC32 ([49, 50, 51, 52, 53, 54, 55, 56, 57]) /= 16#CBF4_3926# then
      Put_Line ("FAIL: CRC-32 check value");
      raise Program_Error;
   end if;
   One (1, 1, 1);
   One (2, 3, 2);
   One (3, 257, 129);
   One (4, 1024, 768);
   One (5, 1999, 37);
   Put_Line ("PASS: PNG frames written (tests/ccl-remote/check_png.py checks them)");
end PNG_Tests;
