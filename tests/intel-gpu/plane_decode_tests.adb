with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Plane_Decode; use Intel_GPU_Plane_Decode;
with Intel_GPU_Plane_Control;
procedure Plane_Decode_Tests is
   Baseline : constant Sample :=
     (16#8400_0000#, 120, 16#0437_077F#, 0, 16#200000#, 16#200000#);
   S, Changed : Sample;
   D : Decoded;
   Cases : Natural := 0;
   procedure Expect (Value : Sample; Wanted : Status) is
      R : constant Decoded := Decode (Value, Value, 8 * 1024 * 1024);
   begin
      pragma Assert (R.State = Wanted);
      pragma Assert (R.Memory.Valid = (Wanted = Linear_Ready));
      Cases := Cases + 1;
   end Expect;
begin
   declare
      use Intel_GPU_Plane_Control;
      C : Control;
   begin
      pragma Assert (Control'Size = 32);
      for Bit in 0 .. 31 loop
         C := From_Word (Shift_Left (1, Bit));
         pragma Assert (To_Word (C) = Shift_Left (1, Bit));
      end loop;
      C := (others => <>); C.Allow_Update_Disable := 1;
      pragma Assert (To_Word (C) = 8);
      C := (others => <>); C.Horizontal_Flip := 1;
      pragma Assert (To_Word (C) = 16#100#);
      C := From_Word (16#8400_0008#);
      pragma Assert (C.Enabled = 1 and C.Pixel_Format = 8 and
        C.Allow_Update_Disable = 1 and C.Tiling = 0 and C.Rotation = 0);
      pragma Assert (To_Word (From_Word (Unsigned_32'Last)) = Unsigned_32'Last);
   end;
   D := Decode (Baseline, Baseline, 8 * 1024 * 1024);
   pragma Assert (D.State = Linear_Ready and D.Memory.First = 16#200000#);
   pragma Assert (D.Memory.Bytes = 8_294_400); -- 1920*1080*4, 2025 pages
   -- Every control bit is classified; the accepted optional fields do not
   -- change address geometry.
   for Bit in 0 .. 31 loop
      S := Baseline;
      S.Control := S.Control xor Shift_Left (1, Bit);
      Expect (S, (if Bit = 31 then Disabled elsif Bit in 3 | 20 then Linear_Ready
                  else Unsupported));
   end loop;
   for Field in 0 .. 5 loop
      S := Baseline;
      case Field is
         when 0 => S.Control := Unsigned_32'Last;
         when 1 => S.Stride := Unsigned_32'Last;
         when 2 => S.Size := Unsigned_32'Last;
         when 3 => S.Offset := Unsigned_32'Last;
         when 4 => S.Surface := Unsigned_32'Last;
         when others => S.Live_Surface := Unsigned_32'Last;
      end case;
      Expect (S, Invalid_Read);
      D := Decode (Baseline, S, 8 * 1024 * 1024);
      pragma Assert (D.State = Changing and not D.Memory.Valid);
      Changed := Baseline;
      case Field is
         when 0 => Changed.Control := Changed.Control xor 16#100000#;
         when 1 => Changed.Stride := Changed.Stride + 1;
         when 2 => Changed.Size := Changed.Size + 1;
         when 3 => Changed.Offset := 1;
         when 4 => Changed.Surface := Changed.Surface + 4096;
         when others => Changed.Live_Surface := Changed.Live_Surface + 4096;
      end case;
      D := Decode (Baseline, Changed, 8 * 1024 * 1024);
      pragma Assert (D.State = Changing and not D.Memory.Valid);
   end loop;
   S := Baseline; S.Control := 16#8400_0008#;
   S.Surface := 0; S.Live_Surface := 0;
   D := Decode (S, S, 8 * 1024 * 1024);
   pragma Assert (D.State = Linear_Ready and D.Memory.First = 0 and
                    D.Memory.Bytes = 8_294_400);
   S.Control := 16#8410_0008#; Expect (S, Linear_Ready);
   S := Baseline; S.Live_Surface := 16#300000#; Expect (S, Changing);
   for Bit in 0 .. 11 loop
      S := Baseline; S.Surface := S.Surface or Shift_Left (1, Bit);
      Expect (S, (if Bit = 3 then Linear_Ready else Unsupported));
      S := Baseline; S.Live_Surface := S.Live_Surface or Shift_Left (1, Bit);
      Expect (S, Unsupported);
   end loop;
   S := Baseline; S.Surface := S.Surface or 8;
   D := Decode (S, S, 8 * 1024 * 1024);
   pragma Assert (D.Memory.First = 16#200000# and D.Memory.Bytes = 8_294_400);
   D := Decode (Baseline, S, 8 * 1024 * 1024);
   pragma Assert (D.State = Changing and not D.Memory.Valid);
   for Bit in 0 .. 31 loop
      if Bit in 13 .. 15 | 29 .. 31 then
         S := Baseline; S.Size := S.Size or Shift_Left (1, Bit);
         Expect (S, Unsupported);
         S := Baseline; S.Offset := Shift_Left (1, Bit);
         Expect (S, Unsupported);
      end if;
   end loop;
   S := Baseline; S.Stride := 0; Expect (S, Invalid_Geometry);
   S := Baseline; S.Stride := 119; Expect (S, Invalid_Geometry);
   S := Baseline; S.Stride := 16#1000#; Expect (S, Unsupported);
   S := Baseline; S.Offset := 1; Expect (S, Invalid_Geometry);
   S := Baseline; S.Surface := 1; Expect (S, Unsupported);
   S := Baseline; S.Live_Surface := 1; Expect (S, Unsupported);
   S := Baseline; S.Size := 0; S.Stride := 1; S.Offset := 16#0001_000F#;
   D := Decode (S, S, 8 * 1024 * 1024);
   pragma Assert (D.State = Linear_Ready and D.Memory.Bytes = 4096);
   S := Baseline; S.Surface := 16#FFFFF000#; S.Live_Surface := S.Surface;
   Expect (S, Invalid_Geometry);
   D := Decode (Baseline, Baseline, 0);
   pragma Assert (D.State = Invalid_Geometry);
   Ada.Text_IO.Put_Line ("Plane decode PASS:" & Cases'Image &
     " rejection/format cases plus geometry and six-field transition checks");
end Plane_Decode_Tests;
