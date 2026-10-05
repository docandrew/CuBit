with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Execution_Time;
with Ada.Real_Time;
with Interfaces; use Interfaces;
with CuBit.Appearance;
with Desktop_Wallpaper;
with Wallpaper_Assets;
procedure Wallpaper_Benchmark is
   package A renames CuBit.Appearance;
   use type Ada.Execution_Time.CPU_Time;
   W : constant := 800;
   H : constant := 600;
   Runs : constant := 12;
   type Pixels is array (Natural range <>) of Unsigned_32 with Convention => C;
   Buffer : aliased Pixels (0 .. W * H - 1);
   Mode : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   Style : constant A.Preferences :=
     (A.Alloy_Dark, (if Mode < 3 then A.Wallpaper else A.Cubie), A.Placement'Val (Mode mod 3));
   Start, Stop : Ada.Execution_Time.CPU_Time;
   Hash : Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
begin
   for I in Wallpaper_Assets.Wallpaper'Range loop
      Wallpaper_Assets.Wallpaper (I) := 16#FF00_0000# or Unsigned_32 ((I * 31) mod 16#100_0000#);
   end loop;
   for I in Wallpaper_Assets.Cubie'Range loop
      Wallpaper_Assets.Cubie (I) := 16#FF00_0000# or Unsigned_32 ((I * 47) mod 16#100_0000#);
   end loop;
   for Warmup in 1 .. 2 loop
      Desktop_Wallpaper.Paint (Buffer'Address, W, H, W * 4, 0, 0, W, H, Style);
   end loop;
   Start := Ada.Execution_Time.Clock;
   for Iteration in 1 .. Runs loop
      Desktop_Wallpaper.Paint (Buffer'Address, W, H, W * 4, 0, 0, W, H, Style);
   end loop;
   Stop := Ada.Execution_Time.Clock;
   for Pixel of Buffer loop Hash := (Hash xor Unsigned_64 (Pixel)) * 16#100_0000_01B3#; end loop;
   Ada.Text_IO.Put_Line ("BENCH" & Mode'Image & Runs'Image &
     Duration'Image (Ada.Real_Time.To_Duration (Stop - Start)) & Hash'Image);
end Wallpaper_Benchmark;
