with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Calendar;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.Fonts;
with Glyph_Pen;
with Compositor_Software_Text;

--  Renders the shared sample spec through the real toolkit text path into
--  memory (1:1, no scaling) and writes a PPM; "bench" times UI text at 4K.
procedure Quality_Sample is
   use type CuBit.Fonts.Face;
   Max_W : constant := 3840;
   Max_H : constant := 2160;
   type Pixels is array (0 .. Max_H - 1, 0 .. Max_W - 1) of Color;
   type Pixels_Access is access Pixels;
   Buffer : constant Pixels_Access := new Pixels;

   function Hex (S : String) return Color is (Color'Value ("16#" & S & "#"));

   procedure Write_PPM (Path : String; W, H : Natural) is
      package SIO renames Ada.Streams.Stream_IO;
      F : SIO.File_Type;
      Header : constant String := "P6" & ASCII.LF & Natural'Image (W) & Natural'Image (H) & ASCII.LF & "255" & ASCII.LF;
   begin
      SIO.Create (F, SIO.Out_File, Path);
      String'Write (SIO.Stream (F), Header);
      for Y in 0 .. H - 1 loop
         for X in 0 .. W - 1 loop
            declare
               P : constant Color := Buffer (Y, X);
            begin
               Character'Write (SIO.Stream (F), Character'Val (Shift_Right (P, 16) and 255));
               Character'Write (SIO.Stream (F), Character'Val (Shift_Right (P, 8) and 255));
               Character'Write (SIO.Stream (F), Character'Val (P and 255));
            end;
         end loop;
      end loop;
      SIO.Close (F);
   end Write_PPM;

   --  Density of the render, n/d (toolkit canvas or Desktop output).
   Scale_N, Scale_D : Positive := 1;
   function Phys (Logical : Natural) return Natural is (Logical * Scale_N / Scale_D);

   --  The Desktop's CPU compositor text (drawUIText -> Draw_Text ->
   --  Compositor_Software_Text): Lay_Out pens, each glyph's cell at the
   --  pen's whole pixel less the raster origin, its fraction as the offset.
   Desktop_Glyphs : Compositor_Software_Text.State;
   procedure Desktop_Text (X, Y : Natural; Text : String; Font : CuBit.Fonts.Face; Fg : Color; W, H : Natural) is
      package T renames Compositor_Software_Text;
      Screen : constant T.G.Output :=
        (T.G.Physical_Extent (W), T.G.Physical_Extent (H), T.G.Unrotated,
         (T.G.Scale_Component (Scale_N), T.G.Scale_Component (Scale_D)), 0, 0);
      Target : T.Software.Pixels (0 .. Max_W * Max_H - 1) with Import, Address => Buffer.all'Address;
      procedure Place (Index : Positive; Ch : Character; Pen : Glyph_Pen.Position) is
         pragma Unreferenced (Index);
         OK : Boolean;
         Left : constant Integer := Pen / Glyph_Pen.Unit - CuBit.Fonts.Origin;
      begin
         T.Paint (Desktop_Glyphs, (CuBit.Fonts.Face'Pos (Font), Character'Pos (Ch), Screen.Scale), Screen,
                  (T.G.Logical_Coordinate (Left), T.G.Logical_Coordinate (Y)), Pen mod Glyph_Pen.Unit,
                  (0, 0, Screen.Width, Screen.Height), Target, Max_W, Fg or 16#FF00_0000#, OK);
         if not OK then raise Program_Error with "desktop glyph"; end if;
      end Place;
      procedure Each is new CuBit.Fonts.Lay_Out (Place);
   begin
      Each (Font, Text, X);
   end Desktop_Text;

   procedure Render (Spec, Output : String; Desktop : Boolean) is
      F : File_Type;
      W, Row, Top, Left, Count : Natural;
      C : Canvas;
   begin
      Open (F, In_File, Spec);
      declare
         package N is new Integer_IO (Natural);
      begin
         N.Get (F, W); N.Get (F, Row); N.Get (F, Top); N.Get (F, Left); N.Get (F, Count);
         Skip_Line (F);
      end;
      C := (addr => Buffer.all'Address, width => W, height => Count * Row, pitch => Max_W * 4,
            densityNumerator => Scale_N, densityDenominator => Scale_D, others => <>);
      for I in 0 .. Count - 1 loop
         declare
            L : constant String := Get_Line (F);
            Face : constant String := L (L'First .. L'First + 3);
            Fg : constant Color := Hex (L (L'First + 5 .. L'First + 10));
            Bg : constant Color := Hex (L (L'First + 12 .. L'First + 17));
            Text : constant String := L (L'First + 19 .. L'Last);
            Y : constant Natural := I * Row;
            Font : constant CuBit.Fonts.Face :=
              (if Face = "mono" then CuBit.Fonts.Monospace
               elsif Face = "semi" then CuBit.Fonts.Sans_Semibold else CuBit.Fonts.Sans);
         begin
            if Desktop or else Font = CuBit.Fonts.Sans_Semibold then
               for YY in Phys (Y) .. Phys (Y + Row) - 1 loop
                  for XX in 0 .. Phys (W) - 1 loop Buffer (YY, XX) := Bg or 16#FF00_0000#; end loop;
               end loop;
               Desktop_Text (Left, Y + Top, Text, Font, Fg, Phys (W), Phys (Count * Row));
            else
               Fill_Rect (C, (0, Y, W, Row), Bg);
               if Face = "mono" then
                  Draw_Code_Text (C, Left, Y + Top, Text, Fg, Bg);
               else
                  Draw_UI_Text (C, Left, Y + Top, Text, Fg, Bg);
               end if;
            end if;
         end;
      end loop;
      Close (F);
      Write_PPM (Output, Phys (W), Phys (Count * Row));
   end Render;

   procedure Bench is
      use Ada.Calendar;
      C : constant Canvas := (addr => Buffer.all'Address, width => Max_W, height => Max_H, pitch => Max_W * 4, others => <>);
      Ink_Line : constant String := "readme-notes-2026.txt    4.2 KiB    Text document    2026-10-18 15:20";
      Blank : constant String (Ink_Line'Range) := [others => ' '];
      Line : constant String := (if Ada.Command_Line.Argument_Count >= 2 then Blank else Ink_Line);
      Rows : constant := Max_H / 18;
      Frames : constant := 40;
      Glyphs : constant Natural := Rows * Line'Length * Frames * 2;
      Start : Time;
      Opaque, Clear : Duration;
   begin
      Fill_Rect (C, (0, 0, Max_W, Max_H), 16#F2F2F0#);
      Draw_UI_Text (C, 0, 0, Line, 16#1E2228#, 16#F2F2F0#);  --  warm caches
      Draw_UI_Text_Transparent (C, 0, 0, Line, 16#1E2228#);
      Start := Clock;
      for F in 1 .. Frames loop
         for R in 0 .. Rows - 1 loop
            Draw_UI_Text (C, 4, R * 18, Line, 16#1E2228#, 16#F2F2F0#);
            Draw_UI_Text (C, 1920 + 4, R * 18, Line, 16#1E2228#, 16#F2F2F0#);
         end loop;
      end loop;
      Opaque := Clock - Start;
      Start := Clock;
      for F in 1 .. Frames loop
         for R in 0 .. Rows - 1 loop
            Draw_UI_Text_Transparent (C, 4, R * 18, Line, 16#1E2228#);
            Draw_UI_Text_Transparent (C, 1920 + 4, R * 18, Line, 16#1E2228#);
         end loop;
      end loop;
      Clear := Clock - Start;
      Put_Line ("4K UI text, " & Natural'Image (Rows * 2) & " rows x" & Natural'Image (Line'Length) & " glyphs per frame");
      Put_Line ("  opaque      : " & Duration'Image (Opaque / Frames * 1000) & " ms/frame, "
                & Natural'Image (Natural (Float (Opaque) * 1.0E9 / Float (Glyphs))) & " ns/glyph");
      Put_Line ("  transparent : " & Duration'Image (Clear / Frames * 1000) & " ms/frame, "
                & Natural'Image (Natural (Float (Clear) * 1.0E9 / Float (Glyphs))) & " ns/glyph");
      declare
         Wide : constant Natural := UI_Text_Width (Line);
         Start2 : constant Time := Clock;
         Sum : Natural := 0;
      begin
         for I in 1 .. 100_000 loop
            Sum := Sum + UI_Text_Width (Line (Line'First .. Line'First + I mod 20 + 40)) mod 7;
         end loop;
         Put_Line ("  measure     : " & Natural'Image (Natural (Float (Clock - Start2) * 1.0E9 / 100_000.0))
                   & " ns/string (" & Natural'Image (Wide) & " px wide," & Natural'Image (Sum mod 2) & ")");
      end;
   end Bench;
   --  The Desktop's CPU compositor text at 4K: the same rows as Bench,
   --  drawn glyph by glyph through Compositor_Software_Text (warm cache).
   procedure Bench_Desktop is
      use Ada.Calendar;
      Line : constant String := "readme-notes-2026.txt    4.2 KiB    Text document    2026-10-18 15:20";
      Rows : constant := Max_H / 18;
      Frames : constant := 20;
      Glyphs : constant Natural := Rows * Line'Length * Frames * 2;
      Start : Time;
      Took : Duration;
   begin
      for Y in 0 .. Max_H - 1 loop
         for X in 0 .. Max_W - 1 loop Buffer (Y, X) := 16#FFF2_F2F0#; end loop;
      end loop;
      Desktop_Text (4, 0, Line, CuBit.Fonts.Sans, 16#1E2228#, Max_W, Max_H);  --  warm the cache
      Start := Clock;
      for F in 1 .. Frames loop
         for R in 0 .. Rows - 1 loop
            Desktop_Text (4, R * 18, Line, CuBit.Fonts.Sans, 16#1E2228#, Max_W, Max_H);
            Desktop_Text (1920 + 4, R * 18, Line, CuBit.Fonts.Sans, 16#1E2228#, Max_W, Max_H);
         end loop;
      end loop;
      Took := Clock - Start;
      Put_Line ("4K Desktop CPU text, " & Natural'Image (Rows * 2) & " rows x" & Natural'Image (Line'Length) & " glyphs per frame");
      Put_Line ("  desktop     : " & Duration'Image (Took / Frames * 1000) & " ms/frame, "
                & Natural'Image (Natural (Float (Took) * 1.0E9 / Float (Glyphs))) & " ns/glyph");
   end Bench_Desktop;

   --  quality_sample SPEC OUT [toolkit|desktop [N D]]
   Mode : constant String := (if Ada.Command_Line.Argument_Count >= 3 then Ada.Command_Line.Argument (3) else "toolkit");
begin
   if Ada.Command_Line.Argument (1) = "bench" then
      --  Several trials: compare the fastest of each (frequency, cache).
      for Trial in 1 .. 3 loop
         Bench;
         Bench_Desktop;
      end loop;
   else
      if Ada.Command_Line.Argument_Count >= 5 then
         Scale_N := Positive'Value (Ada.Command_Line.Argument (4));
         Scale_D := Positive'Value (Ada.Command_Line.Argument (5));
      end if;
      Render (Ada.Command_Line.Argument (1), Ada.Command_Line.Argument (2), Mode = "desktop");
   end if;
end Quality_Sample;
