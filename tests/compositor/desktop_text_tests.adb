with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Desktop_Compositor;
with Compositor_Text;
procedure Desktop_Text_Tests is
   package T renames Compositor_Text;
   procedure Reset with Import, Convention => C, External_Name => "glyph_mock_reset";
   procedure Fault (Value : Unsigned_32) with Import, Convention => C, External_Name => "glyph_mock_fault";
   function Stat (Index : Unsigned_32) return Unsigned_32 with Import, Convention => C, External_Name => "glyph_mock_stat";
   type Pixels is array (Natural range <>) of Unsigned_32;
   Target : aliased Pixels (0 .. 80 * 72 - 1) := (others => 16#0012_3456#);
   Items : T.Glyphs := (others => (65, (0, 0, 12, 17)));
   Mode : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   Drawn, Repaint, Restart, Safe : Boolean;
   procedure Draw is
   begin
      Desktop_Compositor.Draw_Text ((Target'Address, 80, 72, 320, 1), Target'Length * 4,
        (80, 72, T.G.Unrotated, (1, 1), 0, 0), Items, 32, (0, 0, 80, 72), 16#FFED_CBA9#,
        False, Drawn, Repaint, Restart);
   end Draw;
begin
   Reset;
   if Mode in 1 .. 5 then Fault (Unsigned_32 (Mode)); end if;
   Draw;
   if Mode = 0 then
      pragma Assert (Drawn and not Repaint and not Restart and Stat (0) = 1 and Stat (5) = 32);
      Draw;
      pragma Assert (Drawn and Stat (0) = 1 and Stat (5) = 64);
      Desktop_Compositor.Forget_Targets (Safe); pragma Assert (Safe and Stat (4) = 1);
      Draw; pragma Assert (Drawn and Stat (0) = 1 and Stat (5) = 96);
   elsif Mode in 1 .. 2 then
      pragma Assert (not Drawn and not Repaint and not Restart and Stat (3) = 0 and Stat (4) = 0);
      Fault (0); Draw; pragma Assert (Drawn and not Repaint and not Restart and Desktop_Compositor.Software_Text);
   elsif Mode in 3 .. 4 then
      pragma Assert (not Drawn and Repaint and not Restart and Stat (3) = 1 and Stat (4) = 0);
      Fault (0); Draw; pragma Assert (Drawn and not Repaint and not Restart and Stat (3) = 1 and Stat (0) = 1 and Desktop_Compositor.Software_Text);
   elsif Mode = 5 then
      pragma Assert (not Drawn and Repaint and Restart and Stat (4) = 2 and Stat (2) = 0);
      Desktop_Compositor.Forget_Targets (Safe); pragma Assert (not Safe and Stat (2) = 0);
   else
      pragma Assert (Drawn);
      Fault (6); Desktop_Compositor.Forget_Targets (Safe);
      pragma Assert (not Safe and Stat (4) = 2);
      Draw; pragma Assert (not Drawn and Restart);
   end if;
   Ada.Text_IO.Put_Line ("DESKTOP-TEXT: PASS mode" & Mode'Image);
end Desktop_Text_Tests;
