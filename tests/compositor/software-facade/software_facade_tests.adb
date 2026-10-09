with Compositor_Damage;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Desktop_Compositor;
with Compositor_Text;
with Compositor_Formats;
with Compositor_Pool;
with Compositor_Backend_Selection;
procedure Software_Facade_Tests is
 package D renames Desktop_Compositor;
 package T renames Compositor_Text;
 package F renames Compositor_Formats;
 use type D.Output_Start, D.Render_Completion, D.Recovery_Result, T.Scene_Decision;
 procedure Fault (Code : Unsigned_32) with Import, Convention => C, External_Name => "fail_code";
 type Pixels is array (Natural range <>) of Unsigned_32;
 Target : aliased Pixels (0 .. 80 * 72 - 1) := (others => 16#FF123456#);
 Before : Pixels (Target'Range);
 Image : F.Image := (Target'Address, 80, 72, 320, 1);
 Screen : T.G.Output := (80, 72, T.G.Unrotated, (5, 4), 0, 0);
 Items : T.Glyphs := (1 => (65, (0, 0, 8, 17)), 2 => (66, (8, 0, 16, 17)), others => <>);
 Drawn, Repaint, Restart : Boolean;
 Damage, Writer_Damage : Compositor_Damage.State;
 Start : D.Output_Start;
 Completion : D.Render_Completion;
 Accepted : Boolean;
 Recovery : D.Recovery_Result;
 Ticket : constant Compositor_Pool.Ticket := (others => <>);
 procedure Draw (Length : T.Count := 2; Capacity : F.Byte_Count := 80 * 72 * 4) is
 begin
 D.Draw_Text (Image, Capacity, Screen, Items, Length, (0, 0, 80, 72), 16#FFFFFFFF#, False,
              Drawn, Repaint, Restart);
 end Draw;
begin
 pragma Assert (D.Selected and D.Software_Text);
 D.Configure_Renderer ((others => True), Accepted);
 pragma Assert (not Accepted);
 D.Configure_Renderer ((others => False), Accepted);
 pragma Assert (Accepted);
 D.Configure_Renderer ((others => False), Accepted);
 pragma Assert (not Accepted);
 D.Recover_Renderer ((Output => 0, Epoch => 1, Frame => 1, Buffer => 1), True, True, Recovery);
 pragma Assert (Recovery = D.Recovery_Unsafe);
 Compositor_Damage.Add (Damage, (0, 0, 80, 72));
 Writer_Damage := Damage;
 D.Begin_Output (Image, 80 * 72 * 4, Ticket, Screen, False, Start, Damage, Writer_Damage); pragma Assert (Start = D.Started);
 Before := Target; Image.Writable := 0; Draw;
 pragma Assert (not Drawn and not Repaint and not Restart and Target = Before);
 Image.Writable := 1; Draw (Capacity => 1);
 pragma Assert (not Drawn and not Repaint and not Restart and Target = Before);
 Image.Width := 79; Draw;
 pragma Assert (not Drawn and not Repaint and not Restart and Target = Before);
 Image.Width := 80; Draw (Length => 0);
 pragma Assert (Drawn and not Repaint and not Restart and Target = Before);
 Before := Target;
 if Ada.Command_Line.Argument_Count > 0 then
 Fault (65); Draw;
 pragma Assert (not Drawn and not Repaint and not Restart and Target = Before);
 Fault (0); Draw;
 pragma Assert (not Drawn and not Repaint and not Restart and Target = Before);
 Ada.Text_IO.Put_Line ("PASS first glyph fault disables retained text safely");
 return;
 end if;
 Fault (66); Draw;
 pragma Assert (not Drawn and Repaint and not Restart and Target /= Before);
 pragma Assert (T.Finish (1, Repaint) = T.Replay);
 -- Publication must await replay; the facade reports the partial write.
 D.Complete_Output (Target'Address, Ticket, False, False, Completion);
 pragma Assert (Completion = D.Complete);
 Target := Before; Fault (0); Draw;
 pragma Assert (not Drawn and not Repaint and not Restart and Target = Before);
 D.Complete_Output (Target'Address, Ticket, False, True, Completion);
 pragma Assert (Completion = D.Complete);
 Ada.Text_IO.Put_Line ("PASS software facade: first/later glyph faults, replay recovery, readonly/short/mismatched targets, empty batch, quiescence");
end Software_Facade_Tests;
