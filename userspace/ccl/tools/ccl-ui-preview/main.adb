with Interfaces; use Interfaces;
with Interfaces.C;
with System;
with CCL.Language;
with CCL.VM;
with CuBit.UI;
with CuBit.UI.Editor;
with CuBit.UI.Widgets;

--  HOSTED/LINUX Workbench preview.  Rendering below uses the SHARED CuBit UI
--  canvas.  Native_Window is the only hosted presentation/input boundary.
procedure Main is
   use type Interfaces.C.int;
   use type Interfaces.C.unsigned;
   use type System.Address;
   use type CCL.Language.Interpretation_Status;
   use type CCL.VM.Value_Kind;

   --  Compact native canvas: never downscale the toolkit's 11 px UI font.
   --  The hosted adapter scales this canvas upward when space permits.
   WIDTH  : constant Natural := 640;
   HEIGHT : constant Natural := 400;

   type Pixel_Buffer is array (Natural range 0 .. WIDTH * HEIGHT - 1)
     of aliased Unsigned_32;
   Pixels : aliased Pixel_Buffer := [others => 0];

   Canvas : constant CuBit.UI.Canvas :=
     (addr => Pixels'Address, width => WIDTH, height => HEIGHT,
      pitch => WIDTH * 4, clipEnabled => False, clip => (others => 0));
   Colors : constant CuBit.UI.Theme := CuBit.UI.Classic;

   Input       : CuBit.UI.Editor.Edit_State;
   Result_Text : String (1 .. 96) := [others => ' '];
   Result_Last : Natural := 5;
   Input_Bounds : CuBit.UI.Rect := (others => 0);

   function Window_Open (Width, Height : Interfaces.C.int) return System.Address
   with Import, Convention => C, External_Name => "ccl_window_open";
   function Window_Poll
     (Handle : System.Address; Kind : access Interfaces.C.int;
      Code, Modifiers : access Interfaces.C.unsigned;
      X, Y : access Interfaces.C.int) return Interfaces.C.int
   with Import, Convention => C, External_Name => "ccl_window_poll";
   function Window_Present
     (Handle, Pixels : System.Address;
      Pitch : Interfaces.C.int) return Interfaces.C.int
   with Import, Convention => C, External_Name => "ccl_window_present";
   procedure Window_Wait
   with Import, Convention => C, External_Name => "ccl_window_wait";
   procedure Window_Close (Handle : System.Address)
   with Import, Convention => C, External_Name => "ccl_window_close";

   procedure Set_Result (Text : String) is
      Length : constant Natural := Natural'Min (Text'Length, Result_Text'Length);
   begin
      Result_Text := [others => ' '];
      if Length > 0 then
         Result_Text (1 .. Length) := Text (Text'First .. Text'First + Length - 1);
      end if;
      Result_Last := Length;
   end Set_Result;

   procedure Submit is
      Outcome : CCL.Language.Interpretation_Result;
   begin
      if CuBit.UI.Editor.Length (Input) = 0 then return; end if;
      CCL.Language.Interpret (CuBit.UI.Editor.Content (Input), 1_024, Outcome);
      if Outcome.Status /= CCL.Language.Succeeded then
         Set_Result ("error: " & CCL.Language.Diagnostic_Code'Image (Outcome.Diagnostic));
      elsif not Outcome.Has_Value then
         Set_Result ("ok");
      elsif Outcome.Result_Value.Kind = CCL.VM.Integer_Value then
         Set_Result (Integer_64'Image (Outcome.Result_Value.Integer));
      else
         Set_Result ((if Outcome.Result_Value.Boolean then "true" else "false"));
      end if;
   end Submit;

   function Position_At (Pixel_X : Natural)
     return CuBit.UI.Editor.Text_Position
   is
      Text : constant String := CuBit.UI.Editor.Content (Input);
      Text_X : constant Natural := Input_Bounds.x + 8;
      Offset : Natural;
      Draw_X : Natural := 0;
      Width : Natural;
   begin
      if Pixel_X <= Text_X then return 1; end if;
      Offset := Pixel_X - Text_X;
      for Index in Text'Range loop
         Width := CuBit.UI.UI_Text_Width (Text (Index .. Index));
         if Offset < Draw_X + (Width + 1) / 2 then
            return CuBit.UI.Editor.Text_Position (Index);
         end if;
         Draw_X := Draw_X + Width;
      end loop;
      return CuBit.UI.Editor.Length (Input) + 1;
   end Position_At;

   procedure Draw_Chart (Bounds : CuBit.UI.Rect) is
      Samples : constant array (Natural range 0 .. 15) of Natural :=
        [18, 25, 21, 34, 30, 42, 48, 39, 55, 63, 57, 70, 66, 79, 73, 86];
      Inner : constant CuBit.UI.Rect :=
        (x => Bounds.x + 12, y => Bounds.y + 12,
         w => Bounds.w - 24, h => Bounds.h - 24);
      Bar_W : constant Natural := Inner.w / Samples'Length;
      Bar_H : Natural;
   begin
      CuBit.UI.Fill_Rect (Canvas, Bounds, Colors.panel);
      CuBit.UI.Stroke_Rect (Canvas, Bounds, Colors.edge, Colors.shadow);
      for Index in Samples'Range loop
         Bar_H := Samples (Index) * Inner.h / 100;
         CuBit.UI.Fill_Rect
           (Canvas,
            (x => Inner.x + Index * Bar_W,
             y => Inner.y + Inner.h - Bar_H,
             w => Natural'Max (1, Bar_W - 3), h => Bar_H),
            Colors.accent);
      end loop;
   end Draw_Chart;

   procedure Render is
      Content : CuBit.UI.Rect;
      Repl_Content : CuBit.UI.Rect;
      Monitor_Content : CuBit.UI.Rect;
      Card_W : constant Natural := 100;
      Prompt_W : constant Natural := CuBit.UI.UI_Text_Width ("ccl>");
   begin
      CuBit.UI.Fill_Rect
        (Canvas, (x => 0, y => 0, w => WIDTH, h => HEIGHT), Colors.desktop);

      CuBit.UI.Draw_Menu_Bar
        (Canvas, (x => 0, y => 0, w => WIDTH, h => 28), Colors);
      CuBit.UI.Draw_UI_Text
        (Canvas, 10, 7, "CCL Workbench", Colors.text, Colors.panel);
      CuBit.UI.Widgets.Badge
        (Canvas, (x => WIDTH - 144, y => 3, w => 136, h => 22), Colors,
         "deterministic data", CuBit.UI.Widgets.Badge_Good);

      CuBit.UI.Widgets.Group_Box
        (Canvas, (x => 8, y => 36, w => 220, h => 326), Colors,
         "CCL REPL", Repl_Content, 8);
      CuBit.UI.Draw_UI_Text
        (Canvas, Repl_Content.x, Repl_Content.y,
         "ccl>", Colors.text, Colors.face);
      Input_Bounds :=
        (x => Repl_Content.x + Prompt_W + 8, y => Repl_Content.y - 5,
         w => Repl_Content.w - Prompt_W - 8, h => 25);
      CuBit.UI.Draw_Text_Edit_Field
        (CuBit.UI.With_Clip (Canvas, Input_Bounds), Input_Bounds,
         Colors, CuBit.UI.Editor.Content (Input),
         CuBit.UI.Editor.Cursor (Input) - 1,
         CuBit.UI.Editor.Selection_First (Input) - 1,
         CuBit.UI.Editor.Selection_Last (Input) - 1,
         focused => True, hot => False);
      CuBit.UI.Draw_UI_Text
        (Canvas, Repl_Content.x, Repl_Content.y + 32,
         Result_Text (1 .. Result_Last), Colors.muted, Colors.face);
      CuBit.UI.Widgets.Group_Box
        (Canvas,
         (x => Repl_Content.x, y => Repl_Content.y + 84,
          w => Repl_Content.w, h => 132),
         Colors, "Session authority", Content, 8);
      CuBit.UI.Widgets.Key_Value
        (Canvas, (x => Content.x, y => Content.y, w => Content.w, h => 24),
         Colors, "Network", "observe only");
      CuBit.UI.Widgets.Key_Value
        (Canvas, (x => Content.x, y => Content.y + 30,
                  w => Content.w, h => 24),
         Colors, "UI", "surface 7");
      CuBit.UI.Widgets.Key_Value
        (Canvas, (x => Content.x, y => Content.y + 60,
                  w => Content.w, h => 24),
         Colors, "Control", "not granted", True);

      CuBit.UI.Widgets.Group_Box
        (Canvas, (x => 236, y => 36, w => 396, h => 326), Colors,
         "Network monitor", Monitor_Content, 8);
      CuBit.UI.Widgets.Metric_Card
        (Canvas,
         (x => Monitor_Content.x, y => Monitor_Content.y,
          w => Card_W, h => 56),
         Colors, "Interfaces", 2);
      CuBit.UI.Widgets.Metric_Card
        (Canvas,
         (x => Monitor_Content.x + Card_W + 10, y => Monitor_Content.y,
          w => Card_W, h => 56),
         Colors, "Received MiB", 18);
      CuBit.UI.Widgets.Metric_Card
        (Canvas,
         (x => Monitor_Content.x + (Card_W + 10) * 2,
          y => Monitor_Content.y, w => Card_W, h => 56),
         Colors, "Sent MiB", 4);

      CuBit.UI.Widgets.Label
        (Canvas,
         (x => Monitor_Content.x, y => Monitor_Content.y + 68,
          w => Monitor_Content.w, h => 24),
         Colors, "Throughput samples", muted => True);
      Draw_Chart
        ((x => Monitor_Content.x, y => Monitor_Content.y + 92,
          w => Monitor_Content.w, h => 80));

      CuBit.UI.Draw_Table_Header
        (Canvas,
         (x => Monitor_Content.x, y => Monitor_Content.y + 182,
          w => Monitor_Content.w, h => 24),
         Colors, "Interface", "State", "Scope");
      CuBit.UI.Draw_Table_Row
        (Canvas,
         (x => Monitor_Content.x, y => Monitor_Content.y + 206,
          w => Monitor_Content.w, h => 28),
         Colors, True, False, "virtio0", "online", "observe");
      CuBit.UI.Draw_Table_Row
        (Canvas,
         (x => Monitor_Content.x, y => Monitor_Content.y + 234,
          w => Monitor_Content.w, h => 28),
         Colors, False, False, "loopback", "online", "observe");
      CuBit.UI.Widgets.Badge
        (Canvas,
         (x => Monitor_Content.x,
          y => Monitor_Content.y + Monitor_Content.h - 28,
          w => 196, h => 28),
         Colors, "Network.Control absent", CuBit.UI.Widgets.Badge_Neutral);

      CuBit.UI.Draw_Status_Bar
        (Canvas, (x => 0, y => HEIGHT - 26, w => WIDTH, h => 26), Colors,
         "Monitor preview uses the CuBit widget toolkit",
         "no live authority");
   end Render;

begin
   declare
      Accepted : Boolean;
   begin
      CuBit.UI.Editor.Initialize (Input, "(+ 20 22)", Accepted);
      if not Accepted then raise Program_Error; end if;
   end;
   Result_Text (1 .. Result_Last) := "ready";
   declare
      Handle : constant System.Address :=
        Window_Open (Interfaces.C.int (WIDTH), Interfaces.C.int (HEIGHT));
      Kind : aliased Interfaces.C.int := 0;
      Code : aliased Interfaces.C.unsigned := 0;
      Modifiers : aliased Interfaces.C.unsigned := 0;
      Mouse_X : aliased Interfaces.C.int := 0;
      Mouse_Y : aliased Interfaces.C.int := 0;
      Running : Boolean := Handle /= System.Null_Address;
      Dragging : Boolean := False;
      Changed : Boolean;
      Extend : Boolean;
      By_Word : Boolean;
   begin
      if not Running then raise Program_Error; end if;
      Render;
      while Running loop
         while Running and then
           Window_Poll
             (Handle, Kind'Access, Code'Access, Modifiers'Access,
              Mouse_X'Access, Mouse_Y'Access) /= 0
         loop
            case Kind is
               when 1 => Running := False;
               when 2 =>
                  if Code >= 32 and then Code <= 126 then
                     CuBit.UI.Editor.Insert
                       (Input, String'(1 => Character'Val (Code)), Changed);
                  end if;
               when 3 => CuBit.UI.Editor.Backspace (Input, Changed);
               when 4 => Submit;
               when 5 | 6 =>
                  Extend := (Modifiers and 1) /= 0;
                  By_Word := (Modifiers and 2) /= 0;
                  CuBit.UI.Editor.Move
                    (Input,
                     (if Kind = 5 then
                        (if By_Word then CuBit.UI.Editor.Move_Word_Left
                         else CuBit.UI.Editor.Move_Left)
                      else
                        (if By_Word then CuBit.UI.Editor.Move_Word_Right
                         else CuBit.UI.Editor.Move_Right)),
                     Extend);
               when 7 =>
                  CuBit.UI.Editor.Move
                    (Input, CuBit.UI.Editor.Move_Start,
                     (Modifiers and 1) /= 0);
               when 8 =>
                  CuBit.UI.Editor.Move
                    (Input, CuBit.UI.Editor.Move_End,
                     (Modifiers and 1) /= 0);
               when 9 => CuBit.UI.Editor.Delete_Forward (Input, Changed);
               when 10 => CuBit.UI.Editor.Select_All (Input);
               when 11 | 14 | 15 =>
                  if Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Input_Bounds)
                  then
                     if Kind = 15 then
                        CuBit.UI.Editor.Select_All (Input);
                     elsif Kind = 14 then
                        CuBit.UI.Editor.Select_Word_At
                          (Input, Position_At (Natural (Mouse_X)));
                     else
                        CuBit.UI.Editor.Place_Cursor
                          (Input, Position_At (Natural (Mouse_X)),
                           (Modifiers and 1) /= 0);
                     end if;
                     Dragging := True;
                  end if;
               when 12 =>
                  if Dragging and then Mouse_X >= 0 then
                     CuBit.UI.Editor.Place_Cursor
                       (Input, Position_At (Natural (Mouse_X)),
                        Extend_Selection => True);
                  end if;
               when 13 => Dragging := False;
               when others => null;
            end case;
         end loop;
         exit when not Running;
         Render;
         exit when Window_Present
           (Handle, Pixels'Address, Interfaces.C.int (WIDTH * 4)) /= 0;
         Window_Wait;
      end loop;
      Window_Close (Handle);
   end;
end Main;
