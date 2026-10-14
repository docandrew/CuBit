------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Clipboard regression client: an ordinary field and a secret field
--
--  Keyboard: Tab switches fields, Ctrl+A selects all, Ctrl+C/X/V (and the
--  Insert/Delete chords) use the desktop clipboard, Ctrl+R reveals or masks
--  the secret field, Esc quits. Serial markers name outcomes and lengths
--  only; a secret's content and length are never printed.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Startup_State;
with Startup;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.State;
with CuBit.UI.Controls;
with CuBit.UI.Editor;
with CuBit.UI.Input;
with CuBit.UI.Clipboard;
with CuBit.UI.Editor_Clipboard;

procedure Main is
   package App renames CuBit.UI.App;
   package Editor renames CuBit.UI.Editor;
   package Clip renames CuBit.UI.Clipboard;
   use type Clip.Outcome, Clip.Sensitivity, Editor.Sensitivity;

   KEY_TAB       : constant Unsigned_64 := 16#0F#;
   KEY_BACKSPACE : constant Unsigned_64 := 16#0E#;
   KEY_A         : constant Unsigned_64 := 16#1E#;
   KEY_R         : constant Unsigned_64 := 16#13#;
   KEYMOD_MASK   : constant Unsigned_64 := 16#0F#;
   CODE_POINT_MASK : constant Unsigned_64 := 16#FF#;

   WINDOW_W : constant := 460;
   WINDOW_H : constant := 190;
   FIELD_X  : constant := 120;
   FIELD_W  : constant := 310;
   FIELD_H  : constant := 30;

   type Field is (Name_Field, Secret_Field);
   Field_Y : constant array (Field) of Natural := [40, 96];
   Field_Label : constant array (Field) of access constant String :=
     [new String'("Name"), new String'("Password")];

   Win : App.Window;
   UI : CuBit.UI.State.UI_State;
   Map : CuBit.UI.Controls.Control_Map;
   Fields : array (Field) of Editor.Edit_State;
   Focused : Field := Name_Field;
   Revealed : Boolean := False;
   Ready : Boolean := False;
   Ignore : Unsigned_64;

   procedure Say (Text : String) is
   begin
      debugPrint ("clipboard-check: " & Text & ASCII.LF);
   end Say;

   function Field_Rect (F : Field) return Rect is
     ((x => FIELD_X, y => Field_Y (F), w => FIELD_W, h => FIELD_H));

   procedure Render (Win : in out App.Window; Damage : Rect) is
      C : constant Canvas := App.Canvas (Win, Damage);
      Colors : constant Theme := Current_Theme;
   begin
      Fill_Rect (C, App.Full_Rect (Win), Colors.face);
      for F in Field loop
         Draw_UI_Text (C, 20, Field_Y (F) + 7, Field_Label (F).all,
                       Colors.text, Colors.face);
         Draw_Text_Edit_Field
           (C, Field_Rect (F), Colors, Editor.Content (Fields (F)),
            Editor.Cursor (Fields (F)) - 1,
            Editor.Selection_First (Fields (F)) - 1,
            Editor.Selection_Last (Fields (F)) - 1,
            Focused = F, False,
            masked => Editor.Kind_Of (Fields (F)) = Editor.Sensitive
                      and then not Revealed);
      end loop;
      Draw_UI_Text (C, 20, 148,
                    "Tab field  Ctrl+C/X/V clipboard  Ctrl+R reveal",
                    Colors.muted, Colors.face);
      if not Ready then
         Ready := True;
         Say ("ready");
      end if;
   end Render;

   --  A paste the user did not ask for must be refused.
   procedure Probe_Ungestured_Paste is
      Buffer : String (1 .. 64);
      Last : Natural;
      Kind : Clip.Sensitivity;
      Result : Clip.Outcome;
   begin
      Clip.Paste_Text (App.Surface_ID (Win), Clip.Ordinary, Buffer, Last,
                       Kind, Result);
      Say ("ungestured paste " &
           (if Result = Clip.Denied then "denied" else "ALLOWED"));
   end Probe_Ungestured_Paste;

   procedure Handle (Win : in out App.Window; Event : App.Input_Event;
                     Dirty : in out Rect; Running : in out Boolean) is
      Changed : Boolean := False;
      Mods : constant Unsigned_64 := Event.payload1 and KEYMOD_MASK;
      Ctrl : constant Boolean := (Mods and App.KEYMOD_CTRL) /= 0;
   begin
      if Event.kind = App.INPUT_KEY_DOWN then
         declare
            R : constant CuBit.UI.Editor_Clipboard.Result :=
              CuBit.UI.Editor_Clipboard.Handle_Key
                (Fields (Focused), App.Surface_ID (Win), Event.payload0, Mods);
         begin
            if R.Handled then
               --  Select All changes only the selection: redraw anyway.
               Changed := True;
               Say ((case Clip.Chord_Of (Event.payload0, Mods) is
                       when Clip.Copy_Chord => "copy",
                       when Clip.Cut_Chord => "cut",
                       when Clip.Paste_Chord => "paste",
                       when Clip.No_Chord => "select-all") &
                    " field=" & (if Focused = Name_Field then "name" else "secret") &
                    " outcome=" & Clip.Image (R.Outcome) &
                    (if Focused = Name_Field then
                       " length=" & Editor.Length (Fields (Focused))'Image
                     else ""));
            elsif Event.payload0 = KEY_TAB then
               Focused := (if Focused = Name_Field then Secret_Field else Name_Field);
               Changed := True;
            elsif Ctrl and then Event.payload0 = KEY_A then
               Editor.Select_All (Fields (Focused));
               Changed := True;
            elsif Ctrl and then Event.payload0 = KEY_R then
               Revealed := not Revealed;
               Changed := True;
            elsif Event.payload0 = KEY_BACKSPACE then
               Editor.Backspace (Fields (Focused), Changed);
            elsif Event.payload0 = App.KEY_ESC then
               Running := False;
            end if;
         end;
      elsif Event.kind = App.INPUT_TEXT then
         declare
            Code : constant Unsigned_64 := Event.payload0 and CODE_POINT_MASK;
         begin
            if Code in Character'Pos (' ') .. Character'Pos ('~') then
               Editor.Insert (Fields (Focused), [Character'Val (Code)], Changed);
            end if;
         end;
      elsif Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      end if;
      if Changed then
         Dirty := App.Full_Rect (Win);
      end if;
   end Handle;

   procedure Run is new App.Run (UI, Map, Render => Render, Handle_Event => Handle);
   OK, Accepted : Boolean;
begin
   if not CuBit.Startup_State.Ready (Startup.Verify) then
      debugPrint ("clipboard-check: startup capabilities missing" & ASCII.LF);
      return;
   end if;
   Editor.Initialize (Fields (Name_Field), "", Accepted);
   Editor.Initialize (Fields (Secret_Field), "", Accepted, Kind => Editor.Sensitive);
   App.Open (Win, WINDOW_W, WINDOW_H,
             App.WINDOW_FLAG_DECORATED or App.WINDOW_FLAG_CLOSEABLE, OK,
             title => "Clipboard check", protected_frames => True);
   if not OK then
      Say ("FAIL window");
      Ignore := syscall (SYSCALL_EXIT, 1);
   end if;
   Probe_Ungestured_Paste;
   Run (Win);
   --  Leave nothing behind: the secret field is overwritten before exit.
   Editor.Wipe (Fields (Secret_Field));
   App.Close (Win);
   Say ("closed");
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
