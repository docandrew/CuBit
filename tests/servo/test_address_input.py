#!/usr/bin/env python3
"""Hosted actual shell branches + real editor; no native IPC/rendering claim."""
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
source = (ROOT / 'userspace/servo/native/servo_session.adb').read_text()

def between(start, end):
    assert source.count(start) == 1 and source.count(end) == 1
    return source.split(start, 1)[1].split(end, 1)[0]

resync = between('      if Input.kind = App.INPUT_CONFIGURE or else Input.kind = App.INPUT_RESYNC then',
                 '      if Settings_Open then\n         Settings_Input (Input, Result); return 1;')
resync = '      if Input.kind = App.INPUT_CONFIGURE or else Input.kind = App.INPUT_RESYNC then' + resync
focused = between('      if Focused and then Input.kind in App.INPUT_KEY_DOWN | App.INPUT_KEY_UP | App.INPUT_TEXT then',
                  '      if Input.kind in App.INPUT_POINTER_MOVE | App.INPUT_POINTER_DOWN |')
focused = '      if Focused and then Input.kind in App.INPUT_KEY_DOWN | App.INPUT_KEY_UP | App.INPUT_TEXT then' + focused
load = between('   procedure Load_Address is', '   procedure Navigation_Error is')
load = '   procedure Load_Address is' + load
ada = '''with Interfaces; use Interfaces;
with Browser_Editor;
with Ada.Text_IO; use Ada.Text_IO;
procedure Address_Check is
   package Editor renames Browser_Editor;
   package App is
      INPUT_CONFIGURE : constant Unsigned_64 := 8;
      INPUT_RESYNC : constant Unsigned_64 := 9;
      INPUT_KEY_DOWN : constant Unsigned_64 := 1;
      INPUT_KEY_UP : constant Unsigned_64 := 2;
      INPUT_TEXT : constant Unsigned_64 := 3;
   end App;
   type Event is record
      Kind, Payload0, Payload1 : Unsigned_64 := 0;
   end record;
   Input, Result : Event;
   Address_Edit : Editor.Edit_State;
   Current_URL : constant String := "https://original.example/";
   URL_Last : constant Natural := Current_URL'Length;
   First_Shown : Natural := 0;
   Held_Buttons, Pressed_Control : Unsigned_64 := 0;
   Focused, Chrome_Dirty, Chrome_Press, Address_Input_Lost : Boolean := False;
   Ctrl, Shift : Boolean := False;
   Changed, OK : Boolean;
   procedure Reveal_Cursor is null;
''' + load + '''
   function Poll return Natural is
   begin
''' + resync + focused + '''
      return 0;
   end Poll;
   procedure Send (Kind, Code : Unsigned_64; Expect_Dirty : Boolean) is
      Value : Natural;
   begin
      Input := (Kind, Code, 0); Result := (others => 0);
      Chrome_Dirty := False;
      Value := Poll;
      pragma Assert (Value = 1 and Chrome_Dirty = Expect_Dirty);
   end Send;
begin
   for Cycle in 1 .. 1000 loop
      Load_Address; Focus_Address;
      Send (App.INPUT_KEY_UP, 16#1E#, False);
      Send (App.INPUT_KEY_DOWN, 16#1E#, False);
      Send (App.INPUT_TEXT, Character'Pos ('x'), True);
      pragma Assert (Editor.Content (Address_Edit) = "x");
      Send (App.INPUT_CONFIGURE, 0, True);
      pragma Assert (not Address_Input_Lost and Editor.Content (Address_Edit) = "x");
      Send (App.INPUT_KEY_DOWN, 16#1C#, True);
      pragma Assert (Result.Kind = 16 and not Focused);
      Focus_Address;
      Send (App.INPUT_TEXT, Character'Pos ('y'), True);
      Send (App.INPUT_RESYNC, 0, True);
      pragma Assert (Address_Input_Lost and Focused);
      Send (App.INPUT_KEY_DOWN, 16#1C#, True);
      pragma Assert (Result.Kind = 22 and Focused);
      Send (App.INPUT_TEXT, Character'Pos ('z'), True);
      Send (App.INPUT_KEY_DOWN, 16#1C#, True);
      pragma Assert (Result.Kind = 22 and Address_Input_Lost);
      Focus_Address;
      pragma Assert (not Address_Input_Lost and Editor.Content (Address_Edit) = Current_URL);
      Send (App.INPUT_TEXT, Character'Pos ('q'), True);
      Send (App.INPUT_KEY_DOWN, 16#1C#, True);
      pragma Assert (Result.Kind = 16 and not Focused and Editor.Content (Address_Edit) = "q");
   end loop;
   Put_Line ("SERVO-ADDRESS-INPUT: PASS 1000 actual-branch cycles dirty/configure/resync/restart");
end Address_Check;
'''
with tempfile.TemporaryDirectory(prefix='cubit-servo-address-') as directory:
    work = Path(directory)
    for ext in ('ads', 'adb'):
        editor = (ROOT / f'userspace/lib/ui/cubit-ui-editor.{ext}').read_text()
        (work / f'browser_editor.{ext}').write_text(editor.replace('CuBit.UI.Editor', 'Browser_Editor'))
    (work / 'address_check.adb').write_text(ada)
    subprocess.run(['gnatmake', '-q', '-gnat2022', '-gnata', '-gnato', 'address_check.adb'], cwd=work, check=True)
    subprocess.run([str(work / 'address_check')], cwd=work, check=True)
