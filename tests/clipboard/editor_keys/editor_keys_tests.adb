--  Hosted tests of the toolkit's clipboard chords: CuBit.UI.Editor_Clipboard
--  on Edit_State fields (Handle_Key) and plain line buffers (Handle_Line_Key),
--  against Host_Clipboard, which issues tickets from each key exactly as
--  Desktop does. Every chord the toolkit acts on must find its ticket.
--  Run by tests/clipboard/run.sh.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI.Clipboard;
with CuBit.UI.Editor;
with CuBit.UI.Editor_Clipboard;
with Host_Clipboard;
with Clipboard_Chords;

procedure Editor_Keys_Tests is
   package C renames CuBit.UI.Clipboard;
   package E renames CuBit.UI.Editor;
   package EC renames CuBit.UI.Editor_Clipboard;
   package H renames Host_Clipboard;
   use type C.Outcome;
   use type C.Sensitivity;

   KEY_A      : constant Unsigned_64 := 16#1E#;
   KEY_C      : constant Unsigned_64 := 16#2E#;
   KEY_X      : constant Unsigned_64 := 16#2D#;
   KEY_V      : constant Unsigned_64 := 16#2F#;
   KEY_INSERT : constant Unsigned_64 := 16#52#;
   KEY_DELETE : constant Unsigned_64 := 16#53#;
   SHIFT : constant Unsigned_64 := 1;
   CTRL  : constant Unsigned_64 := 2;
   ALT   : constant Unsigned_64 := 4;
   SURFACE : constant Unsigned_64 := 7;

   Failures : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   --  Desktop routes the key (issuing its ticket), then the field acts.
   function Press
     (Field : in out E.Edit_State; Code, Modifiers : Unsigned_64) return EC.Result is
   begin
      H.Key (Code, Modifiers);
      return EC.Handle_Key (Field, SURFACE, Code, Modifiers);
   end Press;

   function Field_Of (Text : String; Kind : E.Sensitivity := E.Ordinary) return E.Edit_State is
      Field : E.Edit_State;
      Accepted : Boolean;
   begin
      E.Initialize (Field, Text, Accepted, Kind);
      return Field;
   end Field_Of;

   procedure Ordinary_Field is
      Field : E.Edit_State := Field_Of ("hello world");
      R : EC.Result;
   begin
      --  Copy with nothing selected: spent, nothing copied.
      R := Press (Field, KEY_C, CTRL);
      Check (R.Handled and then R.Outcome = C.Empty and then H.Copies = 0, "copy without a selection");
      R := Press (Field, KEY_A, CTRL);
      Check (R.Handled and then E.Selection_First (Field) = 1 and then E.Selection_Last (Field) = 12,
             "Ctrl+A selects the field");
      R := Press (Field, KEY_C, CTRL);
      Check (R.Outcome = C.Done and then H.Current = "hello world" and then H.Kind = C.Ordinary,
             "Ctrl+C copies the selection as ordinary text");
      Check (E.Content (Field) = "hello world" and then not R.Changed, "copy leaves the field");
      R := Press (Field, KEY_X, CTRL);
      Check (R.Outcome = C.Done and then R.Changed and then E.Length (Field) = 0, "Ctrl+X cuts");
      R := Press (Field, KEY_V, CTRL);
      Check (R.Outcome = C.Done and then E.Content (Field) = "hello world", "Ctrl+V pastes");
      R := Press (Field, KEY_INSERT, SHIFT);
      Check (R.Outcome = C.Done and then E.Content (Field) = "hello worldhello world", "Shift+Insert pastes");
      E.Select_All (Field);
      R := Press (Field, KEY_DELETE, SHIFT);
      Check (R.Outcome = C.Done and then E.Length (Field) = 0
             and then H.Current = "hello worldhello world", "Shift+Delete cuts");
      Field := Field_Of ("abc");
      E.Select_All (Field);
      R := Press (Field, KEY_INSERT, CTRL);
      Check (R.Outcome = C.Done and then H.Current = "abc", "Ctrl+Insert copies");
      --  Not chords: Alt combinations, Ctrl+Shift+A, plain letters.
      R := Press (Field, KEY_C, CTRL or ALT);
      Check (not R.Handled, "Ctrl+Alt+C is not a chord");
      R := Press (Field, KEY_A, CTRL or SHIFT);
      Check (not R.Handled, "Ctrl+Shift+A is not Select All");
      R := Press (Field, KEY_C, 0);
      Check (not R.Handled, "C alone is text, not a chord");
   end Ordinary_Field;

   procedure Without_Ticket is
      Field : E.Edit_State := Field_Of ("keep");
      R : EC.Result;
   begin
      E.Select_All (Field);
      H.Ticket := Clipboard_Chords.No_Ticket;
      R := EC.Handle_Key (Field, SURFACE, KEY_X, CTRL);
      Check (R.Outcome = C.Denied and then E.Content (Field) = "keep" and then not R.Changed,
             "a refused cut keeps the text");
      H.Put ("planted");
      R := EC.Handle_Key (Field, SURFACE, KEY_V, CTRL);
      Check (R.Outcome = C.Denied and then E.Content (Field) = "keep", "no paste without a ticket");
      --  A copy ticket never pastes, a paste ticket never copies.
      H.Key (KEY_C, CTRL);
      R := EC.Handle_Key (Field, SURFACE, KEY_V, CTRL);
      Check (R.Outcome = C.Denied, "a copy ticket does not paste");
      H.Key (KEY_V, CTRL);
      R := EC.Handle_Key (Field, SURFACE, KEY_C, CTRL);
      Check (R.Outcome = C.Denied and then H.Current = "planted", "a paste ticket does not copy");
   end Without_Ticket;

   procedure Sensitive_Field is
      Secret : E.Edit_State := Field_Of ("hunter2", E.Sensitive);
      Plain : E.Edit_State := Field_Of ("");
      R : EC.Result;
   begin
      E.Select_All (Secret);
      R := Press (Secret, KEY_C, CTRL);
      Check (R.Outcome = C.Done and then H.Kind = C.Sensitive, "a sensitive field copies a secret");
      R := Press (Plain, KEY_V, CTRL);
      Check (R.Outcome = C.Done and then E.Content (Plain) = "hunter2", "the secret pastes once");
      R := Press (Plain, KEY_V, CTRL);
      Check (R.Outcome = C.Empty and then E.Content (Plain) = "hunter2", "and only once");
      --  An ordinary entry pasted into a sensitive field is consumed too.
      H.Put ("ordinary");
      E.Wipe (Secret);
      R := Press (Secret, KEY_V, CTRL);
      Check (R.Outcome = C.Done and then E.Content (Secret) = "ordinary" and then H.Length = 0,
             "a paste into a sensitive field leaves no copy");
   end Sensitive_Field;

   procedure Line_Breaks is
      Field : E.Edit_State := Field_Of ("");
      R : EC.Result;
   begin
      H.Put ("a" & ASCII.LF & "b" & ASCII.HT & "c" & Character'Val (1) & "d" & ASCII.CR);
      R := Press (Field, KEY_V, CTRL);
      Check (R.Outcome = C.Done and then E.Content (Field) = "a b cd ", "pasted controls become one line");
   end Line_Breaks;

   procedure Line_Buffer is
      Text : String (1 .. 8) := [others => ' '];
      Length : Natural := 0;
      R : EC.Result;
      function Line_Press (Code, Modifiers : Unsigned_64) return EC.Result is
      begin
         H.Key (Code, Modifiers);
         return EC.Handle_Line_Key (Text, Length, SURFACE, Code, Modifiers);
      end Line_Press;
   begin
      R := Line_Press (KEY_C, CTRL);
      Check (R.Handled and then R.Outcome = C.Empty, "line: copy of nothing");
      H.Put ("name");
      R := Line_Press (KEY_V, CTRL);
      Check (R.Outcome = C.Done and then Text (1 .. Length) = "name", "line: paste");
      R := Line_Press (KEY_C, CTRL);
      Check (R.Outcome = C.Done and then H.Current = "name", "line: copy takes the whole text");
      H.Put ("toolong");
      R := Line_Press (KEY_INSERT, SHIFT);
      Check (R.Outcome = C.Done and then Text (1 .. Length) = "nametool", "line: paste stops at the buffer");
      R := Line_Press (KEY_V, CTRL);
      Check (R.Outcome = C.Too_Large and then Length = 8, "line: a full buffer takes nothing");
      R := Line_Press (KEY_X, CTRL);
      Check (R.Outcome = C.Done and then Length = 0 and then H.Current = "nametool", "line: cut");
      R := Line_Press (KEY_A, CTRL);
      Check (not R.Handled, "line: no Select All without a selection");
   end Line_Buffer;
begin
   Ordinary_Field;
   Without_Ticket;
   Sensitive_Field;
   Line_Breaks;
   Line_Buffer;
   if Failures = 0 then
      Put_Line ("editor keys: PASS" & Checks'Image & " checks");
   else
      Put_Line ("editor keys: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Editor_Keys_Tests;
