------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Clipboard chords for CuBit.UI.Editor text fields
--
--  Call from the key-down handler of the field that has keyboard focus.
--  Ctrl+C / Ctrl+Insert copy the selection, Ctrl+X / Shift+Delete cut it,
--  Ctrl+V / Shift+Insert paste, Ctrl+A selects the whole field. A
--  Sensitive field copies a paste-once secret and consumes what it pastes.
--  Line breaks become spaces (single-line fields); other control bytes are
--  dropped (Clipboard_Text.Single_Line). The chords are Clipboard_Chords',
--  the table Desktop issues the matching tickets from.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.UI.Editor;
with CuBit.UI.Clipboard;

package CuBit.UI.Editor_Clipboard is
   type Result is record
      Handled : Boolean := False;   --  the key was a clipboard chord
      Changed : Boolean := False;   --  the field's text changed
      Outcome : CuBit.UI.Clipboard.Outcome := CuBit.UI.Clipboard.Done;
   end record;

   function Handle_Key
     (State : in out CuBit.UI.Editor.Edit_State; Surface : Unsigned_64;
      Code, Modifiers : Unsigned_64) return Result;

   --  The same chords for a plain one-line buffer without a selection
   --  (Text (Text'First .. Text'First + Length - 1)): copy and cut take the
   --  whole text, paste appends, Select All is not handled (the field has
   --  no selection to show). Use Handle_Key where the field can hold an
   --  Edit_State.
   function Handle_Line_Key
     (Text : in out String; Length : in out Natural; Surface : Unsigned_64;
      Code, Modifiers : Unsigned_64; Kind : CuBit.UI.Clipboard.Sensitivity :=
        CuBit.UI.Clipboard.Ordinary) return Result
   with Pre => Length <= Text'Length;
end CuBit.UI.Editor_Clipboard;
