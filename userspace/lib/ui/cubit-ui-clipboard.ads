------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Client side of the desktop clipboard (docs/clipboard.md)
--
--  Copy offers bytes to Desktop; paste asks Desktop for the current entry.
--  Both succeed only right after the user's own chord (Ctrl+C/X/V,
--  Ctrl/Shift+Insert, Shift+Delete) delivered to this surface: call them from
--  the key handler of that chord. The transfer window is one 64 KiB region
--  this process owns and grants to Desktop once; it is zeroed after every
--  transfer. Secrets (Sensitive) are pasted once and never shown in the
--  manager, logged or shared with a host.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Compositor_Clipboard_Protocol;
with Clipboard_Chords;
with CuBit.UI.Input;
with Clipboard_Text;

package CuBit.UI.Clipboard is
   package P renames Compositor_Clipboard_Protocol;
   subtype Sensitivity is P.Sensitivity;
   Ordinary  : constant Sensitivity := P.Ordinary;
   Sensitive : constant Sensitivity := P.Sensitive;

   --  Explicit bound on a clipboard call to Desktop.
   Call_Deadline_Ms : constant := 1_000;

   type Outcome is
     (Done,          --  copied, or pasted into the caller's buffer
      Denied,        --  no fresh chord for this surface, or not focused
      Empty,         --  nothing to paste
      Too_Large,     --  over P.Maximum_Payload, or the caller's buffer
      Unavailable);  --  no window, Desktop refused it, or a failed reply

   --  For diagnostics: the runtime discards enumeration names.
   function Image (Item : Outcome) return String is
     (case Item is
        when Done => "DONE", when Denied => "DENIED", when Empty => "EMPTY",
        when Too_Large => "TOO_LARGE", when Unavailable => "UNAVAILABLE");

   --  Copy plain text (UTF-8).
   procedure Copy_Text
     (Surface : Unsigned_64; Text : String; Kind : Sensitivity;
      Result : out Outcome);

   --  Copy a typed CCL value: its type name, a text form for plain text
   --  fields, and the canonical literal that reads back as the value.
   procedure Copy_Value
     (Surface : Unsigned_64; Type_Name, Text, Literal : String;
      Result : out Outcome);

   --  Copy what a view built (Clipboard_Text): the typed value when Literal
   --  holds one that fits, else the text form alone. Too_Large when the
   --  text form did not fit either; Empty when it is empty.
   procedure Copy_Built
     (Surface : Unsigned_64; Type_Name : String;
      Text, Literal : Clipboard_Text.Builder; Result : out Outcome);

   --  Paste as text into Text (Text'First .. Last), truncated to Text'Length.
   --  A Sensitive target
   --  consumes the entry: a secret field never leaves a copy behind.
   --  Kind reports whether the entry was a secret.
   procedure Paste_Text
     (Surface : Unsigned_64; Target : Sensitivity;
      Text : out String; Last : out Natural; Kind : out Sensitivity;
      Result : out Outcome);

   --  Paste with the typed form when the entry has one: Type_Name and
   --  Literal are empty for plain text entries.
   procedure Paste_Value
     (Surface : Unsigned_64;
      Type_Name : out String; Type_Last : out Natural;
      Text : out String; Text_Last : out Natural;
      Literal : out String; Literal_Last : out Natural;
      Result : out Outcome);

   --  The chords, for key handlers (Set-1 scancodes and modifier bits).
   type Chord is (No_Chord, Copy_Chord, Cut_Chord, Paste_Chord);
   function Chord_Of (Code : Unsigned_64; Modifiers : Unsigned_64) return Chord;

   --  The full editing command of a key press, Select All included
   --  (Clipboard_Chords: the same table Desktop issues tickets from).
   function Command_Of
     (Code : Unsigned_64; Modifiers : Unsigned_64)
      return Clipboard_Chords.Command is
     (Clipboard_Chords.Command_Of
        (Clipboard_Chords.Key_Code (Code),
         Clipboard_Chords.Modifier_Bits (Modifiers and CuBit.UI.Input.KEY_MODIFIERS_MASK)));
end CuBit.UI.Clipboard;
