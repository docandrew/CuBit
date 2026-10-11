------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The editing chords of every CuBit text surface (docs/clipboard.md)
--
--  One classification shared by Desktop, which issues the clipboard tickets
--  as it routes keys, and by the toolkit, which acts on the same keys in the
--  focused app. Both sides call Command_Of, so the chord an app treats as
--  "copy" is exactly the chord Desktop issued a copy ticket for.
--
--  Proved (level 2, tests/clipboard): Command_Of is exactly the chord table
--  in its postcondition; a paste ticket comes only from a paste chord; a copy
--  ticket only from a copy or cut chord, or (Activation_Ticket) from a
--  pointer press or an activation key on the focused surface.
------------------------------------------------------------------------------
with Interfaces;

package Clipboard_Chords with SPARK_Mode, Pure is

   --  Set-1 scancode of a key press (the desktop protocol's payload0).
   type Key_Code is new Interfaces.Unsigned_64;
   --  KEYMOD_* bits of the same event (payload1's low word).
   type Modifier_Bits is new Interfaces.Unsigned_64;

   Key_A      : constant Key_Code := 16#1E#;
   Key_C      : constant Key_Code := 16#2E#;
   Key_X      : constant Key_Code := 16#2D#;
   Key_V      : constant Key_Code := 16#2F#;
   Key_Enter  : constant Key_Code := 16#1C#;
   Key_Insert : constant Key_Code := 16#52#;
   Key_Delete : constant Key_Code := 16#53#;

   Shift_Bit : constant Modifier_Bits := 1;
   Ctrl_Bit  : constant Modifier_Bits := 2;
   Alt_Bit   : constant Modifier_Bits := 4;

   function Has (Modifiers, Bit : Modifier_Bits) return Boolean is
     ((Modifiers and Bit) /= 0);

   type Command is
     (No_Command,
      Copy_Command,         --  Ctrl+C, Ctrl+Insert
      Cut_Command,          --  Ctrl+X, Shift+Delete
      Paste_Command,        --  Ctrl+V, Shift+Insert
      Select_All_Command);  --  Ctrl+A

   function Command_Of
     (Code : Key_Code; Modifiers : Modifier_Bits) return Command
   with Post => Command_Of'Result =
     (if Has (Modifiers, Alt_Bit) then No_Command
      elsif Has (Modifiers, Ctrl_Bit) and then Code in Key_C | Key_Insert
      then Copy_Command
      elsif (Has (Modifiers, Ctrl_Bit) and then Code = Key_X) or else
        (Has (Modifiers, Shift_Bit) and then not Has (Modifiers, Ctrl_Bit)
         and then Code = Key_Delete)
      then Cut_Command
      elsif (Has (Modifiers, Ctrl_Bit) and then Code = Key_V) or else
        (Has (Modifiers, Shift_Bit) and then not Has (Modifiers, Ctrl_Bit)
         and then Code = Key_Insert)
      then Paste_Command
      elsif Has (Modifiers, Ctrl_Bit) and then
        not Has (Modifiers, Shift_Bit) and then Code = Key_A
      then Select_All_Command
      else No_Command);

   --  The clipboard authority a key press gives the focused surface.
   type Ticket is (No_Ticket, Copy_Ticket, Paste_Ticket);

   --  The key that activates the focused menu row or button: Enter, with
   --  neither Ctrl nor Alt. Like a pointer press it is the user's own
   --  gesture, so it may authorize a copy from a menu, never a paste.
   function Is_Activation_Key
     (Code : Key_Code; Modifiers : Modifier_Bits) return Boolean is
     (Code = Key_Enter and then
      not Has (Modifiers, Ctrl_Bit) and then not Has (Modifiers, Alt_Bit));

   function Ticket_Of
     (Code : Key_Code; Modifiers : Modifier_Bits) return Ticket
   with Post =>
     (case Ticket_Of'Result is
        when Paste_Ticket =>
          Command_Of (Code, Modifiers) = Paste_Command,
        when Copy_Ticket =>
          Command_Of (Code, Modifiers) in Copy_Command | Cut_Command
          or else Is_Activation_Key (Code, Modifiers),
        when No_Ticket =>
          Command_Of (Code, Modifiers) not in
            Copy_Command | Cut_Command | Paste_Command
          and then not Is_Activation_Key (Code, Modifiers));

   --  A pointer press on the focused surface (a menu row, a Copy button):
   --  a copy ticket, never a paste ticket.
   Activation_Ticket : constant Ticket := Copy_Ticket;
end Clipboard_Chords;
