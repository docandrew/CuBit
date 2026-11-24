------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Keys with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   --  Set-1 make codes (the IBM PC/AT scancode set 1 as PS/2 keyboards
   --  and the USB HID boot-protocol translation send them).
   type Code_Table is array (Key) of Scancode;
   CODES : constant Code_Table :=
     [Unknown => 16#00#,
      Escape => 16#01#, Digit_1 => 16#02#, Digit_2 => 16#03#, Digit_3 => 16#04#, Digit_4 => 16#05#,
      Digit_5 => 16#06#, Digit_6 => 16#07#, Digit_7 => 16#08#, Digit_8 => 16#09#, Digit_9 => 16#0A#,
      Digit_0 => 16#0B#, Minus => 16#0C#, Equals => 16#0D#, Backspace => 16#0E#, Tab => 16#0F#,
      Q => 16#10#, W => 16#11#, E => 16#12#, R => 16#13#, T => 16#14#, Y => 16#15#, U => 16#16#,
      I => 16#17#, O => 16#18#, P => 16#19#, Left_Bracket => 16#1A#, Right_Bracket => 16#1B#,
      Enter => 16#1C#, Control_Key => 16#1D#,
      A => 16#1E#, S => 16#1F#, D => 16#20#, F => 16#21#, G => 16#22#, H => 16#23#, J => 16#24#,
      K => 16#25#, L => 16#26#, Semicolon => 16#27#, Apostrophe => 16#28#, Grave => 16#29#,
      Left_Shift_Key => 16#2A#, Backslash => 16#2B#,
      Z => 16#2C#, X => 16#2D#, C => 16#2E#, V => 16#2F#, B => 16#30#, N => 16#31#, M => 16#32#,
      Comma => 16#33#, Period => 16#34#, Slash => 16#35#, Right_Shift_Key => 16#36#,
      Keypad_Multiply => 16#37#, Alt_Key => 16#38#, Space => 16#39#, Caps_Lock => 16#3A#,
      F1 => 16#3B#, F2 => 16#3C#, F3 => 16#3D#, F4 => 16#3E#, F5 => 16#3F#, F6 => 16#40#,
      F7 => 16#41#, F8 => 16#42#, F9 => 16#43#, F10 => 16#44#, Num_Lock => 16#45#,
      Scroll_Lock => 16#46#, Home => 16#47#, Up => 16#48#, Page_Up => 16#49#,
      Keypad_Minus => 16#4A#, Left => 16#4B#, Keypad_5 => 16#4C#, Right => 16#4D#,
      Keypad_Plus => 16#4E#, End_Key => 16#4F#, Down => 16#50#, Page_Down => 16#51#,
      Insert => 16#52#, Delete => 16#53#, F11 => 16#57#, F12 => 16#58#,
      Super_Key => 16#5B#, Menu => 16#5D#];

   function Scancode_Of (Item : Key) return Scancode is (CODES (Item));

   --  The inverse, computed by search so it is the inverse by construction.
   function Key_Of (Code : Interfaces.Unsigned_64) return Key is
   begin
      if Code = 0 or else Code > MAXIMUM_SCANCODE then
         return Unknown;
      end if;
      for Item in Key loop
         if CODES (Item) = Code then
            return Item;
         end if;
         pragma Loop_Invariant (for all J in Key'First .. Item => CODES (J) /= Code);
      end loop;
      return Unknown;
   end Key_Of;

   function Modifier_Of (Item : Key) return Modifier is
     (case Item is
         when Control_Key => Ctrl,
         when Left_Shift_Key | Right_Shift_Key => Shift,
         when Alt_Key => Alt,
         when others => Super);

   function Modifiers_Of (Bits : Interfaces.Unsigned_64) return Modifier_Set is
     [Shift => (Bits and WIRE_SHIFT) /= 0,
      Ctrl => (Bits and WIRE_CTRL) /= 0,
      Alt => (Bits and WIRE_ALT) /= 0,
      Super => False];

   function "+" (Left, Right : Modifier) return Modifier_Set is
      Result : Modifier_Set := No_Modifiers;
   begin
      Result (Left) := True;
      Result (Right) := True;
      return Result;
   end "+";

   function "+" (Left : Modifier_Set; Right : Modifier) return Modifier_Set is
      Result : Modifier_Set := Left;
   begin
      Result (Right) := True;
      return Result;
   end "+";

   function "+" (Left : Modifier; Right : Modifier_Set) return Modifier_Set is (Right + Left);

   function "+" (Item : Key) return Chord is ((Modifiers => No_Modifiers, Pressed => Item));

   function "+" (Left : Modifier; Right : Key) return Chord is
      Result : Chord := (Modifiers => No_Modifiers, Pressed => Right);
   begin
      Result.Modifiers (Left) := True;
      return Result;
   end "+";

   function "+" (Left : Key; Right : Modifier) return Chord is (Right + Left);

   function "+" (Left : Modifier_Set; Right : Key) return Chord is ((Modifiers => Left, Pressed => Right));

   function "+" (Left : Chord; Right : Modifier) return Chord is
     ((Modifiers => Left.Modifiers + Right, Pressed => Left.Pressed));

   function "+" (Left : Modifier; Right : Chord) return Chord is (Right + Left);

   function Chord_Of (Code, Modifier_Bits : Interfaces.Unsigned_64) return Chord is
     ((Modifiers => Modifiers_Of (Modifier_Bits), Pressed => Key_Of (Code)));

   function Label (Item : Key) return String is
     (case Item is
         when Unknown => "?",
         when Escape => "Esc", when Digit_1 => "1", when Digit_2 => "2", when Digit_3 => "3",
         when Digit_4 => "4", when Digit_5 => "5", when Digit_6 => "6", when Digit_7 => "7",
         when Digit_8 => "8", when Digit_9 => "9", when Digit_0 => "0", when Minus => "-",
         when Equals => "=", when Backspace => "Backspace", when Tab => "Tab",
         when Q => "Q", when W => "W", when E => "E", when R => "R", when T => "T", when Y => "Y",
         when U => "U", when I => "I", when O => "O", when P => "P", when Left_Bracket => "[",
         when Right_Bracket => "]", when Enter => "Enter", when Control_Key => "Ctrl",
         when A => "A", when S => "S", when D => "D", when F => "F", when G => "G", when H => "H",
         when J => "J", when K => "K", when L => "L", when Semicolon => ";", when Apostrophe => "'",
         when Grave => "`", when Left_Shift_Key | Right_Shift_Key => "Shift", when Backslash => "\",
         when Z => "Z", when X => "X", when C => "C", when V => "V", when B => "B", when N => "N",
         when M => "M", when Comma => ",", when Period => ".", when Slash => "/",
         when Keypad_Multiply => "Num *", when Alt_Key => "Alt", when Space => "Space",
         when Caps_Lock => "Caps Lock", when F1 => "F1", when F2 => "F2", when F3 => "F3",
         when F4 => "F4", when F5 => "F5", when F6 => "F6", when F7 => "F7", when F8 => "F8",
         when F9 => "F9", when F10 => "F10", when Num_Lock => "Num Lock", when Scroll_Lock => "Scr Lock",
         when Home => "Home", when Up => "Up", when Page_Up => "PgUp", when Keypad_Minus => "Num -",
         when Left => "Left", when Keypad_5 => "Num 5", when Right => "Right", when Keypad_Plus => "Num +",
         when End_Key => "End", when Down => "Down", when Page_Down => "PgDn", when Insert => "Ins",
         when Delete => "Del", when F11 => "F11", when F12 => "F12", when Super_Key => "Super",
         when Menu => "Menu");

   function Label (Item : Modifier) return String is
     (case Item is
         when Shift => "Shift", when Ctrl => "Ctrl", when Alt => "Alt", when Super => "Super");

   function Image (Item : Chord) return String is
      PART : constant := MAXIMUM_LABEL + 1;  --  a label and its "+"
      Buffer : String (1 .. DISPLAY_ORDER'Length * PART + MAXIMUM_LABEL) := [others => ' '];
      Last : Natural range 0 .. DISPLAY_ORDER'Length * PART := 0;
   begin
      if not Is_Chord (Item) then
         return "";
      end if;
      for Position in DISPLAY_ORDER'Range loop
         pragma Loop_Invariant (Last <= (Position - 1) * PART);
         if Item.Modifiers (DISPLAY_ORDER (Position)) then
            declare
               Text : constant String := Label (DISPLAY_ORDER (Position)) & "+";
            begin
               Buffer (Last + 1 .. Last + Text'Length) := Text;
               Last := Last + Text'Length;
            end;
         end if;
      end loop;
      declare
         Text : constant String := Label (Item.Pressed);
      begin
         Buffer (Last + 1 .. Last + Text'Length) := Text;
         return Buffer (1 .. Last + Text'Length);
      end;
   end Image;
end CuBit.UI.Keys;
