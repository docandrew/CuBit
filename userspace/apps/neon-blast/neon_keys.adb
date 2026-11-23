------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Neon_Keys with SPARK_Mode is

   --  Set 1 make codes, in Key order from Escape (IBM PC AT scan code set 1).
   Last_Code : constant := 16#58#;
   type Code_Table is array (Known_Key) of Unsigned_64;
   Codes : constant Code_Table :=
     [Escape => 16#01#,
      Digit_1 => 16#02#, Digit_2 => 16#03#, Digit_3 => 16#04#, Digit_4 => 16#05#,
      Digit_5 => 16#06#, Digit_6 => 16#07#, Digit_7 => 16#08#, Digit_8 => 16#09#,
      Digit_9 => 16#0A#, Digit_0 => 16#0B#, Minus => 16#0C#, Equals => 16#0D#,
      Backspace => 16#0E#, Tab => 16#0F#,
      Q => 16#10#, W => 16#11#, E => 16#12#, R => 16#13#, T => 16#14#, Y => 16#15#,
      U => 16#16#, I => 16#17#, O => 16#18#, P => 16#19#, Left_Bracket => 16#1A#,
      Right_Bracket => 16#1B#, Enter => 16#1C#, Left_Control => 16#1D#,
      A => 16#1E#, S => 16#1F#, D => 16#20#, F => 16#21#, G => 16#22#, H => 16#23#,
      J => 16#24#, K => 16#25#, L => 16#26#, Semicolon => 16#27#, Apostrophe => 16#28#,
      Grave => 16#29#, Left_Shift => 16#2A#, Backslash => 16#2B#,
      Z => 16#2C#, X => 16#2D#, C => 16#2E#, V => 16#2F#, B => 16#30#, N => 16#31#,
      M => 16#32#, Comma => 16#33#, Period => 16#34#, Slash => 16#35#,
      Right_Shift => 16#36#, Keypad_Multiply => 16#37#, Left_Alt => 16#38#, Space => 16#39#,
      Caps_Lock => 16#3A#, F1 => 16#3B#, F2 => 16#3C#, F3 => 16#3D#, F4 => 16#3E#,
      F5 => 16#3F#, F6 => 16#40#, F7 => 16#41#, F8 => 16#42#, F9 => 16#43#, F10 => 16#44#,
      Num_Lock => 16#45#, Scroll_Lock => 16#46#, Home => 16#47#, Up => 16#48#,
      Page_Up => 16#49#, Keypad_Minus => 16#4A#, Left => 16#4B#, Keypad_5 => 16#4C#,
      Right => 16#4D#, Keypad_Plus => 16#4E#, End_Key => 16#4F#, Down => 16#50#,
      Page_Down => 16#51#, Insert => 16#52#, Delete => 16#53#, F11 => 16#57#, F12 => 16#58#];

   function Code_Of (K : Key) return Unsigned_64 is
     (if K = Unknown then 0 else Codes (K));

   function Decode (Wire_Code : Unsigned_64) return Key is
   begin
      if Wire_Code = 0 or else Wire_Code > Last_Code then
         return Unknown;
      end if;
      for Candidate in Known_Key loop
         if Codes (Candidate) = Wire_Code then
            return Candidate;
         end if;
      end loop;
      return Unknown;
   end Decode;

   function Label (Item : Key) return String is
   begin
      case Item is
         when Unknown => return "?";
         when Escape => return "ESC";
         when Digit_1 => return "1";
         when Digit_2 => return "2";
         when Digit_3 => return "3";
         when Digit_4 => return "4";
         when Digit_5 => return "5";
         when Digit_6 => return "6";
         when Digit_7 => return "7";
         when Digit_8 => return "8";
         when Digit_9 => return "9";
         when Digit_0 => return "0";
         when Minus => return "-";
         when Equals => return "=";
         when Backspace => return "BKSP";
         when Tab => return "TAB";
         when Q => return "Q";
         when W => return "W";
         when E => return "E";
         when R => return "R";
         when T => return "T";
         when Y => return "Y";
         when U => return "U";
         when I => return "I";
         when O => return "O";
         when P => return "P";
         when Left_Bracket => return "[";
         when Right_Bracket => return "]";
         when Enter => return "ENTER";
         when Left_Control => return "CTRL";
         when A => return "A";
         when S => return "S";
         when D => return "D";
         when F => return "F";
         when G => return "G";
         when H => return "H";
         when J => return "J";
         when K => return "K";
         when L => return "L";
         when Semicolon => return ";";
         when Apostrophe => return "'";
         when Grave => return "`";
         when Left_Shift => return "LSHIFT";
         when Backslash => return "\";
         when Z => return "Z";
         when X => return "X";
         when C => return "C";
         when V => return "V";
         when B => return "B";
         when N => return "N";
         when M => return "M";
         when Comma => return ",";
         when Period => return ".";
         when Slash => return "/";
         when Right_Shift => return "RSHIFT";
         when Keypad_Multiply => return "KP*";
         when Left_Alt => return "ALT";
         when Space => return "SPACE";
         when Caps_Lock => return "CAPS";
         when F1 => return "F1";
         when F2 => return "F2";
         when F3 => return "F3";
         when F4 => return "F4";
         when F5 => return "F5";
         when F6 => return "F6";
         when F7 => return "F7";
         when F8 => return "F8";
         when F9 => return "F9";
         when F10 => return "F10";
         when Num_Lock => return "NUMLK";
         when Scroll_Lock => return "SCRLK";
         when Home => return "HOME";
         when Up => return "UP";
         when Page_Up => return "PGUP";
         when Keypad_Minus => return "KP-";
         when Left => return "LEFT";
         when Keypad_5 => return "KP5";
         when Right => return "RIGHT";
         when Keypad_Plus => return "KP+";
         when End_Key => return "END";
         when Down => return "DOWN";
         when Page_Down => return "PGDN";
         when Insert => return "INS";
         when Delete => return "DEL";
         when F11 => return "F11";
         when F12 => return "F12";
      end case;
   end Label;

   function Held_Count (B : Board) return Natural is
      Count : Natural := 0;
   begin
      for K in Key loop
         pragma Loop_Invariant (Count <= Key'Pos (K));
         if B.Down (K) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Held_Count;

   procedure Press (B : in out Board; K : Key; Repeat : Boolean; At_Time : Timestamp) is
   begin
      if K = Unknown then
         return;
      end if;
      --  A repeat of a held key changes nothing; a "repeat" of a key the
      --  board thinks is up (its press was lost) still holds it.
      if Repeat and then B.Down (K) then
         return;
      end if;
      if not B.Down (K) then
         B.Down (K) := True;
         B.Pressed (K) := True;
         B.Down_At (K) := At_Time;
      end if;
   end Press;

   procedure Release (B : in out Board; K : Key; At_Time : Timestamp) is
   begin
      if B.Down (K) then
         B.Released (K) := True;
         B.Up_At (K) := At_Time;
      end if;
      B.Down (K) := False;
   end Release;

   procedure Release_All (B : in out Board; At_Time : Timestamp) is
   begin
      for K in Key loop
         Release (B, K, At_Time);
         pragma Loop_Invariant (for all J in Key'First .. K => not Held (B, J));
      end loop;
   end Release_All;

   procedure Take_Snapshot (B : in out Board; Into : out Snapshot) is
   begin
      Into := (Down => B.Down, Pressed => B.Pressed, Released => B.Released,
               Down_At => B.Down_At, Up_At => B.Up_At);
      B.Pressed := No_Keys;
      B.Released := No_Keys;
   end Take_Snapshot;

end Neon_Keys;
