------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  One typed key vocabulary (UI-049): physical keys, modifier sets and
--  chords, decoded once from the desktop protocol's wire scancode.
--
--  @description
--  A Key is a physical position, not a letter: Keys.W is the key in the
--  QWERTY W position whatever the layout, so WASD works on any layout.
--  Typed characters come separately, as text events after the layout.
--
--  Chords are values built with "+": a modifier and a key, a key and a
--  modifier, two modifiers (a Modifier_Set) and a key, a chord and one more
--  modifier. So Keys.Ctrl + Keys.S and Keys.Ctrl + Keys.Shift + Keys.Z are
--  chords; a lone key is the chord +Keys.F5. Key + Key is not defined, so it
--  does not compile. Adding a modifier is idempotent and order-independent
--  (the postconditions), so Ctrl + Shift + Z = Shift + Ctrl + Z.
--
--  Two layers (UI-052): chords with Super belong to the environment (the
--  Desktop owns them; apps never see them and cannot bind them); Ctrl, Alt
--  and the function keys are the application's. The secure attention chord
--  (UI-045) belongs to neither: nobody can bind it.
--
--  Proved (tests/ui-keys, level 2): the operators' postconditions and
--  total decoding. Tested: decoding a key's own scancode gives it back.
------------------------------------------------------------------------------
with Interfaces;

package CuBit.UI.Keys with SPARK_Mode is

   --  Physical keys, in set-1 scancode order (the desktop protocol's
   --  payload0 of a key event). The modifier keys themselves are keys too
   --  (their presses and releases are events), named *_Key.
   type Key is
     (Unknown,
      Escape, Digit_1, Digit_2, Digit_3, Digit_4, Digit_5, Digit_6, Digit_7, Digit_8, Digit_9, Digit_0,
      Minus, Equals, Backspace, Tab,
      Q, W, E, R, T, Y, U, I, O, P, Left_Bracket, Right_Bracket, Enter, Control_Key,
      A, S, D, F, G, H, J, K, L, Semicolon, Apostrophe, Grave, Left_Shift_Key, Backslash,
      Z, X, C, V, B, N, M, Comma, Period, Slash, Right_Shift_Key, Keypad_Multiply, Alt_Key, Space, Caps_Lock,
      F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, Num_Lock, Scroll_Lock,
      Home, Up, Page_Up, Keypad_Minus, Left, Keypad_5, Right, Keypad_Plus, End_Key, Down, Page_Down,
      Insert, Delete, F11, F12, Super_Key, Menu);

   subtype Letter is Key with Static_Predicate =>
     Letter in Q | W | E | R | T | Y | U | I | O | P | A | S | D | F | G | H | J | K | L
             | Z | X | C | V | B | N | M;
   subtype Function_Key is Key range F1 .. F10;  --  F11 and F12 follow below
   function Is_Function_Key (Item : Key) return Boolean is (Item in Function_Key | F11 | F12);

   --  The wire's scancode: set 1, the release bit and the E0 prefix
   --  already taken off by the Desktop (arrows share codes with the keypad).
   MAXIMUM_SCANCODE : constant := 16#7F#;
   subtype Scancode is Interfaces.Unsigned_64 range 0 .. MAXIMUM_SCANCODE;

   function Key_Of (Code : Interfaces.Unsigned_64) return Key;
   --  The scancode a key sends (Unknown: 0, sent by no key).
   function Scancode_Of (Item : Key) return Scancode;

   --  Decoding a key's own code gives it back (tested exhaustively,
   --  tests/ui-keys: the code table is one-to-one).

   --  Modifiers and sets of them.
   type Modifier is (Shift, Ctrl, Alt, Super);
   type Modifier_Set is array (Modifier) of Boolean with Pack;
   No_Modifiers : constant Modifier_Set := [others => False];

   function Is_Empty (Set : Modifier_Set) return Boolean is (Set = No_Modifiers);

   --  The modifier a modifier key holds, if it is one.
   function Is_Modifier_Key (Item : Key) return Boolean is
     (Item in Control_Key | Left_Shift_Key | Right_Shift_Key | Alt_Key | Super_Key);
   function Modifier_Of (Item : Key) return Modifier
   with Pre => Is_Modifier_Key (Item);

   --  The desktop protocol's modifier bits (payload1's low word of a key
   --  event, the high word of a pointer event's). Super never travels to
   --  an app: the Desktop owns it.
   WIRE_SHIFT : constant Interfaces.Unsigned_64 := 1;
   WIRE_CTRL  : constant Interfaces.Unsigned_64 := 2;
   WIRE_ALT   : constant Interfaces.Unsigned_64 := 4;
   function Modifiers_Of (Bits : Interfaces.Unsigned_64) return Modifier_Set
   with Post => not Modifiers_Of'Result (Super);

   --  A chord: modifiers held and one key pressed. No_Chord is "none".
   type Chord is record
      Modifiers : Modifier_Set := No_Modifiers;
      Pressed : Key := Unknown;
   end record;
   No_Chord : constant Chord := (Modifiers => No_Modifiers, Pressed => Unknown);
   --  A chord names a key (not Unknown, not a modifier key by itself).
   function Is_Chord (Item : Chord) return Boolean is
     (Item.Pressed /= Unknown and then not Is_Modifier_Key (Item.Pressed));

   --  Building chords.
   function "+" (Left, Right : Modifier) return Modifier_Set
   with Post => (for all M in Modifier => "+"'Result (M) = (M = Left or else M = Right));
   function "+" (Left : Modifier_Set; Right : Modifier) return Modifier_Set
   with Post => (for all M in Modifier => "+"'Result (M) = (Left (M) or else M = Right));
   function "+" (Left : Modifier; Right : Modifier_Set) return Modifier_Set
   with Post => (for all M in Modifier => "+"'Result (M) = (Right (M) or else M = Left));
   function "+" (Item : Key) return Chord
   with Post => "+"'Result = (Modifiers => No_Modifiers, Pressed => Item);
   function "+" (Left : Modifier; Right : Key) return Chord
   with Post => "+"'Result.Pressed = Right
     and then (for all M in Modifier => "+"'Result.Modifiers (M) = (M = Left));
   function "+" (Left : Key; Right : Modifier) return Chord
   with Post => "+"'Result = Right + Left;
   function "+" (Left : Modifier_Set; Right : Key) return Chord
   with Post => "+"'Result = (Modifiers => Left, Pressed => Right);
   function "+" (Left : Chord; Right : Modifier) return Chord
   with Post => "+"'Result = (Modifiers => Left.Modifiers + Right, Pressed => Left.Pressed);
   function "+" (Left : Modifier; Right : Chord) return Chord
   with Post => "+"'Result = Right + Left;

   --  A key event's chord: its key and the modifiers held.
   function Chord_Of (Code, Modifier_Bits : Interfaces.Unsigned_64) return Chord
   with Post => Chord_Of'Result = (Modifiers => Modifiers_Of (Modifier_Bits), Pressed => Key_Of (Code));

   --  The layers (UI-052). The environment's chords hold Super; the
   --  application's never do.
   type Layer is (Application_Layer, Environment_Layer);
   function Layer_Of (Item : Chord) return Layer is
     (if Item.Modifiers (Super) then Environment_Layer else Application_Layer);

   --  The secure attention chord (UI-045): always the trusted path's,
   --  never any keymap's.
   Secure_Attention : constant Chord :=
     (Modifiers => [Ctrl | Alt => True, others => False], Pressed => Delete);
   function Is_Reserved (Item : Chord) return Boolean is (Item = Secure_Attention);

   --  What a keycap shows for a key ("F5", "Ctrl", "Esc", "Left") and for a
   --  modifier, and a chord as text ("Ctrl+Shift+C") for accessibility
   --  data and logs; never typed by hand.
   MAXIMUM_LABEL : constant := 9;
   function Label (Item : Key) return String
   with Post => Label'Result'Length in 1 .. MAXIMUM_LABEL and then Label'Result'First = 1;
   function Label (Item : Modifier) return String
   with Post => Label'Result'Length in 1 .. MAXIMUM_LABEL and then Label'Result'First = 1;
   --  The modifiers of a chord in display order (Super, Ctrl, Alt, Shift).
   type Modifier_Order is array (Positive range 1 .. 4) of Modifier;
   DISPLAY_ORDER : constant Modifier_Order := [Super, Ctrl, Alt, Shift];
   function Image (Item : Chord) return String;
end CuBit.UI.Keys;
