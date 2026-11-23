------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's typed keys: physical key positions, the held-key board and
--  per-frame snapshots. A local prototype of UI-049 (CuBit.UI.Keys),
--  written to move into the toolkit unchanged.
--
--  @description
--  A Key names a physical position (Key.W is the key in QWERTY's W place
--  whatever the layout), decoded once from the desktop's wire code (a set 1
--  make code; the desktop folds the E0 prefix away today, so the arrows and
--  the keypad's 8/4/6/2 share codes). Nothing outside Decode sees a scan
--  code.
--
--  The Board keeps which keys are held and when each went down and up.
--  Auto-repeat presses are marked by the desktop and ignored here (a held
--  key is one press). Release_All ends every hold: call it when the window
--  may have lost keys (a resynchronised input queue, focus loss once the
--  desktop reports it), so no key sticks.
--
--  Take_Snapshot gives a frame its view: what is down now, and what went
--  down or up since the last snapshot, so a tap shorter than a frame still
--  counts once.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Neon_Keys with SPARK_Mode, Pure is

   type Key is
     (Unknown, Escape,
      Digit_1, Digit_2, Digit_3, Digit_4, Digit_5, Digit_6, Digit_7, Digit_8, Digit_9, Digit_0,
      Minus, Equals, Backspace, Tab,
      Q, W, E, R, T, Y, U, I, O, P, Left_Bracket, Right_Bracket, Enter, Left_Control,
      A, S, D, F, G, H, J, K, L, Semicolon, Apostrophe, Grave, Left_Shift, Backslash,
      Z, X, C, V, B, N, M, Comma, Period, Slash, Right_Shift, Keypad_Multiply, Left_Alt, Space,
      Caps_Lock, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, Num_Lock, Scroll_Lock,
      Home, Up, Page_Up, Keypad_Minus, Left, Keypad_5, Right, Keypad_Plus, End_Key, Down,
      Page_Down, Insert, Delete, F11, F12);
   subtype Known_Key is Key range Escape .. F12;

   --  The key at a desktop wire code (payload0 of a key event).
   function Decode (Wire_Code : Unsigned_64) return Key;
   --  Its set 1 make code (Unknown: 0). Decode (Code_Of (K)) = K.
   function Code_Of (K : Key) return Unsigned_64;
   --  A short label for the held-keys strip ("W", "SPACE", "LSHIFT").
   function Label (Item : Key) return String
     with Post => Label'Result'Length in 1 .. 8;

   --  Microseconds on the caller's monotonic clock.
   type Timestamp is new Unsigned_64;

   type Key_Set is array (Key) of Boolean;
   No_Keys : constant Key_Set := [others => False];
   type Key_Times is array (Key) of Timestamp;

   type Board is private;
   function Empty_Board return Board;
   function Held (B : Board; K : Key) return Boolean;
   --  How many keys are held now (rollover in use).
   function Held_Count (B : Board) return Natural;

   --  A press (Repeat: the desktop's auto-repeat flag, ignored) or a release.
   procedure Press (B : in out Board; K : Key; Repeat : Boolean; At_Time : Timestamp)
     with Post => (if K /= Unknown then Held (B, K));
   procedure Release (B : in out Board; K : Key; At_Time : Timestamp)
     with Post => not Held (B, K)
                  and then (for all J in Key => (if J /= K then Held (B, J) = Held (B'Old, J)));
   --  Release every held key at At_Time (input lost or focus gone).
   procedure Release_All (B : in out Board; At_Time : Timestamp)
     with Post => (for all K in Key => not Held (B, K));

   type Snapshot is record
      Down : Key_Set := No_Keys;      --  held at the snapshot
      Pressed : Key_Set := No_Keys;   --  went down since the last snapshot
      Released : Key_Set := No_Keys;  --  went up since the last snapshot
      Down_At : Key_Times := [others => 0];  --  last press of each key
      Up_At : Key_Times := [others => 0];    --  last release of each key
   end record;

   --  The frame's view; clears the since-last-snapshot sets.
   procedure Take_Snapshot (B : in out Board; Into : out Snapshot);
   function Is_Down (S : Snapshot; K : Key) return Boolean is (S.Down (K));
   --  Down now, or tapped (down and up) since the last snapshot.
   function Active (S : Snapshot; K : Key) return Boolean is (S.Down (K) or else S.Pressed (K));
   function Was_Pressed (S : Snapshot; K : Key) return Boolean is (S.Pressed (K));

private
   type Board is record
      Down, Pressed, Released : Key_Set := No_Keys;
      Down_At, Up_At : Key_Times := [others => 0];
   end record;
   function Empty_Board return Board is ((others => <>));
   function Held (B : Board; K : Key) return Boolean is (B.Down (K));
end Neon_Keys;
