------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Holding a modifier reveals the shortcuts that use it (UI-051, UI-052):
--  one timing model for Ctrl, Shift, Alt and Super.
--
--  @description
--  Feed every key event (the modifier keys' own presses and releases
--  included) and the window's clock. After the modifiers have been held for
--  HOLD_DELAY_MS with no key pressed alongside them, Revealed is the set
--  held: the button bar shows that layer's bindings, icon-only buttons show
--  their keycaps, Alt shows the mnemonic underlines and Super the Desktop's
--  hints. A quick chord (Ctrl+S) never reveals anything, so fast typing
--  does not flicker; a chord pressed after the reveal hides it until every
--  modifier is released. Adding a modifier to a revealed set (Shift with
--  Ctrl held) switches the layer at once.
--
--  Proved (tests/ui-keys, level 2): nothing is revealed before the delay,
--  after a chord, or with nothing held, and what is revealed is exactly
--  what is held.
------------------------------------------------------------------------------
with Interfaces;
with CuBit.UI.Keys;

package CuBit.UI.Modifier_Reveal with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   package Keys renames CuBit.UI.Keys;
   use type Keys.Modifier_Set, Keys.Key;

   --  How long modifiers are held before their hints show. The same for
   --  every modifier, so a quick tap of Super still opens the Apps menu.
   HOLD_DELAY_MS : constant Interfaces.Unsigned_64 := 500;
   NEVER : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;

   type State is private;

   function Held (S : State) return Keys.Modifier_Set;
   --  A key was pressed while modifiers were held: the chord completed.
   function Chord_Used (S : State) return Boolean;
   --  When the held set last changed.
   function Since (S : State) return Interfaces.Unsigned_64;

   function Revealed (S : State; Now_Ms : Interfaces.Unsigned_64) return Keys.Modifier_Set is
     (if not Chord_Used (S) and then not Keys.Is_Empty (Held (S))
        and then Now_Ms >= Since (S) and then Now_Ms - Since (S) >= HOLD_DELAY_MS
      then Held (S) else Keys.No_Modifiers);

   function Is_Revealed (S : State; Now_Ms : Interfaces.Unsigned_64) return Boolean is
     (not Keys.Is_Empty (Revealed (S, Now_Ms)));

   --  One key event: the key, whether it went down, the modifiers the
   --  event says are held (a modifier key's own event may report them
   --  before or after itself; the key's own state wins).
   procedure Key_Event
     (S : in out State; Item : Keys.Key; Down : Boolean; Modifiers : Keys.Modifier_Set;
      Now_Ms : Interfaces.Unsigned_64)
   with Post =>
     (if not Keys.Is_Modifier_Key (Item) and then Down and then not Keys.Is_Empty (Held (S'Old))
      then not Is_Revealed (S, Now_Ms))
     and then (if Keys.Is_Empty (Held (S)) then not Chord_Used (S))
     and then (if Keys.Is_Modifier_Key (Item) then Held (S) (Keys.Modifier_Of (Item)) = Down);

   --  A click (or any other use) with the modifiers held: like a chord.
   procedure Note_Use (S : in out State)
   with Post => Held (S) = Held (S'Old) and then (if not Keys.Is_Empty (Held (S)) then Chord_Used (S));

   --  Focus left the window: nothing is held any more.
   procedure Release_All (S : out State)
   with Post => Keys.Is_Empty (Held (S)) and then not Chord_Used (S);

   --  When Revealed next changes without input (NEVER: it will not, as
   --  once it shows: a deadline already passed would wake a loop forever).
   function Next_Deadline (S : State; Now_Ms : Interfaces.Unsigned_64) return Interfaces.Unsigned_64 is
     (if Chord_Used (S) or else Keys.Is_Empty (Held (S)) then NEVER
      elsif Since (S) > NEVER - HOLD_DELAY_MS then NEVER
      elsif Now_Ms >= Since (S) + HOLD_DELAY_MS then NEVER
      else Since (S) + HOLD_DELAY_MS)
   with Post => Next_Deadline'Result = NEVER or else Next_Deadline'Result > Now_Ms;
private
   type State is record
      Held_Set : Keys.Modifier_Set := Keys.No_Modifiers;
      Used : Boolean := False;
      Changed_At : Interfaces.Unsigned_64 := 0;
   end record;
   function Held (S : State) return Keys.Modifier_Set is (S.Held_Set);
   function Chord_Used (S : State) return Boolean is (S.Used);
   function Since (S : State) return Interfaces.Unsigned_64 is (S.Changed_At);
end CuBit.UI.Modifier_Reveal;
