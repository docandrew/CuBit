------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Hover tooltips for any control: the application gives a control (by its
--  Control_ID) or a region (a truncated cell) tooltip text and an optional
--  shortcut (a typed chord, from the keymap's Hint) while it renders; the toolkit decides when the tip shows
--  (Client_Tooltip_Policy, proved), places it near the pointer on screen
--  (Client_Popup_Layout, proved) and draws it last: a flat, square box with
--  a 1-pixel theme border, the shortcut's keycaps after the text
--  (CuBit.UI.Keycaps: "Copy  [F5]").
--  Keyboard users get the focused control's tip after the same delay,
--  below the control. It never takes focus or input: the application feeds
--  pointer motion, focus changes, dismissing events and its clock, and
--  includes Next_Deadline in its loop's wait (never a sleep). Damage is the
--  tip's own rectangle (shown, moved or hidden).
------------------------------------------------------------------------------
with Interfaces;
with CuBit.UI.Controls;
with CuBit.UI.Keys;
with CuBit.UI.Keycaps;
with Client_Tooltip_Policy;

package CuBit.UI.Tooltips is
   MAXIMUM_TIPS : constant := 96;
   MAXIMUM_TEXT : constant := 96;

   type Tip_Table is private;
   --  Each frame: Clear, then Set for every control and region that has a
   --  tip (regions are looked up before controls).
   procedure Clear (Table : in out Tip_Table);
   procedure Set
     (Table : in out Tip_Table; Id : Controls.Control_ID; Text : String;
      Hint : CuBit.UI.Keys.Chord := CuBit.UI.Keys.No_Chord;
      Shade : CuBit.UI.Keycaps.Tone := CuBit.UI.Keycaps.Neutral);
   procedure Set_Region
     (Table : in out Tip_Table; Area : Rect; Text : String;
      Hint : CuBit.UI.Keys.Chord := CuBit.UI.Keys.No_Chord;
      Shade : CuBit.UI.Keycaps.Tone := CuBit.UI.Keycaps.Neutral);

   type Tooltip is private;
   function Visible (Tip : Tooltip) return Boolean;
   --  The pointer at (X, Y); Map is the frame's control map.
   procedure Pointer_At
     (Tip : in out Tooltip; Table : Tip_Table; Map : Controls.Control_Map; X, Y : Natural; Now_Ms : Unsigned_64;
      Damage : out Rect);
   --  The keyboard focus is on control Id (Controls.NO_CONTROL: on none
   --  with a tip). Call after Dismiss for the key that moved it.
   procedure Focus_On
     (Tip : in out Tooltip; Table : Tip_Table; Map : Controls.Control_Map; Id : Controls.Control_ID;
      Now_Ms : Unsigned_64; Damage : out Rect);
   --  A click, key, scroll or deactivation.
   procedure Dismiss (Tip : in out Tooltip; Damage : out Rect);
   procedure Tick (Tip : in out Tooltip; Table : Tip_Table; Now_Ms : Unsigned_64; Damage : out Rect);
   function Next_Deadline (Tip : Tooltip) return Unsigned_64;
   --  Where the tip is (empty while hidden), within Area at the last Draw.
   --  Damage from Pointer_At, Dismiss and Tick covers the old and new boxes.
   function Area_Of (Tip : Tooltip) return Rect;
   --  Draw after everything else; Area keeps the tip on screen.
   procedure Draw (C : Canvas; Tip : in out Tooltip; Table : Tip_Table; Area : Rect; Colors : Theme);
   --  The text of the tip showing (for tests), "" while hidden.
   function Text (Tip : Tooltip; Table : Tip_Table) return String;
   --  The shortcut of the tip showing (for tests), No_Chord while hidden.
   function Hint (Tip : Tooltip; Table : Tip_Table) return CuBit.UI.Keys.Chord;
private
   type Tip_Kind is (Control_Tip, Region_Tip);
   type Tip_Entry is record
      Kind : Tip_Kind := Control_Tip;
      Id : Controls.Control_ID := Controls.NO_CONTROL;
      Area : Rect := (others => 0);
      Text : String (1 .. MAXIMUM_TEXT) := [others => ' '];
      Text_Length : Natural range 0 .. MAXIMUM_TEXT := 0;
      Hint : CuBit.UI.Keys.Chord := CuBit.UI.Keys.No_Chord;
      Shade : CuBit.UI.Keycaps.Tone := CuBit.UI.Keycaps.Neutral;
   end record;
   type Tip_Entries is array (1 .. MAXIMUM_TIPS) of Tip_Entry;
   type Tip_Table is record
      Entries : Tip_Entries;
      Count : Natural range 0 .. MAXIMUM_TIPS := 0;
   end record;
   type Tooltip is record
      Policy : Client_Tooltip_Policy.Tooltip_State;
      --  Where the tip is, and the bounds it was last placed within (Draw).
      Box : Rect := (others => 0);
      Area : Rect := (others => 0);
      --  The focused control a keyboard tip hangs below (empty: the tip
      --  follows the pointer).
      Below : Rect := (others => 0);
   end record;
end CuBit.UI.Tooltips;
