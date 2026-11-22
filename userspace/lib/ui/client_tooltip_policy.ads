------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  When a tooltip shows (CuBit.UI.Tooltips): after the pointer rests on a
--  target for SHOW_DELAY_MS; at once on the next target while one is shown
--  (sliding along a toolbar); gone when the pointer leaves, and dismissed
--  by a click, key, scroll or deactivation until the pointer reaches another
--  target. Time comes from the caller (its event loop's deadline), so an
--  idle window still sleeps. Pure policy, proved (tests/ui-popups).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Client_Tooltip_Policy with SPARK_Mode, Pure is
   SHOW_DELAY_MS : constant := 500;
   NEVER : constant Unsigned_64 := Unsigned_64'Last;
   --  A target is the application's identity for what is under the
   --  pointer (a control ID, a region number); NO_TARGET is nothing.
   subtype Target is Natural;
   NO_TARGET : constant Target := 0;

   type Phase is (Idle, Pending, Shown);
   type Tooltip_State is record
      Current : Phase := Idle;
      On : Target := NO_TARGET;
      Since : Unsigned_64 := 0;
      X, Y : Natural := 0;
      --  Dismissed while over this target: it stays hidden until another.
      Dismissed : Target := NO_TARGET;
   end record;

   function Showing (S : Tooltip_State) return Boolean is (S.Current = Shown);

   --  The pointer at (X, Y) over Over at Now_Ms.
   procedure Pointer_At (S : in out Tooltip_State; Over : Target; X, Y : Natural; Now_Ms : Unsigned_64)
     with Post => (if Over = NO_TARGET then S.Current = Idle)
                  and then (if Over /= NO_TARGET and then S'Old.Current = Shown and then Over /= S'Old.On
                              and then Over /= S'Old.Dismissed then S.Current = Shown and then S.On = Over);
   --  A click, key, scroll or deactivation: hide, and stay hidden on this
   --  target.
   procedure Dismiss (S : in out Tooltip_State)
     with Post => S.Current = Idle;
   --  Time passing. Changed: show or hide happened.
   procedure Tick (S : in out Tooltip_State; Now_Ms : Unsigned_64; Changed : out Boolean)
     with Post => (if Changed then S.Current = Shown and then S'Old.Current = Pending);
   --  When Tick next has work (NEVER: nothing pending).
   function Next_Deadline (S : Tooltip_State) return Unsigned_64 is
     (if S.Current = Pending then
        (if S.Since > NEVER - SHOW_DELAY_MS then NEVER else S.Since + SHOW_DELAY_MS)
      else NEVER);
end Client_Tooltip_Policy;
