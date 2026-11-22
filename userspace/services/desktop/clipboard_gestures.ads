------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Single-use clipboard authority from the user's own key gestures
--
--  Desktop routes every key to the focused surface. When it routes a copy
--  chord (Ctrl+C, Ctrl+X, Ctrl+Insert, Shift+Delete) or a paste chord
--  (Ctrl+V, Shift+Insert) it issues a ticket for exactly that surface. The
--  app's following clipboard request redeems it. Nothing else does: no
--  background reads, no reads by a window that merely exists, no reuse.
--
--  Proved (level 2, tests/clipboard): Redeem grants only a live ticket of the
--  same kind for the same surface, issued no more than Window_Ms earlier and
--  not in the future; a grant consumes the ticket; Revoke leaves none.
------------------------------------------------------------------------------
with Interfaces;

package Clipboard_Gestures with SPARK_Mode, Pure is

   --  How long after the chord the app may redeem it. Generous for a busy
   --  app, short enough that a stale chord cannot be harvested later.
   Window_Ms : constant := 2_000;

   type Millisecond is new Interfaces.Unsigned_64;
   type Surface_Name is new Interfaces.Unsigned_64;
   No_Surface : constant Surface_Name := 0;
   subtype Live_Surface is Surface_Name range 1 .. Surface_Name'Last;

   type Gesture is (Copy_Gesture, Paste_Gesture);

   type Ticket is record
      Live    : Boolean := False;
      Surface : Surface_Name := No_Surface;
      Issued  : Millisecond := 0;
   end record;
   type State is array (Gesture) of Ticket;

   function Fresh (T : Ticket; Surface : Surface_Name; Now : Millisecond)
     return Boolean is
     (T.Live and then T.Surface = Surface and then Surface /= No_Surface
      and then Now >= T.Issued and then Now - T.Issued <= Window_Ms);

   --  A new chord replaces the previous ticket of its kind.
   procedure Issue
     (S : in out State; Kind : Gesture; Surface : Live_Surface;
      Now : Millisecond)
     with Post => S (Kind) = (True, Surface, Now) and then
       (for all G in Gesture => (if G /= Kind then S (G) = S'Old (G)));

   procedure Redeem
     (S : in out State; Kind : Gesture; Surface : Surface_Name;
      Now : Millisecond; Granted : out Boolean)
     with Post => Granted = Fresh (S'Old (Kind), Surface, Now) and then
       (if Granted then not S (Kind).Live else S (Kind) = S'Old (Kind)) and then
       (for all G in Gesture => (if G /= Kind then S (G) = S'Old (G)));

   --  Focus moved, the surface went away, or input was resynchronized.
   procedure Revoke (S : out State)
     with Post => (for all G in Gesture => not S (G).Live);
end Clipboard_Gestures;
