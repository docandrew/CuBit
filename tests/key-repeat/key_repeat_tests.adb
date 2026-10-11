--  Hosted tests of the desktop typematic state machine (Key_Repeat):
--  delay, rate, release, a second key, modifiers, toggles, dedupe of device
--  repeats, route (focus) change, cancel/reset (resynchronization), stalls.
with Ada.Text_IO;
with Interfaces;
with Key_Repeat;

procedure Key_Repeat_Tests is
   use type Interfaces.Unsigned_64;
   use type Key_Repeat.Key;
   use type Key_Repeat.Press_Outcome;
   use type Key_Repeat.Key_Class;
   package KR renames Key_Repeat;

   Failures : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   A      : constant KR.Key := (16#1E#, False);
   B      : constant KR.Key := (16#30#, False);
   Shift  : constant KR.Key := (16#2A#, False);
   Ctrl   : constant KR.Key := (16#1D#, False);
   Caps   : constant KR.Key := (16#3A#, False);
   Up     : constant KR.Key := (16#48#, True);
   Pad_8  : constant KR.Key := (16#48#, False);
   Fake   : constant KR.Key := (16#2A#, True);
   Super  : constant KR.Key := (16#5B#, True);
   Mute   : constant KR.Key := (16#20#, True);
   Volume : constant KR.Key := (16#30#, True);
   App    : constant KR.Route := (KR.Focused_Surface, 7);
   Other  : constant KR.Route := (KR.Focused_Surface, 8);
   Menu   : constant KR.Route := (KR.Apps_Menu, 0);
   Config : constant KR.Settings := KR.DEFAULT_SETTINGS;

   function Fresh return KR.State is
      S : KR.State;
   begin
      return S;
   end Fresh;

   procedure Press (S : in out KR.State; K : KR.Key; Now : KR.Time;
                    Where : KR.Route := App; Settings : KR.Settings := Config;
                    Expect : KR.Press_Outcome := KR.New_Press; Name : String := "press") is
      Outcome : KR.Press_Outcome;
   begin
      KR.Press (S, K, Now, Settings, Where, Outcome);
      Check (Outcome = Expect, Name & ": outcome");
   end Press;

   function Fires (S : in out KR.State; Now : KR.Time; Where : KR.Route := App) return Boolean is
      Fire : Boolean;
      K : KR.Key;
   begin
      KR.Tick (S, Now, Where, Fire, K);
      return Fire;
   end Fires;
begin
   --  Defaults match common desktops.
   Check (Config.Enabled and Config.Initial_Delay = 500 and Config.Rate = 30, "defaults 500 ms / 30 Hz");
   Check (KR.Class_Of (A) = KR.Repeating and KR.Class_Of (Up) = KR.Repeating, "ordinary keys repeat");
   Check (KR.Class_Of (Shift) = KR.Modifier and KR.Class_Of (Ctrl) = KR.Modifier and
          KR.Class_Of (Super) = KR.Modifier and KR.Class_Of ((16#38#, True)) = KR.Modifier,
          "modifiers");
   Check (KR.Class_Of (Caps) = KR.Toggle and KR.Class_Of (Mute) = KR.Toggle, "toggles");
   Check (KR.Class_Of (Fake) = KR.Ignored and KR.Class_Of ((16#36#, True)) = KR.Ignored, "fake shifts");
   Check (KR.Class_Of (Volume) = KR.Repeating, "volume keys repeat");

   --  Delay, then the rate, exact on average.
   declare
      S : KR.State := Fresh;
      Fire : Boolean;
      K : KR.Key;
      Count : Natural := 0;
   begin
      Check (not KR.Armed (S) and KR.Next_Due (S) = KR.NEVER, "idle");
      Press (S, A, 1_000);
      Check (KR.Armed (S) and KR.Armed_Key (S) = A and KR.Next_Due (S) = 1_500, "first repeat after the delay");
      Check (not Fires (S, 1_499), "nothing before the delay");
      KR.Tick (S, 1_500, App, Fire, K);
      Check (Fire and K = A, "repeat at the delay");
      Check (KR.Next_Due (S) = 1_533, "next after one period (33 ms + carry)");
      Count := 1;
      while KR.Next_Due (S) <= 2_500 loop
         KR.Tick (S, KR.Next_Due (S), App, Fire, K);
         exit when not Fire;
         Count := Count + 1;
      end loop;
      Check (Count = 31, "30 repeats per second after the first" & Count'Image);
      Check (KR.Next_Due (S) = 2_533, "the carry keeps the schedule exact");
      Check (not Fires (S, 2_532), "not early");
      KR.Release (S, A);
      Check (not KR.Armed (S) and KR.Next_Due (S) = KR.NEVER and not KR.Is_Held (S, A), "release stops");
      Check (not Fires (S, 10_000), "released key never fires");
   end;

   --  Rates at the bounds.
   declare
      S : KR.State := Fresh;
   begin
      Press (S, A, 0, Settings => (True, 100, 1));
      Check (KR.Next_Due (S) = 100, "minimum delay");
      Check (Fires (S, 100) and KR.Next_Due (S) = 1_100, "1 Hz");
      KR.Reset (S);
      Press (S, A, 0, Settings => (True, 2_000, 100));
      Check (Fires (S, 2_000) and KR.Next_Due (S) = 2_010, "100 Hz, maximum delay");
   end;

   --  A device repeat of a held key is dropped and changes nothing.
   declare
      S : KR.State := Fresh;
   begin
      Press (S, A, 1_000);
      Press (S, A, 1_400, Expect => KR.Held_Repeat, Name => "PS/2 typematic make");
      Check (KR.Next_Due (S) = 1_500, "device repeat does not restart the delay");
      Press (S, Caps, 1_450);
      Press (S, Caps, 1_480, Expect => KR.Held_Repeat, Name => "held Caps Lock does not toggle again");
      Press (S, Shift, 1_490);
      Press (S, Shift, 1_495, Expect => KR.Held_Repeat, Name => "held Shift");
   end;

   --  A second key takes over; releasing the first leaves it running.
   declare
      S : KR.State := Fresh;
   begin
      Press (S, A, 1_000);
      Press (S, B, 1_200);
      Check (KR.Armed_Key (S) = B and KR.Next_Due (S) = 1_700, "the newest key repeats");
      KR.Release (S, A);
      Check (KR.Armed (S) and KR.Armed_Key (S) = B and KR.Next_Due (S) = 1_700, "rolling release keeps it");
      Press (S, A, 1_300);
      Check (KR.Armed_Key (S) = A, "pressing the first again takes over");
      KR.Release (S, B);
      Check (KR.Armed_Key (S) = A, "old key's release is not the armed key's");
      KR.Release (S, A);
      Check (not KR.Armed (S), "armed key's release stops");
   end;

   --  Modifiers and toggles never repeat; pressing them, or releasing a
   --  modifier, stops a repeat.
   declare
      S : KR.State := Fresh;
   begin
      Press (S, Shift, 1_000);
      Check (not KR.Armed (S), "Shift alone does not repeat");
      Press (S, A, 1_100);
      Check (KR.Armed (S), "Shift+A repeats");
      KR.Release (S, Shift);
      Check (not KR.Armed (S), "releasing a modifier stops");
      Check (KR.Is_Held (S, A), "A still held");
      Press (S, A, 1_200, Expect => KR.Held_Repeat, Name => "still-held A does not restart");
      KR.Release (S, A);
      Press (S, A, 1_300);
      Press (S, Ctrl, 1_350);
      Check (not KR.Armed (S), "pressing a modifier stops");
      KR.Release (S, Ctrl);
      KR.Release (S, A);
      Press (S, A, 1_400);
      Press (S, Caps, 1_450);
      Check (not KR.Armed (S), "pressing a toggle stops");
      KR.Release (S, Caps);
      Check (not KR.Armed (S), "toggle release does not restart");
      KR.Release (S, A);
      Press (S, Super, 1_500);
      Check (not KR.Armed (S), "Super does not repeat");
      KR.Release (S, Super);
      Press (S, Mute, 1_600);
      Check (not KR.Armed (S), "Mute does not repeat");
   end;

   --  Fake shifts do not touch state; E0 keys are their own keys.
   declare
      S : KR.State := Fresh;
   begin
      Press (S, Up, 1_000);
      Press (S, Fake, 1_100);
      Check (KR.Armed (S) and KR.Armed_Key (S) = Up and not KR.Is_Held (S, Fake), "fake shift ignored");
      Press (S, Fake, 1_120, Name => "fake shift again");
      KR.Release (S, Fake);
      Check (KR.Armed (S) and KR.Next_Due (S) = 1_500, "fake shift release ignored");
      Press (S, Pad_8, 1_200);
      Check (KR.Armed_Key (S) = Pad_8 and KR.Is_Held (S, Up), "keypad 8 is not Up");
   end;

   --  Disabled repeat, and destinations that never repeat.
   declare
      S : KR.State := Fresh;
   begin
      Press (S, A, 1_000, Settings => (False, 500, 30));
      Check (not KR.Armed (S), "disabled");
      KR.Reset (S);
      for Command in KR.Super_Key .. KR.Exit_Key loop
         KR.Reset (S);
         Press (S, A, 1_000, Where => (Command, 0));
         Check (not KR.Armed (S), "desktop commands do not repeat: " & Command'Image);
      end loop;
      KR.Reset (S);
      Press (S, A, 1_000, Where => (KR.Nowhere, 0));
      Check (not KR.Armed (S), "undelivered keys do not repeat");
      KR.Reset (S);
      Press (S, A, 1_000, Where => Menu);
      Check (KR.Armed (S) and Fires (S, 1_500, Menu), "menus repeat (arrows)");
   end;

   --  A change of route (focus) stops the repeat at its next due time.
   declare
      S : KR.State := Fresh;
   begin
      Press (S, A, 1_000);
      Check (not Fires (S, 1_499, Other), "not due: no route check yet");
      Check (KR.Armed (S), "still armed before due");
      Check (not Fires (S, 1_500, Other), "focus moved: no repeat");
      Check (not KR.Armed (S), "focus moved: stopped");
      Check (not Fires (S, 1_600, App), "focus back: still stopped");
      Press (S, A, 1_700, Expect => KR.Held_Repeat, Name => "held A after focus change");
      KR.Reset (S);
      Press (S, A, 2_000, Where => Menu);
      Check (not Fires (S, 2_500, App), "menu closed: no leak to the app");
   end;

   --  Cancel keeps held keys; Reset forgets them.
   declare
      S : KR.State := Fresh;
   begin
      Press (S, A, 1_000);
      KR.Cancel (S);
      Check (not KR.Armed (S) and KR.Is_Held (S, A), "cancel stops, keeps held");
      Check (not Fires (S, 2_000), "cancelled never fires");
      Press (S, A, 2_000, Expect => KR.Held_Repeat, Name => "device repeat after cancel");
      KR.Reset (S);
      Check (not KR.Is_Held (S, A), "reset forgets held keys");
      Press (S, A, 3_000, Name => "press after reset");
      Check (KR.Armed (S), "press after reset repeats");
   end;

   --  A stall fires once and re-anchors: never a burst.
   declare
      S : KR.State := Fresh;
   begin
      Press (S, A, 1_000);
      Check (Fires (S, 5_000), "late tick fires");
      Check (KR.Next_Due (S) = 5_033, "re-anchored one period after now");
      Check (not Fires (S, 5_000), "no catch-up burst");
      Check (not Fires (S, 5_032), "not before the period");
      Check (Fires (S, 5_033), "then the rate");
   end;

   --  Clock clamp.
   Check (KR.Clamp (Interfaces.Unsigned_64'Last) = KR.LATEST and KR.Clamp (42) = 42, "clamp");
   declare
      S : KR.State := Fresh;
   begin
      Press (S, A, KR.LATEST, Settings => (True, 2_000, 1));
      Check (KR.Next_Due (S) = KR.LATEST + 2_000, "no wrap at the latest time");
      Check (Fires (S, KR.LATEST) = False, "not due");
   end;

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("key-repeat: PASS" & Checks'Image & " checks");
   else
      Ada.Text_IO.Put_Line ("key-repeat: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Key_Repeat_Tests;
