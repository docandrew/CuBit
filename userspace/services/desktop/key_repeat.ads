------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Keyboard auto-repeat (typematic) for the desktop seat.
--
--  USB boot keyboards report key state, never repeats; PS/2 keyboards (and
--  QEMU's host-side auto-repeat, on any keyboard) send repeated make codes.
--  The desktop owns repeat for both, for every application:
--
--  * Dedupe. A press of a key already held is a device repeat and is
--    dropped (Held_Repeat): Desktop alone synthesizes repeats, at the
--    configured delay and rate, whatever the keyboard does.
--  * Which key. Only the most recently pressed Repeating key repeats, and
--    only toward a destination that allows it. Releasing it, pressing any
--    other key, releasing a modifier, a change of destination or focus
--    (Tick's route check), Cancel and Reset stop it. Releasing some other
--    non-modifier key does not (rolling key-overs keep the last key going).
--  * Timing. Repeats fall due at Press time + delay, then every 1/rate s
--    (exact on average: the millisecond remainder is carried). A late Tick
--    fires once and re-anchors past Now: a stall never becomes a burst.
--
--  Pure SPARK (level 2): tests/key-repeat proves it and runs hosted tests.
--  Keys are set-1 scan codes as the drivers deliver them (break bit
--  removed, E0 context separate); see Class_Of for the key classes.
------------------------------------------------------------------------------
with Interfaces;
with CCL.Interfaces.Desktop_Input;

package Key_Repeat with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   package DI renames CCL.Interfaces.Desktop_Input;

   subtype Milliseconds is Interfaces.Unsigned_64;
   --  The latest time this unit computes with: far beyond any uptime, and
   --  low enough that a due time (Latest + delay or period) cannot wrap.
   LATEST : constant Milliseconds := Milliseconds'Last / 2;
   subtype Time is Milliseconds range 0 .. LATEST;
   --  No repeat is due.
   NEVER : constant Milliseconds := Milliseconds'Last;
   --  A monotonic clock reading as a Time (saturating at LATEST).
   function Clamp (Reading : Milliseconds) return Time is
     (if Reading > LATEST then LATEST else Reading);

   MILLISECONDS_PER_SECOND : constant := 1_000;
   subtype Delay_Ms is Milliseconds range DI.MINIMUM_DELAY_MS .. DI.MAXIMUM_DELAY_MS;
   subtype Rate_Hz is Milliseconds range DI.MINIMUM_RATE_HZ .. DI.MAXIMUM_RATE_HZ;

   type Settings is record
      Enabled       : Boolean := True;
      Initial_Delay : Delay_Ms := DI.DEFAULT_DELAY_MS;
      Rate          : Rate_Hz := DI.DEFAULT_RATE_HZ;
   end record;
   DEFAULT_SETTINGS : constant Settings := (others => <>);

   --  A set-1 scan code without its break bit, and whether an E0 prefix
   --  preceded it.
   type Scan_Code is mod 2 ** 7;
   type Key is record
      Code     : Scan_Code := 0;
      Extended : Boolean := False;
   end record;

   --  Set-1 codes Class_Of names.
   CODE_LEFT_CTRL    : constant Scan_Code := 16#1D#;
   CODE_LEFT_SHIFT   : constant Scan_Code := 16#2A#;
   CODE_RIGHT_SHIFT  : constant Scan_Code := 16#36#;
   CODE_LEFT_ALT     : constant Scan_Code := 16#38#;
   CODE_CAPS_LOCK    : constant Scan_Code := 16#3A#;
   CODE_NUM_LOCK     : constant Scan_Code := 16#45#;
   CODE_SCROLL_LOCK  : constant Scan_Code := 16#46#;
   --  After E0:
   CODE_MUTE         : constant Scan_Code := 16#20#;
   CODE_LEFT_SUPER   : constant Scan_Code := 16#5B#;
   CODE_RIGHT_SUPER  : constant Scan_Code := 16#5C#;

   --  Repeating: ordinary keys. Modifier: Shift, Ctrl, Alt, Super (never
   --  repeat; pressing or releasing one stops a repeat). Toggle: lock keys
   --  and Mute (never repeat; a held one must not flip again; pressing one
   --  stops a repeat). Ignored: the E0-prefixed "fake shifts" some PS/2
   --  keyboards wrap around navigation keys; they are not keys and do not
   --  touch repeat or held state.
   type Key_Class is (Repeating, Modifier, Toggle, Ignored);
   function Class_Of (K : Key) return Key_Class is
     (if K.Code in CODE_LEFT_SHIFT | CODE_RIGHT_SHIFT then
        (if K.Extended then Ignored else Modifier)
      elsif K.Code in CODE_LEFT_CTRL | CODE_LEFT_ALT then Modifier
      elsif K.Extended and then K.Code in CODE_LEFT_SUPER | CODE_RIGHT_SUPER then Modifier
      elsif not K.Extended and then
        K.Code in CODE_CAPS_LOCK | CODE_NUM_LOCK | CODE_SCROLL_LOCK
      then Toggle
      elsif K.Extended and then K.Code = CODE_MUTE then Toggle
      else Repeating);

   --  Where the desktop delivered a press: the Super key, a Super chord,
   --  Alt+Tab focus cycling, the exit key (before any client), the volume
   --  keys, the volume popup, the clipboard manager, the Apps menu or the
   --  focused surface. Desktop commands and undelivered keys never repeat.
   type Destination is
     (Nowhere, Super_Key, Super_Chord, Focus_Cycle, Exit_Key, Volume_Control,
      Audio_Popup, Clipboard_Manager, Apps_Menu, Focused_Surface);
   function Repeatable (Where : Destination) return Boolean is
     (Where in Volume_Control | Audio_Popup | Clipboard_Manager | Apps_Menu | Focused_Surface);

   --  A destination plus the focused surface it applies to (zero unless
   --  Focused_Surface). A repeat is delivered only along the press's route.
   type Route is record
      Where   : Destination := Nowhere;
      Surface : Interfaces.Unsigned_64 := 0;
   end record;

   type State is private with Default_Initial_Condition => not Armed (State);

   function Armed (S : State) return Boolean;
   function Armed_Key (S : State) return Key with Pre => Armed (S);
   function Armed_Route (S : State) return Route with Pre => Armed (S);
   function Is_Held (S : State; K : Key) return Boolean;
   --  The next repeat's due time; NEVER exactly when not armed. An armed
   --  due time is always a Time plus at most one delay or period.
   function Next_Due (S : State) return Milliseconds
     with Post => (if Armed (S) then Next_Due'Result < NEVER else Next_Due'Result = NEVER);

   --  The armed repeat's rate and its carried millisecond remainder.
   subtype Carry is Milliseconds range 0 .. DI.MAXIMUM_RATE_HZ - 1;
   function Armed_Rate (S : State) return Rate_Hz with Pre => Armed (S);
   function Rate_Carry (S : State) return Carry;
   --  Bresenham schedule: each period is 1000 / Rate ms plus one more
   --  whenever the carried remainder (1000 mod Rate per repeat) reaches
   --  Rate, so N periods total N * 1000 / Rate ms within one millisecond.
   function Period (Rate : Rate_Hz; Remainder : Carry) return Milliseconds is
     (MILLISECONDS_PER_SECOND / Rate + (Remainder + MILLISECONDS_PER_SECOND mod Rate) / Rate)
     with Pre => Remainder < Rate;
   function Next_Carry (Rate : Rate_Hz; Remainder : Carry) return Carry is
     ((Remainder + MILLISECONDS_PER_SECOND mod Rate) mod Rate)
     with Pre => Remainder < Rate;

   type Press_Outcome is (New_Press, Held_Repeat);

   --  A make code. Held_Repeat (drop it) exactly when K is a key already
   --  held; then nothing changes. Otherwise K is held and repeats if it is a
   --  Repeating key, repeat is enabled and Where allows it; any other new
   --  press stops the current repeat.
   procedure Press
     (S : in out State; K : Key; Now : Time; Config : Settings; Where : Route;
      Outcome : out Press_Outcome)
     with Post =>
       (if Class_Of (K) = Ignored then Outcome = New_Press and S = S'Old
        elsif Is_Held (S'Old, K) then Outcome = Held_Repeat and S = S'Old
        else Outcome = New_Press and Is_Held (S, K) and
          (for all Other in Scan_Code =>
             (for all E in Boolean =>
                (if (Other, E) /= K then
                   Is_Held (S, (Other, E)) = Is_Held (S'Old, (Other, E))))) and
          (if Class_Of (K) = Repeating and Config.Enabled and Repeatable (Where.Where)
           then Armed (S) and Armed_Key (S) = K and Armed_Route (S) = Where and
                Next_Due (S) = Now + Config.Initial_Delay and
                Armed_Rate (S) = Config.Rate and Rate_Carry (S) = 0
           else not Armed (S)));

   --  A break code. K is no longer held. Releasing the repeating key or any
   --  modifier stops the repeat; any other release leaves it as it was.
   procedure Release (S : in out State; K : Key)
     with Post =>
       (if Class_Of (K) = Ignored then S = S'Old
        else not Is_Held (S, K) and
          (for all Other in Scan_Code =>
             (for all E in Boolean =>
                (if (Other, E) /= K then
                   Is_Held (S, (Other, E)) = Is_Held (S'Old, (Other, E))))) and
          (if Armed (S'Old) and then
              (Armed_Key (S'Old) = K or else Class_Of (K) = Modifier)
           then not Armed (S)
           elsif Armed (S'Old) then
             Armed (S) and Armed_Key (S) = Armed_Key (S'Old) and
             Armed_Route (S) = Armed_Route (S'Old) and Next_Due (S) = Next_Due (S'Old) and
             Armed_Rate (S) = Armed_Rate (S'Old) and Rate_Carry (S) = Rate_Carry (S'Old)
           else not Armed (S)));

   --  Stop repeating (focus change, input resynchronization). Held keys
   --  stay held: their device repeats are still dropped.
   procedure Cancel (S : in out State)
     with Post => not Armed (S) and
       (for all C in Scan_Code =>
          (for all E in Boolean => Is_Held (S, (C, E)) = Is_Held (S'Old, (C, E))));

   --  Forget everything (a gap in the keyboard's report stream: which keys
   --  are down is unknown).
   procedure Reset (S : out State)
     with Post => not Armed (S) and
       (for all C in Scan_Code => (for all E in Boolean => not Is_Held (S, (C, E))));

   --  Fire when a repeat is due along Where, the route the armed key's
   --  press took. A due repeat whose route changed stops instead.
   procedure Tick
     (S : in out State; Now : Time; Where : Route; Fire : out Boolean; K : out Key)
     with Post =>
       (if not Armed (S'Old) or else Now < Next_Due (S'Old) then
          not Fire and S = S'Old
        elsif Where /= Armed_Route (S'Old) then
          not Fire and not Armed (S) and
          (for all C in Scan_Code =>
             (for all E in Boolean => Is_Held (S, (C, E)) = Is_Held (S'Old, (C, E))))
        else
          Fire and K = Armed_Key (S'Old) and Armed (S) and
          Armed_Key (S) = Armed_Key (S'Old) and Armed_Route (S) = Armed_Route (S'Old) and
          Armed_Rate (S) = Armed_Rate (S'Old) and Next_Due (S) > Now and
          --  On time: one period later. Late: one whole period after Now.
          (if Next_Due (S'Old) + Period (Armed_Rate (S'Old), Rate_Carry (S'Old)) > Now then
             Next_Due (S) = Next_Due (S'Old) + Period (Armed_Rate (S'Old), Rate_Carry (S'Old)) and
             Rate_Carry (S) = Next_Carry (Armed_Rate (S'Old), Rate_Carry (S'Old))
           else
             Next_Due (S) = Now + MILLISECONDS_PER_SECOND / Armed_Rate (S'Old) and
             Rate_Carry (S) = 0) and
          (for all C in Scan_Code =>
             (for all E in Boolean => Is_Held (S, (C, E)) = Is_Held (S'Old, (C, E)))));

private
   type Held_Keys is array (Scan_Code, Boolean) of Boolean;

   type State is record
      Held     : Held_Keys := [others => [others => False]];
      Active   : Boolean := False;
      Current  : Key;
      Target   : Route;
      Rate     : Rate_Hz := DI.DEFAULT_RATE_HZ;
      Due      : Milliseconds := NEVER;
      Fraction : Carry := 0;
   end record
     with Dynamic_Predicate =>
       (if State.Active then State.Due <= LATEST + DI.MAXIMUM_DELAY_MS and
                             State.Fraction < State.Rate
        else State.Due = NEVER);

   function Armed (S : State) return Boolean is (S.Active);
   function Armed_Key (S : State) return Key is (S.Current);
   function Armed_Route (S : State) return Route is (S.Target);
   function Is_Held (S : State; K : Key) return Boolean is (S.Held (K.Code, K.Extended));
   function Next_Due (S : State) return Milliseconds is (S.Due);
   function Armed_Rate (S : State) return Rate_Hz is (S.Rate);
   function Rate_Carry (S : State) return Carry is (S.Fraction);
end Key_Repeat;
