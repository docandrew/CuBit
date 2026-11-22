------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which ready channel a server serves next (CAP-002 step 3,
--  docs/capability-handles.md section 4): priority between channels, never
--  inside one.
--
--  @description
--  The server works in turns. In a turn each channel is served at most once
--  (one batch, up to the server's per-channel budget), higher classes first,
--  round robin within a class from where the class's last turn stopped.
--  Next is asked again after every batch, so a channel of a higher class
--  that became ready meanwhile is served before the next lower batch (a
--  preemption point). Turn_Over when no ready channel is left unserved; the
--  server then starts a new turn.
--
--  Proved (level 2): Next returns an active, ready channel not yet served
--  this turn, of the highest class among such channels, and the first one
--  after the class's cursor (round robin); it reports Turn_Over only when
--  no active ready channel is unserved, so every channel ready at the start
--  of a turn, and still ready, is served in that turn (no class starves).
------------------------------------------------------------------------------
pragma Ada_2022;

generic
   type Class is (<>);   --  Class'First is the highest
package CuBit.Channel_Schedule with Pure, SPARK_Mode is

   --  The structural bound (the server's table grows up to it).
   Maximum_Size : constant := 65_536;
   subtype Index is Positive range 1 .. Maximum_Size;
   type Flags is array (Index range <>) of Boolean;
   type Classes is array (Index range <>) of Class;
   type Cursors is array (Class) of Natural;

   type State (Size : Index) is record
      Active  : Flags (1 .. Size) := [others => False];
      Ready   : Flags (1 .. Size) := [others => False];
      Served  : Flags (1 .. Size) := [others => False];
      Class_Of : Classes (1 .. Size) := [others => Class'First];
      Cursor  : Cursors := [others => 0];
   end record;

   function Valid (S : State) return Boolean is
     (for all C in Class => S.Cursor (C) <= S.Size);

   --  Eligible this turn: active, ready, not yet served.
   function Eligible (S : State; I : Index) return Boolean is
     (S.Active (I) and then S.Ready (I) and then not S.Served (I))
   with Pre => I <= S.Size;

   function Any_Eligible (S : State; C : Class) return Boolean is
     (for some I in 1 .. S.Size => Eligible (S, I) and then S.Class_Of (I) = C);

   procedure Open (S : in out State; I : Index; C : Class)
   with Pre  => Valid (S) and then I <= S.Size,
        Post => Valid (S) and then S.Active (I) and then S.Class_Of (I) = C
                and then not S.Ready (I) and then not S.Served (I);

   procedure Close (S : in out State; I : Index)
   with Pre  => Valid (S) and then I <= S.Size,
        Post => Valid (S) and then not S.Active (I) and then not S.Ready (I);

   procedure Mark_Ready (S : in out State; I : Index)
   with Pre => Valid (S) and then I <= S.Size, Post => Valid (S) and then S.Ready (I)
     and then (for all J in 1 .. S.Size => S.Active (J) = S'Old.Active (J))
                and then (for all J in 1 .. S.Size => S.Served (J) = S'Old.Served (J))
     and then (for all J in 1 .. S.Size => S.Class_Of (J) = S'Old.Class_Of (J));

   --  The server served a batch of I's; Still_Ready: more is waiting.
   procedure Served (S : in out State; I : Index; Still_Ready : Boolean)
   with Pre  => Valid (S) and then I <= S.Size,
        Post => Valid (S) and then S.Served (I) and then S.Ready (I) = Still_Ready
                and then (for all J in 1 .. S.Size => S.Active (J) = S'Old.Active (J))
                and then (for all J in 1 .. S.Size => S.Class_Of (J) = S'Old.Class_Of (J));

   procedure Begin_Turn (S : in out State)
   with Pre  => Valid (S),
        Post => Valid (S) and then (for all I in 1 .. S.Size => not S.Served (I))
                and then (for all J in 1 .. S.Size => S.Active (J) = S'Old.Active (J))
                and then (for all J in 1 .. S.Size => S.Ready (J) = S'Old.Ready (J))
                and then (for all J in 1 .. S.Size => S.Class_Of (J) = S'Old.Class_Of (J));

   --  The first eligible channel of class C after position After (cyclic).
   function First_After (S : State; C : Class; After : Natural) return Index
   with Pre  => Valid (S) and then After <= S.Size and then Any_Eligible (S, C),
        Post => First_After'Result <= S.Size
                and then Eligible (S, First_After'Result) and then S.Class_Of (First_After'Result) = C
                and then (if First_After'Result > After then
                            (for all J in After + 1 .. First_After'Result - 1 =>
                               not (Eligible (S, J) and then S.Class_Of (J) = C))
                          else
                            (for all J in After + 1 .. S.Size =>
                               not (Eligible (S, J) and then S.Class_Of (J) = C))
                            and then
                            (for all J in 1 .. First_After'Result - 1 =>
                               not (Eligible (S, J) and then S.Class_Of (J) = C)));

   procedure Next (S : in out State; Found : out Boolean; I : out Index)
   with Pre  => Valid (S),
        Post => Valid (S)
                and then (for all J in 1 .. S.Size => S.Active (J) = S'Old.Active (J))
                and then (for all J in 1 .. S.Size => S.Ready (J) = S'Old.Ready (J))
                and then (for all J in 1 .. S.Size => S.Served (J) = S'Old.Served (J))
                and then (for all J in 1 .. S.Size => S.Class_Of (J) = S'Old.Class_Of (J))
                and then Found = (for some C in Class => Any_Eligible (S'Old, C))
                and then (if Found then
                            I <= S.Size and then Eligible (S'Old, I)
                            --  Highest class first.
                            and then (for all C in Class =>
                                        (if C < S'Old.Class_Of (I) then not Any_Eligible (S'Old, C)))
                            --  Round robin within the class.
                            and then I = First_After (S'Old, S'Old.Class_Of (I),
                                                      S'Old.Cursor (S'Old.Class_Of (I)))
                            and then S.Cursor (S'Old.Class_Of (I)) = I);

   --  The same channels in a larger state (the server's table grew).
   procedure Grow (From : State; Into : out State)
   with Pre  => Valid (From) and then Into.Size >= From.Size,
        Post => Valid (Into)
                and then (for all I in 1 .. From.Size =>
                            Into.Active (I) = From.Active (I) and then Into.Ready (I) = From.Ready (I)
                            and then Into.Served (I) = From.Served (I)
                            and then Into.Class_Of (I) = From.Class_Of (I))
                and then (for all I in From.Size + 1 .. Into.Size => not Into.Active (I));

end CuBit.Channel_Schedule;
