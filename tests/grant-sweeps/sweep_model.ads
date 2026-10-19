------------------------------------------------------------------------------
--  The Grant_Sweeps protocol, proved (SPARK, level 2) on a model.
--
--  A list linked through an array (as the kernel links grants through
--  their records), cursors resting on the grant each walk visits next, and
--  the three things that happen to them: a grant linked at the head, a
--  grant unlinked (every cursor on it first moves to its successor:
--  Grant_Sweeps.Pass), and a walk's step (take the grant under the cursor,
--  move the cursor to its successor, then visit: Grant_Sweeps.Walk).
--
--  Ghost ranks order the list: they increase along it, a grant linked at
--  the head is ranked below everything ever listed, and a cursor only moves
--  up. Proved from that: a cursor always rests on a listed grant (or at the
--  end), and no walk visits a grant twice, even one unlinked and linked
--  again during the walk.
--
--  The model's sizes are the model's; Grant_Sweeps has no such limits.
------------------------------------------------------------------------------
pragma Ada_2022;

package Sweep_Model with SPARK_Mode is

   Model_Slots   : constant := 32;
   Model_Cursors : constant := 4;

   subtype Slot is Natural range 0 .. Model_Slots;
   No_Slot : constant Slot := 0;
   subtype Live_Slot is Slot range 1 .. Model_Slots;
   subtype Cursor is Positive range 1 .. Model_Cursors;

   --  Ghost-like ranks: concrete so the model needs no ghost state, and
   --  wide enough for every operation a proof could chain.
   type Rank is range -2 ** 62 .. 2 ** 62;
   No_Visit : constant Rank := Rank'First;

   type Slot_Links is array (Live_Slot) of Slot;
   type Slot_Flags is array (Live_Slot) of Boolean;
   type Slot_Ranks is array (Live_Slot) of Rank;
   type Cursor_Slots is array (Cursor) of Slot;
   type Cursor_Ranks is array (Cursor) of Rank;
   type Seen_Flags is array (Cursor, Live_Slot) of Boolean;

   type State is record
      Head       : Slot := No_Slot;
      Next, Prev : Slot_Links := [others => No_Slot];
      Listed     : Slot_Flags := [others => False];
      Ranks      : Slot_Ranks := [others => 0];
      --  Every listed rank is above Floor; the next head link gets Floor.
      Floor      : Rank := 0;
      Ahead      : Cursor_Slots := [others => No_Slot];
      --  The rank of each walk's last visit, and what it has visited.
      Last       : Cursor_Ranks := [others => No_Visit];
      Seen       : Seen_Flags := [others => [others => False]];
   end record;

   function Linked (S : State) return Boolean is
     ((S.Head = No_Slot or else (S.Listed (S.Head) and then S.Prev (S.Head) = No_Slot))
      and then
      (for all X in Live_Slot =>
         (if S.Listed (X) then
            (if S.Next (X) /= No_Slot then
               S.Listed (S.Next (X)) and then S.Prev (S.Next (X)) = X
               and then S.Ranks (S.Next (X)) > S.Ranks (X))
            and then
            (if S.Prev (X) /= No_Slot then
               S.Listed (S.Prev (X)) and then S.Next (S.Prev (X)) = X
             else S.Head = X)
            and then S.Ranks (X) > S.Floor)));

   function Cursors_Valid (S : State) return Boolean is
     (for all C in Cursor =>
        (S.Ahead (C) = No_Slot or else
           (S.Listed (S.Ahead (C)) and then S.Ranks (S.Ahead (C)) > S.Last (C)))
        and then (S.Last (C) = No_Visit or else S.Last (C) > S.Floor)
        and then
        (for all X in Live_Slot =>
           (if S.Seen (C, X) then
              S.Last (C) /= No_Visit
              and then (if S.Listed (X) then S.Ranks (X) <= S.Last (C)))));

   function Valid (S : State) return Boolean is
     (Linked (S) and then Cursors_Valid (S));

   --  Link X at the head (Grant_Sweeps: not visited by any walk now
   --  under way).
   procedure Link_Front (S : in out State; X : Live_Slot)
   with
     Pre  => Valid (S) and then not S.Listed (X) and then S.Floor > Rank'First + 1,
     Post => Valid (S) and then S.Listed (X) and then S.Head = X
             and then S.Ahead = S.Ahead'Old
             and then (for all C in Cursor =>
                         S.Ranks (X) < S.Last (C) or else S.Last (C) = No_Visit);

   --  Unlink X; first every cursor on it moves to its successor.
   procedure Unlink (S : in out State; X : Live_Slot)
   with
     Pre  => Valid (S) and then S.Listed (X),
     Post => Valid (S) and then not S.Listed (X)
             and then (for all C in Cursor =>
                         (if S.Ahead'Old (C) = X then S.Ahead (C) = S.Next'Old (X)
                          else S.Ahead (C) = S.Ahead'Old (C)));

   --  Start walk C at the head.
   procedure Start (S : in out State; C : Cursor)
   with
     Pre  => Valid (S),
     Post => Valid (S) and then S.Ahead (C) = S.Head
             and then (for all X in Live_Slot => not S.Seen (C, X));

   --  One step of walk C: Visited is the grant it visits now.
   procedure Step (S : in out State; C : Cursor; Visited : out Live_Slot)
   with
     Pre  => Valid (S) and then S.Ahead (C) /= No_Slot,
     Post => Valid (S)
             and then Visited = S.Ahead'Old (C)
             and then S.Listed (Visited)
             --  Never twice in one walk.
             and then not S.Seen'Old (C, Visited)
             and then S.Seen (C, Visited)
             and then (S.Ahead (C) = No_Slot or else
                       S.Ranks (S.Ahead (C)) > S.Ranks (Visited));

end Sweep_Model;
