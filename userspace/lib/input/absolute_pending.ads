with Interfaces;
with Input_Pending;
with Absolute_Pointer;
--  Absolute-pointer publication retention: latest position wins.
--
--  ABSOLUTE_POINTER reports travel as REPLACEABLE_STATE (CuBit.Input): both
--  parties agree that a newer position replaces an older one that was not
--  yet published. Button and device-flag transitions are ORDERED and are
--  never merged, moved or lost: a report replaces only a pending
--  (unpublished) newest report that is itself not a transition, and only
--  when it carries the same buttons and flags. Wheel steps are summed, but a
--  step is never moved to a different position: a report carrying wheel
--  steps absorbs only later reports at its own position.
--
--  Result: while the consumer stalls, pending reports grow only with button
--  and flag transitions (and wheel-then-move alternations), not with the
--  device's report rate. True overflow (Capacity unpublished transitions)
--  still discards the backlog and flags explicit recovery.
package Absolute_Pending with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_64, Input_Pending.Item,
     Absolute_Pointer.Report;
   subtype Word is Input_Pending.Word;
   subtype Report is Absolute_Pointer.Report;
   function Encode (R : Report) return Word renames Absolute_Pointer.Encode;

   type State is private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Count (S : State) return Input_Pending.Count_Type;
   function Sequence (S : State) return Word;
   function Element (S : State; Offset : Input_Pending.Offset_Type)
     return Input_Pending.Item
     with Pre => Offset < Count (S);
   --  The newest report appended or replaced since the last Reset.
   function Last (S : State) return Report;
   --  The report preceding Last in the stream (Last's transition basis).
   function Before_Last (S : State) return Report;
   --  Last is known to carry no button/flag transition relative to
   --  Before_Last, and nothing (recovery) forbids replacing it.
   function Tail_Mergeable (S : State) return Boolean;
   function Wake_Deadline (S : State; Now, Otherwise : Word) return Word;

   function Wheel_Fits (Older, Newer : Report) return Boolean is
     (Older.Wheel + Newer.Wheel in Absolute_Pointer.Wheel_Steps);

   function Can_Replace (S : State; R : Report) return Boolean is
     (Count (S) > 0 and then Tail_Mergeable (S) and then
      R.Buttons = Last (S).Buttons and then R.Flags = Last (S).Flags and then
      (Last (S).Wheel = 0 or else Absolute_Pointer.Same_Position (Last (S), R))
      and then Wheel_Fits (Last (S), R));

   --  The newer position, with every wheel step either report carried.
   function Merge (Older, Newer : Report) return Report is
     ((Buttons => Older.Buttons, X => Newer.X, Y => Newer.Y,
       Wheel => Older.Wheel + Newer.Wheel, Flags => Older.Flags))
     with Pre => Wheel_Fits (Older, Newer);

   type Append_Outcome is (Appended, Replaced, Overflowed);

   procedure Append
     (S : in out State; R : Report; Observed_Ms : Word;
      Outcome : out Append_Outcome)
     with Pre => Valid (S),
       Post => Valid (S) and then
         (case Outcome is
            when Replaced =>
              --  Agreed replacement of the unpublished newest report: no new
              --  sequence (so no gap), the newest position, wheel steps
              --  conserved, still no transition.
              Can_Replace (S'Old, R) and then
              Count (S) = Count (S'Old) and then
              Sequence (S) = Sequence (S'Old) and then
              Last (S) = Merge (Last (S'Old), R) and then
              Absolute_Pointer.Same_Position (Last (S), R) and then
              Before_Last (S) = Before_Last (S'Old) and then
              Last (S).Buttons = Before_Last (S).Buttons and then
              Element (S, Count (S) - 1).Sequence =
                Element (S'Old, Count (S) - 1).Sequence and then
              Element (S, Count (S) - 1).Observed_Ms =
                Element (S'Old, Count (S) - 1).Observed_Ms and then
              Element (S, Count (S) - 1).Recover =
                Element (S'Old, Count (S) - 1).Recover and then
              (for all I in Input_Pending.Offset_Type =>
                 (if I < Count (S) - 1 then Element (S, I) = Element (S'Old, I))),
            when Appended =>
              not Can_Replace (S'Old, R) and then
              Count (S'Old) < Input_Pending.Capacity and then
              Count (S) = Count (S'Old) + 1 and then
              Sequence (S) = Input_Pending.Next_Sequence (Sequence (S'Old)) and then
              Last (S) = R and then
              Element (S, Count (S) - 1).Sequence = Sequence (S) and then
              Element (S, Count (S) - 1).Observed_Ms = Observed_Ms and then
              (for all I in Input_Pending.Offset_Type =>
                 (if I < Count (S'Old) then Element (S, I) = Element (S'Old, I))),
            when Overflowed =>
              --  Capacity unpublished transitions: explicit, flagged loss.
              not Can_Replace (S'Old, R) and then
              Count (S'Old) = Input_Pending.Capacity and then
              Count (S) = 1 and then Element (S, 0).Recover and then
              Sequence (S) = Input_Pending.Next_Sequence (Sequence (S'Old)) and then
              Last (S) = R and then not Tail_Mergeable (S));

   --  Remove the head only after the transport accepted it.
   procedure Acknowledge (S : in out State)
     with Pre => Valid (S) and then Count (S) > 0,
       Post => Valid (S) and then Count (S) = Count (S'Old) - 1 and then
         Sequence (S) = Sequence (S'Old) and then
         Last (S) = Last (S'Old) and then
         (for all I in Input_Pending.Offset_Type =>
            (if I < Count (S) then Element (S, I) = Element (S'Old, I + 1)));

   --  The device lost events upstream (SYN_DROPPED): every retained report
   --  stays, nothing is replaced into them, and the next report appended is
   --  flagged for explicit recovery.
   procedure Mark_Lost (S : in out State)
     with Pre => Valid (S),
       Post => Valid (S) and then Count (S) = Count (S'Old) and then
         Sequence (S) = Sequence (S'Old) and then not Tail_Mergeable (S) and then
         (for all I in Input_Pending.Offset_Type =>
            (if I < Count (S) then Element (S, I) = Element (S'Old, I)));

   --  Consumer replacement: never deliver the old consumer's backlog; the
   --  next report starts a fresh, recovery-flagged stream state.
   procedure Reset (S : in out State)
     with Pre => Valid (S),
       Post => Valid (S) and then Count (S) = 0 and then
         Sequence (S) = Sequence (S'Old) and then not Tail_Mergeable (S);

private
   type State is record
      Pending   : Input_Pending.Queue;
      Newest    : Report := (others => <>);
      Previous  : Report := (others => <>);
      Known     : Boolean := False;
      Mergeable : Boolean := False;
   end record;

   function Count (S : State) return Input_Pending.Count_Type is
     (Input_Pending.Count (S.Pending));
   function Sequence (S : State) return Word is
     (Input_Pending.Sequence (S.Pending));
   function Element (S : State; Offset : Input_Pending.Offset_Type)
     return Input_Pending.Item is (Input_Pending.Element (S.Pending, Offset));
   function Last (S : State) return Report is (S.Newest);
   function Before_Last (S : State) return Report is (S.Previous);
   function Tail_Mergeable (S : State) return Boolean is (S.Mergeable);
   function Wake_Deadline (S : State; Now, Otherwise : Word) return Word is
     (Input_Pending.Wake_Deadline (S.Pending, Now, Otherwise));

   --  The pending tail is exactly Newest on the wire; a mergeable tail has
   --  the same buttons and flags as the report before it; a stream that was
   --  reset has no basis for replacing.
   function Valid (S : State) return Boolean is
     ((if Input_Pending.Count (S.Pending) > 0 then
         Input_Pending.Element
           (S.Pending, Input_Pending.Count (S.Pending) - 1).Payload =
             Encode (S.Newest)) and then
      (if S.Mergeable then
         S.Known and then S.Newest.Buttons = S.Previous.Buttons and then
         S.Newest.Flags = S.Previous.Flags));
end Absolute_Pending;
