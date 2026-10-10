with Interfaces;
with Input_Pending;
--  Relative-pointer publication retention with agreed motion coalescing.
--
--  RELATIVE_POINTER reports travel as ACCUMULABLE_DISPLACEMENT (CuBit.Input):
--  both parties agree that consecutive motion may be summed. Button and
--  device-flag transitions are ORDERED and are never merged, moved or lost:
--  a report is merged only into a pending (unpublished) newest report that is
--  itself not a transition, and only when it carries the same buttons. Wheel
--  steps are summed too, but never moved past later motion, so a wheel step
--  is always applied at the pointer position it was produced at.
--
--  Result: while the consumer stalls, pending reports grow only with button
--  and flag transitions (and wheel-then-motion alternations), not with the
--  device's report rate. True overflow (Capacity unpublished transitions)
--  still discards the backlog and flags explicit recovery.
package Pointer_Pending with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_64, Input_Pending.Item;
   subtype Word is Input_Pending.Word;

   --  Wire payload of a RELATIVE_POINTER source report:
   --  bits 0..7 buttons, 8..19 X, 20..31 Y (positive up, PS/2 orientation),
   --  32..39 wheel (positive away from the user), 40..47 device flags.
   Displacement_Bits : constant := 12;
   Wheel_Bits        : constant := 8;
   X_Shift           : constant := 8;
   Y_Shift           : constant := 20;
   Wheel_Shift       : constant := 32;
   Flags_Shift       : constant := 40;

   subtype Displacement is Integer range
     -(2 ** (Displacement_Bits - 1)) .. 2 ** (Displacement_Bits - 1) - 1;
   subtype Wheel_Steps is Integer range
     -(2 ** (Wheel_Bits - 1)) .. 2 ** (Wheel_Bits - 1) - 1;
   subtype Button_State is Interfaces.Unsigned_8;
   subtype Device_Flags is Interfaces.Unsigned_8;

   type Report is record
      Buttons : Button_State := 0;
      X       : Displacement := 0;
      Y       : Displacement := 0;
      Wheel   : Wheel_Steps := 0;
      Flags   : Device_Flags := 0;
   end record;

   --  Two's complement of a value in its wire field.
   function Displacement_Field (Value : Displacement) return Word is
     (if Value < 0 then Word (2 ** Displacement_Bits + Value) else Word (Value))
     with Post => Displacement_Field'Result < 2 ** Displacement_Bits;
   function Wheel_Field (Value : Wheel_Steps) return Word is
     (if Value < 0 then Word (2 ** Wheel_Bits + Value) else Word (Value))
     with Post => Wheel_Field'Result < 2 ** Wheel_Bits;

   function Encode (R : Report) return Word is
     (Word (R.Buttons) or
      Interfaces.Shift_Left (Displacement_Field (R.X), X_Shift) or
      Interfaces.Shift_Left (Displacement_Field (R.Y), Y_Shift) or
      Interfaces.Shift_Left (Wheel_Field (R.Wheel), Wheel_Shift) or
      Interfaces.Shift_Left (Word (R.Flags), Flags_Shift));

   type State is private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Count (S : State) return Input_Pending.Count_Type;
   function Sequence (S : State) return Word;
   function Element (S : State; Offset : Input_Pending.Offset_Type)
     return Input_Pending.Item
     with Pre => Offset < Count (S);
   --  The newest report appended or coalesced since the last Reset.
   function Last (S : State) return Report;
   --  The report preceding Last in the stream (Last's transition basis).
   function Before_Last (S : State) return Report;
   --  Last is known to carry no button/flag transition relative to
   --  Before_Last, and nothing (recovery) forbids extending it.
   function Tail_Mergeable (S : State) return Boolean;
   function Wake_Deadline (S : State; Now, Otherwise : Word) return Word;

   function Sum_Fits (A, B : Report) return Boolean is
     (A.X + B.X in Displacement and then A.Y + B.Y in Displacement and then
      A.Wheel + B.Wheel in Wheel_Steps);

   function Can_Coalesce (S : State; R : Report) return Boolean is
     (Count (S) > 0 and then Tail_Mergeable (S) and then
      R.Buttons = Last (S).Buttons and then R.Flags = Last (S).Flags and then
      (Last (S).Wheel = 0 or else (R.X = 0 and then R.Y = 0)) and then
      Sum_Fits (Last (S), R));

   function Sum (A, B : Report) return Report is
     ((Buttons => A.Buttons, X => A.X + B.X, Y => A.Y + B.Y,
       Wheel => A.Wheel + B.Wheel, Flags => A.Flags))
     with Pre => Sum_Fits (A, B);

   type Append_Outcome is (Appended, Coalesced, Overflowed);

   procedure Append
     (S : in out State; R : Report; Observed_Ms : Word;
      Outcome : out Append_Outcome)
     with Pre => Valid (S),
       Post => Valid (S) and then
         (case Outcome is
            when Coalesced =>
              --  Agreed accumulation into the unpublished newest report:
              --  no new sequence (so no gap), displacement conserved, the
              --  merged report still carries no transition.
              Can_Coalesce (S'Old, R) and then
              Count (S) = Count (S'Old) and then
              Sequence (S) = Sequence (S'Old) and then
              Last (S) = Sum (Last (S'Old), R) and then
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
              not Can_Coalesce (S'Old, R) and then
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
              not Can_Coalesce (S'Old, R) and then
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

   --  Consumer replacement: never deliver the old consumer's backlog; the
   --  next report starts a fresh, recovery-flagged stream state.
   procedure Reset (S : in out State)
     with Pre => Valid (S),
       Post => Valid (S) and then Count (S) = 0 and then
         Sequence (S) = Sequence (S'Old) and then not Tail_Mergeable (S);

private
   type State is record
      Pending  : Input_Pending.Queue;
      Newest   : Report := (others => <>);
      Previous : Report := (others => <>);
      Known    : Boolean := False;
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
   --  reset has no basis for merging.
   function Valid (S : State) return Boolean is
     ((if Input_Pending.Count (S.Pending) > 0 then
         Input_Pending.Element
           (S.Pending, Input_Pending.Count (S.Pending) - 1).Payload =
             Encode (S.Newest)) and then
      (if S.Mergeable then
         S.Known and then S.Newest.Buttons = S.Previous.Buttons and then
         S.Newest.Flags = S.Previous.Flags));
end Pointer_Pending;
