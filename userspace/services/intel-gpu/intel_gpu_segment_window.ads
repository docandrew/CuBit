with Interfaces; use Interfaces;
with Intel_GPU_Ring_Reservation;
-- Many segments outstanding in one context's 16 KiB ring (GPU-001 step 2).
-- Pure logic, no I/O; proved at GNATprove level 2.
--
-- Every segment the driver appends is remembered with its timeline value
-- until a later segment's breadcrumb has completed. The retired head the
-- ring reservation takes is the start of the oldest segment still
-- remembered: the last completed segment stays protected, because ring
-- commands after its breadcrumb may still execute (Linux keeps the request's
-- ring space until it retires). Positions are virtual: free-running byte
-- counts whose remainder modulo the ring size is the ring offset, so "this
-- write is in the free part of the ring" is plain arithmetic.
--
-- Proved: the window spans at most Ring_Bytes - Guard_Bytes; an appended
-- segment lies entirely beyond every remembered segment and within one ring
-- of the retired head, so its bytes (and its wrap padding) never share a
-- ring offset with a byte of a segment that has not retired
-- (Lemma_No_Overwrite); the ring offset the reservation names is the
-- segment's virtual start; values only increase; retirement forgets a
-- segment only after a later segment's value completed.
package Intel_GPU_Segment_Window with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package RR renames Intel_GPU_Ring_Reservation;

   Ring_Bytes : constant := 16_384;
   Guard_Bytes : constant := 64;
   pragma Compile_Time_Error
     (RR.Ring_Bytes /= Ring_Bytes or RR.Guard_Bytes /= Guard_Bytes,
      "segment window must match the ring reservation");
   Span_Limit : constant := Ring_Bytes - Guard_Bytes;
   -- Commands are QWORD granular.
   Command_Alignment : constant := 8;

   -- Jobs in flight plus the protected, completed segment before them.
   Max_In_Flight : constant := 32;
   Max_Live : constant := Max_In_Flight + 1;

   type Position is new Unsigned_64;
   type Value is new Unsigned_64;
   -- Positions stop well short of wrapping: 2 ** 62 bytes of commands.
   Position_Limit : constant Position := 2 ** 62;

   subtype Live_Count is Natural range 0 .. Max_Live;
   subtype Live_Index is Positive range 1 .. Max_Live;

   type Window is private;

   function Valid (W : Window) return Boolean;
   function Count (W : Window) return Live_Count;
   -- Start of the oldest remembered segment (the retired head).
   function Head (W : Window) return Position;
   function Tail (W : Window) return Position;
   function Start_Of (W : Window; I : Live_Index) return Position
     with Pre => I <= Count (W);
   function Stop_Of (W : Window; I : Live_Index) return Position
     with Pre => I <= Count (W);
   function Value_Of (W : Window; I : Live_Index) return Value
     with Pre => I <= Count (W);
   -- The value of the newest segment. A valid window always remembers at
   -- least one segment: the newest completed one, or one in flight.
   function Last_Value (W : Window) return Value;

   function Ring_Offset (P : Position) return Unsigned_32 is
     (Unsigned_32 (P mod Ring_Bytes));

   -- The ring as registration left it: one segment of First_Bytes at ring
   -- offset 0 whose breadcrumb writes First_Value, already completed.
   function Initial (First_Bytes : Unsigned_32; First_Value : Value) return Window
     with Pre => First_Bytes in Command_Alignment .. Span_Limit and then
                 First_Bytes mod Command_Alignment = 0,
          Post => Valid (Initial'Result) and then Count (Initial'Result) = 1 and then
                  Head (Initial'Result) = 0 and then
                  Tail (Initial'Result) = Position (First_Bytes) and then
                  Last_Value (Initial'Result) = First_Value;

   -- The ring space a segment of Bytes would take now.
   function Plan (W : Window; Bytes : Unsigned_32) return RR.Plan
     with Pre => Valid (W),
          Post => RR."=" (Plan'Result, RR.Reserve (Ring_Offset (Head (W)), Ring_Offset (Tail (W)), Bytes));

   -- Room in the window for one more segment of Bytes with value Next.
   function Can_Append (W : Window; Bytes : Unsigned_32; Next : Value) return Boolean is
     (Valid (W) and then Count (W) < Max_Live and then Next > Last_Value (W) and then
      Tail (W) < Position_Limit and then
      RR."=" (Plan (W, Bytes).Status, RR.Ready));

   -- Remember a segment the caller has written where Plan said (padding
   -- from the old tail to the end of the ring first, when it wraps) and
   -- published. Its virtual start is the old tail plus padding.
   procedure Append (W : in out Window; Bytes : Unsigned_32; Next : Value)
     with Pre => Can_Append (W, Bytes, Next),
          Post => Valid (W) and then Count (W) = Count (W)'Old + 1 and then
                  Head (W) = Head (W)'Old and then
                  Last_Value (W) = Next and then
                  Start_Of (W, Count (W)) =
                    Tail (W)'Old + Position (Plan (W'Old, Bytes).Padding) and then
                  Ring_Offset (Start_Of (W, Count (W))) = Plan (W'Old, Bytes).Start and then
                  Tail (W) = Tail (W)'Old + Position (Plan (W'Old, Bytes).Consumed) and then
                  Stop_Of (W, Count (W)) = Tail (W) and then
                  Tail (W) - Head (W) <= Span_Limit and then
                  (for all I in 1 .. Count (W)'Old =>
                     Start_Of (W, I) = Start_Of (W'Old, I) and
                     Stop_Of (W, I) = Stop_Of (W'Old, I) and
                     Value_Of (W, I) = Value_Of (W'Old, I));

   -- Forget every segment whose successor's value has completed: the
   -- oldest remembered segment is then the newest completed one (or one
   -- not yet completed). The tail does not move.
   procedure Retire (W : in out Window; Completed : Value)
     with Pre => Valid (W),
          Post => Valid (W) and then Tail (W) = Tail (W)'Old and then
                  Head (W) >= Head (W)'Old and then
                  Count (W) <= Count (W)'Old and then
                  Last_Value (W) = Last_Value (W)'Old and then
                  (Count (W) = Count (W)'Old or else Count (W) >= 1) and then
                  (if Count (W) >= 2 then Value_Of (W, 2) > Completed) and then
                  (if Count (W)'Old >= 1 then Count (W) >= 1);

   -- No write of a planned segment lands on a byte of a remembered one:
   -- X is a byte of the window, Y a byte the plan writes.
   procedure Lemma_No_Overwrite (W : Window; Bytes : Unsigned_32; X, Y : Position)
     with Ghost, Global => null,
          Pre => Valid (W) and then RR."=" (Plan (W, Bytes).Status, RR.Ready) and then
                 X >= Head (W) and then X < Tail (W) and then
                 Y >= Tail (W) and then
                 Y < Tail (W) + Position (Plan (W, Bytes).Consumed) and then
                 Tail (W) < Position_Limit,
          Post => X mod Ring_Bytes /= Y mod Ring_Bytes;
private
   type Segment is record
      Start, Stop : Position := 0;
      Seq : Value := 0;
   end record;
   type Segment_Array is array (Live_Index) of Segment;
   type Window is record
      Items : Segment_Array;
      Size : Live_Count := 0;
      First, Last : Position := 0;
   end record;

   function Count (W : Window) return Live_Count is (W.Size);
   function Head (W : Window) return Position is (W.First);
   function Tail (W : Window) return Position is (W.Last);
   function Start_Of (W : Window; I : Live_Index) return Position is (W.Items (I).Start);
   function Stop_Of (W : Window; I : Live_Index) return Position is (W.Items (I).Stop);
   function Value_Of (W : Window; I : Live_Index) return Value is (W.Items (I).Seq);
   function Last_Value (W : Window) return Value is
     (if W.Size = 0 then 0 else W.Items (W.Size).Seq);

   function Valid (W : Window) return Boolean is
     (W.First <= W.Last and then W.Last - W.First <= Span_Limit and then
      W.Last <= Position_Limit + Span_Limit and then
      W.First mod Command_Alignment = 0 and then W.Last mod Command_Alignment = 0 and then
      W.Size >= 1 and then
      (W.Items (1).Start = W.First and then W.Items (W.Size).Stop = W.Last and then
         (for all I in 1 .. W.Size =>
            W.Items (I).Start < W.Items (I).Stop and then
            W.Items (I).Start mod Command_Alignment = 0 and then
            W.Items (I).Start >= W.First and then W.Items (I).Stop <= W.Last) and then
         (for all I in 1 .. W.Size =>
            (for all J in I + 1 .. W.Size =>
               W.Items (I).Stop <= W.Items (J).Start and then
               W.Items (I).Seq < W.Items (J).Seq))));
end Intel_GPU_Segment_Window;
