------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The sender's SACK scoreboard (RFC 2018, RFC 6675): which bytes in
--  flight the peer has selectively acknowledged, and which are lost.
--
--  Offsets count from SND.UNA. The board is a bounded, sorted list of
--  disjoint, non-adjacent ranges (at most Max_Ranges; a peer reports at most four
--  per ACK, and ranges merge as holes fill). If the board is full a new,
--  unmergeable block is dropped: forgetting a SACK only causes a
--  retransmission that was not needed, whereas believing one that did not
--  happen would never resend lost data.
--
--  To be proved (tests/net-tcp):
--  - soundness: a byte is marked SACKed only if a SACK block covered it
--    (Add), and SND.UNA advancing moves marks, never creates them;
--  - nothing SACKed is forgotten by Add, and a block is recorded whenever
--    the board has room;
--  - IsLost (RFC 6675 4) is monotone (a byte below a lost byte is lost)
--    and needs evidence: some byte above it was SACKed;
--  - Next_Unsacked finds the first byte at or after a point that is not
--    SACKed, with everything before it SACKed.
--  Pipe and NextSeg's rules (RFC 6675 4) combine these with HighRxt in the
--  connection engine.
------------------------------------------------------------------------------
with TCP_Limits; use TCP_Limits;

generic
   Max_Ranges : Positive;
   Max_Flight : Positive;   --  the largest window (2**30 with scaling)
package TCP_Scoreboard with SPARK_Mode is
   pragma Compile_Time_Error (Max_Ranges > 1024, "scoreboards are small");
   pragma Compile_Time_Error (Max_Flight > Maximum_Scaled_Window, "windows are below 2**30");
   --  Integers, not sequence numbers: the engine converts at the edge.
   subtype Offset is Natural range 0 .. Max_Flight;

   type Board is private;

   function Count (B : Board) return Natural
     with Post => Count'Result <= Max_Ranges;
   function Valid (B : Board) return Boolean;
   function SACKed (B : Board; Off : Offset) return Boolean;

   procedure Clear (B : out Board) with
     Post => Valid (B) and then Count (B) = 0 and then
             (for all O in Offset => not SACKed (B, O));

   --  A SACK block [First, Stop), as offsets from SND.UNA (the caller
   --  drops blocks outside SND.UNA .. SND.NXT; RFC 2018 3, RFC 5961 5).
   procedure Add (B : in out Board; First, Stop : Offset) with
     Pre  => Valid (B) and then First < Stop,
     Post => Valid (B) and then
             (for all O in Offset =>
                (if SACKed (B, O) then SACKed (B'Old, O) or else O in First .. Stop - 1)) and then
             (for all O in Offset => (if SACKed (B'Old, O) then SACKed (B, O))) and then
             (if Count (B'Old) < Max_Ranges then
                (for all O in First .. Stop - 1 => SACKed (B, O)));

   --  SND.UNA advanced by By bytes.
   procedure Advance (B : in out Board; By : Offset) with
     Pre  => Valid (B),
     Post => Valid (B) and then Count (B) <= Count (B'Old) and then
             (for all O in Offset =>
                (if O <= Max_Flight - By then SACKed (B, O) = SACKed (B'Old, O + By)
                 else not SACKed (B, O)));

   --  RFC 6675 4 IsLost, with DupThresh = 3: three separate SACKed ranges
   --  above Off, or more than 2 * SMSS SACKed bytes above it.
   function Is_Lost (B : Board; Off : Offset; SMSS : Positive) return Boolean
   with Pre => Valid (B) and then SMSS <= Maximum_MTU;

   --  Loss needs evidence: a SACKed byte above.
   procedure Lemma_Lost_Evidence (B : Board; Off : Offset; SMSS : Positive)
   with Ghost, Global => null,
        Pre  => Valid (B) and then SMSS <= Maximum_MTU and then Is_Lost (B, Off, SMSS),
        Post => (for some O in Offset => O > Off and then SACKed (B, O));

   procedure Lemma_Lost_Monotone (B : Board; Low, High : Offset; SMSS : Positive)
   with Ghost, Global => null,
        Pre  => Valid (B) and then SMSS <= Maximum_MTU and then Low <= High and then
                Is_Lost (B, High, SMSS),
        Post => Is_Lost (B, Low, SMSS);

   --  The first offset at or after From that is not SACKed.
   function Next_Unsacked (B : Board; From : Offset) return Offset
   with Pre  => Valid (B),
        Post => Next_Unsacked'Result >= From and then
                not SACKed (B, Next_Unsacked'Result) and then
                (for all O in Offset =>
                   (if O >= From and then O < Next_Unsacked'Result then SACKed (B, O)));

private
   type Span is record
      First, Stop : Offset := 0;   --  [First, Stop)
   end record;
   type Spans is array (1 .. Max_Ranges) of Span;

   --  Ranges in ascending order, each ending at least one byte before
   --  the next begins.
   type Board is record
      R : Spans;
      N : Natural range 0 .. Max_Ranges := 0;
   end record;

   function In_Span (S : Span; O : Offset) return Boolean is
     (O >= S.First and then O < S.Stop);

   function Count (B : Board) return Natural is (B.N);

   function Valid (B : Board) return Boolean is
     ((for all I in 1 .. B.N => B.R (I).First < B.R (I).Stop) and then
      (for all I in 1 .. B.N - 1 => B.R (I).Stop < B.R (I + 1).First));

   function SACKed (B : Board; Off : Offset) return Boolean is
     (for some I in 1 .. B.N => In_Span (B.R (I), Off));

   --  Bytes of S above Off.
   function Above (S : Span; Off : Offset) return Natural is
     (if S.Stop <= Off + 1 then 0
      elsif S.First > Off then S.Stop - S.First
      else S.Stop - (Off + 1))
   with Pre => S.First <= S.Stop;

   --  In mathematical integers: the bound is linear there.
   function Bytes_Above (B : Board; Off : Offset; N : Natural) return Long_Long_Integer is
     (if N = 0 then 0
      else Bytes_Above (B, Off, N - 1) + Long_Long_Integer (Above (B.R (N), Off)))
   with Pre => N <= B.N and then (for all I in 1 .. B.N => B.R (I).First < B.R (I).Stop),
        Post => Bytes_Above'Result in 0 .. Long_Long_Integer (N) * Long_Long_Integer (Max_Flight),
        Subprogram_Variant => (Decreases => N);

   function Ranges_Above (B : Board; Off : Offset; N : Natural) return Natural is
     (if N = 0 then 0
      else Ranges_Above (B, Off, N - 1) + (if Above (B.R (N), Off) > 0 then 1 else 0))
   with Pre => N <= B.N and then (for all I in 1 .. B.N => B.R (I).First < B.R (I).Stop),
        Post => Ranges_Above'Result <= N,
        Subprogram_Variant => (Decreases => N);

   function Is_Lost (B : Board; Off : Offset; SMSS : Positive) return Boolean is
     (Ranges_Above (B, Off, B.N) >= Duplicate_Threshold or else
      Bytes_Above (B, Off, B.N) > (Duplicate_Threshold - 1) * Long_Long_Integer (SMSS));
end TCP_Scoreboard;
