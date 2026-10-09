with Compositor_Trace_Archive;
package Compositor_Trace_Framing with Pure, SPARK_Mode is
   package A renames Compositor_Trace_Archive;
   subtype Word is A.Word;
   use type Word, A.S.Capture;
   Maximum_Events : constant := 4096;
   subtype Count is Natural range 0 .. Maximum_Events;
   type Metadata is record
      Capture_ID, Endpoint, Started_Us : Word := 0;
      Budget : Count := 0;
   end record;
   -- Clock is monotonic microseconds scoped to this file; boot identity is
   -- explicitly unknown. Do not merge time domains across separate captures.
   function Valid (M : Metadata) return Boolean is
     (M.Capture_ID /= 0 and M.Endpoint /= 0 and M.Started_Us /= Word'Last and M.Budget > 0);
   Header_Magic : constant Word := 16#4354_4148_0000_0001#;
   Footer_Magic : constant Word := 16#4354_4146_0000_0001#;
   function Header_Valid (Data : A.Chunk) return Boolean;
   function Header (M : Metadata) return A.Chunk with Pre => Valid (M),
     Post => Header_Valid (Header'Result);
   type Phase is (Empty, Reading, Footer_Seen, Complete, Incomplete, Rejected);
   type Stop_Reason is (Requested_Stop, Budget_Reached, Observer_Failed);
   type State is private;
   function Status (S : State) return Phase;
   function Info (S : State) return Metadata;
   function Events (S : State) return Count;
   function Digest (S : State) return Word;
   procedure Start (S : out State; Data : A.Chunk) with
     Post => Events (S) = 0 and then
       (if Header_Valid (Data) then Status (S) = Reading and Valid (Info (S))
        else Status (S) = Rejected);
   function Can_Append (S : State; Value : A.S.Capture) return Boolean is
     (Status (S) = Reading and then Events (S) < Info (S).Budget and then
      A.Valid (Value) and then Value.Incarnation = Info (S).Endpoint);
   function Event_Chunk (S : State; Value : A.S.Capture) return A.Chunk with
     Pre => Can_Append (S, Value),
     Post => A.Decode (Event_Chunk'Result).Success and then
       A.Decode (Event_Chunk'Result).Sequence = Word (Events (S)) + 1 and then
       A.Decode (Event_Chunk'Result).Value = Value;
   function Footer_Matches (S : State; Data : A.Chunk) return Boolean;
   function Footer (S : State; Ended_Us : Word; Reason : Stop_Reason;
      Statistics : A.S.Statistics) return A.Chunk with
     Pre => Status (S) = Reading and then Ended_Us /= Word'Last and then
       Ended_Us >= Info (S).Started_Us and then
       Statistics.Emitted_Events >= Word (Events (S)) and then
       (if Reason = Budget_Reached then Events (S) = Info (S).Budget),
     Post => Footer_Matches (S, Footer'Result);
   -- Process every complete chunk in file order, including any after footer.
   -- Only exact next-sequence events in the same observer incarnation count.
   procedure Feed (S : in out State; Data : A.Chunk; Accepted_Event : out Boolean)
     with Post => Info (S) = Info (S'Old) and then
       Events (S) >= Events (S'Old) and then
       (if Accepted_Event then Status (S) = Reading and then
          Events (S) = Events (S'Old) + 1
        else Events (S) = Events (S'Old));
   -- Caller supplies the actual remainder at EOF. A footer alone is not enough.
   -- Complete means checked file structure, never successful disk flush.
   procedure End_Of_File (S : in out State; Trailing_Bytes : Natural)
     with Pre => Trailing_Bytes < 256,
       Post => Events (S) = Events (S'Old) and then Info (S) = Info (S'Old) and then
         (Status (S) = Complete) =
           (Status (S'Old) = Footer_Seen and Trailing_Bytes = 0);
private
   type State is record
      Current : Phase := Empty;
      Meta : Metadata;
      Used : Count := 0;
      Chain : Word := 0;
   end record with Dynamic_Predicate =>
     (if Current in Reading | Footer_Seen | Complete | Incomplete
      then Valid (Meta) and Used <= Meta.Budget);
   function Status (S : State) return Phase is (S.Current);
   function Info (S : State) return Metadata is (S.Meta);
   function Events (S : State) return Count is (S.Used);
   function Digest (S : State) return Word is (S.Chain);
   function Header_Valid (Data : A.Chunk) return Boolean is
     (Data (0) = Header_Magic and Data (1) = 256 and Data (2) /= 0 and
      Data (3) /= 0 and Data (4) /= Word'Last and
      Data (5) in 1 .. Word (Maximum_Events) and Data (6) = 1 and
      (for all I in 7 .. 30 => Data (I) = 0) and
      Data (31) = A.Checksum (A.Prefix (Data)));
end Compositor_Trace_Framing;
