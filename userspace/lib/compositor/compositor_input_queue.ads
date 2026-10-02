with Interfaces;
package Compositor_Input_Queue with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype Word is Interfaces.Unsigned_64;
   Capacity : constant := 32;
   subtype Index is Natural range 0 .. Capacity - 1;
   subtype Selection is Integer range -1 .. Index'Last;
   type Event is record
      Valid : Boolean := False;
      Serial, Kind, Target, Payload0, Payload1 : Word := 0;
   end record;
   type Queue is array (Index) of Event;
   function Newest (Q : Queue) return Selection
     with Post =>
       (if Newest'Result = -1 then (for all E of Q => not E.Valid)
        else Q (Newest'Result).Valid and then
          (for all E of Q => (if E.Valid then E.Serial <= Q (Newest'Result).Serial)));
   function Vacant (Q : Queue) return Selection
     with Post =>
       (if Vacant'Result = -1 then (for all E of Q => E.Valid)
        else not Q (Vacant'Result).Valid and then
          (for all I in Index => (if I < Vacant'Result then Q (I).Valid)));
   function Coalesced (Old_Event, Incoming : Event) return Event is
     (Old_Event with delta Payload0 => Incoming.Payload0, Payload1 => Incoming.Payload1);
   function Numbered (Incoming : Event; Serial : Word) return Event is
     (Incoming with delta Valid => True, Serial => Serial);
   function Cleared (Value : Event) return Event is
     (Value with delta Valid => False);
   function Has_After (Q : Queue; After : Word) return Boolean is
     (for some E of Q => E.Valid and then E.Serial > After);
   function Oldest_After (Q : Queue; After : Word) return Selection
     with Post =>
       (if Oldest_After'Result = -1 then not Has_After (Q, After)
        else Q (Oldest_After'Result).Valid and then
          Q (Oldest_After'Result).Serial > After and then
          (for all E of Q => (if E.Valid and E.Serial > After then
             Q (Oldest_After'Result).Serial <= E.Serial)));
   procedure Pop
     (Q : in out Queue; After : Word; Selected : out Selection; Value : out Event)
     with Post => Selected = Oldest_After (Q'Old, After) and then
       Value = (if Selected = -1 then (others => <>) else Q'Old (Selected)) and then
       (for all I in Index => Q (I) =
          (if Q'Old (I).Valid and then (Q'Old (I).Serial <= After or I = Selected)
           then Cleared (Q'Old (I)) else Q'Old (I)));
   -- Zero denotes refusal. No successful allocation can wrap or reuse zero.
   procedure Reserve (Next_Serial : in out Word; Serial : out Word)
     with Post =>
       (if Next_Serial'Old = 0 or Next_Serial'Old = Word'Last then
          Serial = 0 and Next_Serial = Next_Serial'Old
        else Serial = Next_Serial'Old and Next_Serial = Next_Serial'Old + 1);
   procedure Recover
     (Q : in out Queue; Next_Serial : in out Word; Recovery : Event; Accepted : out Boolean)
     with Post =>
       Accepted = (Next_Serial'Old /= 0 and Next_Serial'Old /= Word'Last) and then
       (if Accepted then Next_Serial = Next_Serial'Old + 1 and
          Q (Index'First) = Numbered (Recovery, Next_Serial'Old) and
          (for all I in Index => (if I /= Index'First then not Q (I).Valid))
        else Q = Q'Old and Next_Serial = Next_Serial'Old);
   type Outcome is (Appended, Motion_Replaced, Resynchronized, Exhausted);
   -- A caller owns one queue per stable surface. Only adjacent motion can be
   -- replaced. Every other event kind is a strict barrier. Recovery describes
   -- authoritative state AFTER the report that discovered overflow.
   procedure Push
     (Q : in out Queue; Next_Serial : in out Word;
      Incoming, Recovery : Event; Motion_Kind : Word; Result : out Outcome)
     with Post =>
       (if Next_Serial'Old = 0 or Next_Serial'Old = Word'Last then
          Result = Exhausted and Q = Q'Old and Next_Serial = Next_Serial'Old
        elsif Incoming.Kind = Motion_Kind and then Newest (Q'Old) /= -1 and then
          Q'Old (Newest (Q'Old)).Kind = Motion_Kind
        then Result = Motion_Replaced and Next_Serial = Next_Serial'Old and
          (for all I in Index => Q (I) =
            (if I = Newest (Q'Old) then Coalesced (Q'Old (I), Incoming) else Q'Old (I)))
        elsif Vacant (Q'Old) /= -1 then
          Result = Appended and Next_Serial = Next_Serial'Old + 1 and
          (for all I in Index => Q (I) =
            (if I = Vacant (Q'Old) then Numbered (Incoming, Next_Serial'Old) else Q'Old (I)))
        else Result = Resynchronized and Next_Serial = Next_Serial'Old + 1 and
          Q (Index'First) = Numbered (Recovery, Next_Serial'Old) and
          (for all I in Index => (if I /= Index'First then not Q (I).Valid)));
end Compositor_Input_Queue;
