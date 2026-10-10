with Interfaces;
package Input_Pending with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype Word is Interfaces.Unsigned_64;
   Capacity : constant := 32;
   subtype Count_Type is Natural range 0 .. Capacity;
   subtype Offset_Type is Natural range 0 .. Capacity - 1;
   type Item is record
      Payload : Word := 0;
      Sequence : Word := 0;
      Recover : Boolean := False;
      Observed_Ms : Word := Word'Last;
   end record;
   type Queue is private;
   function Count (Q : Queue) return Count_Type;
   function Sequence (Q : Queue) return Word;
   function Recovery_Pending (Q : Queue) return Boolean;
   function Next_Sequence (Previous : Word) return Word is
     (if Previous = Word'Last then 1 else Previous + 1)
     with Post => Next_Sequence'Result /= 0;
   function Element (Q : Queue; Offset : Offset_Type) return Item
     with Pre => Offset < Count (Q);
   -- A retained report adds a one-millisecond retry deadline. Existing
   -- storage/log deadlines always win if earlier; idle input adds no timer.
   function Wake_Deadline (Q : Queue; Now, Otherwise : Word) return Word is
     (if Count (Q) = 0 then Otherwise else
        Word'Min (Otherwise, (if Now = Word'Last then Now else Now + 1)))
     with Post => Wake_Deadline'Result <= Otherwise and then
       (if Count (Q) = 0 then Wake_Deadline'Result = Otherwise else
          Wake_Deadline'Result <= (if Now = Word'Last then Now else Now + 1));
   -- Transport refusal does not mutate Q. Remove the head only on acceptance.
   procedure Acknowledge (Q : in out Queue)
     with Pre => Count (Q) > 0,
       Post => Count (Q) = Count (Q'Old) - 1 and then
         Sequence (Q) = Sequence (Q'Old) and then
         Recovery_Pending (Q) = Recovery_Pending (Q'Old) and then
         (for all I in Offset_Type =>
            (if I < Count (Q) then Element (Q, I) = Element (Q'Old, I + 1)));
   -- Preserve exact motion/button/wheel order. On true local overflow discard
   -- the stale backlog, retain the newest state, and explicitly flag recovery.
   -- This is loss reporting, not a claim to recover unbounded input history.
   procedure Append (Q : in out Queue; Payload : Word; Lost : out Boolean;
                     Observed_Ms : Word := Word'Last)
     with Post =>
       Lost = (Count (Q'Old) = Capacity) and then
       Count (Q) = (if Lost then 1 else Count (Q'Old) + 1) and then
       Sequence (Q) = Next_Sequence (Sequence (Q'Old)) and then
       Element (Q, Count (Q) - 1).Observed_Ms = Observed_Ms and then
       Element (Q, Count (Q) - 1).Payload = Payload and then
       Element (Q, Count (Q) - 1).Sequence = Sequence (Q) and then
       Element (Q, Count (Q) - 1).Recover =
         (Lost or Recovery_Pending (Q'Old)) and then
       not Recovery_Pending (Q) and then
       (if not Lost then
         (for all I in Offset_Type =>
            (if I < Count (Q'Old) then Element (Q, I) = Element (Q'Old, I))));
   -- Rewrite the newest (not yet published) report in place: its sequence,
   -- recovery flag and acquisition time stay, so the stream has no gap.
   -- Callers own the payload meaning; Pointer_Pending uses this to coalesce
   -- motion by agreement (ACCUMULABLE_DISPLACEMENT), never transitions.
   procedure Replace_Newest (Q : in out Queue; Payload : Word)
     with Pre => Count (Q) > 0,
       Post => Count (Q) = Count (Q'Old) and then
         Sequence (Q) = Sequence (Q'Old) and then
         Recovery_Pending (Q) = Recovery_Pending (Q'Old) and then
         Element (Q, Count (Q) - 1).Payload = Payload and then
         Element (Q, Count (Q) - 1).Sequence =
           Element (Q'Old, Count (Q) - 1).Sequence and then
         Element (Q, Count (Q) - 1).Recover =
           Element (Q'Old, Count (Q) - 1).Recover and then
         Element (Q, Count (Q) - 1).Observed_Ms =
           Element (Q'Old, Count (Q) - 1).Observed_Ms and then
         (for all I in Offset_Type =>
            (if I < Count (Q) - 1 then Element (Q, I) = Element (Q'Old, I)));
   -- Never deliver a previous consumer's pending input to its replacement.
   procedure Reset (Q : in out Queue)
     with Post => Count (Q) = 0 and then Sequence (Q) = Sequence (Q'Old) and then
       Recovery_Pending (Q);
private
   type Storage is array (Offset_Type) of Item;
   type Queue is record
      Data : Storage := (others => <>);
      Head : Offset_Type := 0;
      Used : Count_Type := 0;
      Last_Sequence : Word := 0;
      Recover_Next : Boolean := False;
   end record;
   function Count (Q : Queue) return Count_Type is (Q.Used);
   function Sequence (Q : Queue) return Word is (Q.Last_Sequence);
   function Recovery_Pending (Q : Queue) return Boolean is (Q.Recover_Next);
   function Element (Q : Queue; Offset : Offset_Type) return Item is
     (Q.Data ((Q.Head + Offset) mod Capacity));
end Input_Pending;
