with Compositor_Input_Queue;

-- Bounded selection policy for a future batched input transport. Selection
-- does not acknowledge or remove events; publication/acknowledgment is separate.
package Compositor_Input_Batches with SPARK_Mode, Pure is
   package IQ renames Compositor_Input_Queue;
   subtype Word is IQ.Word;
   use type Word;
   use type IQ.Event;
   Capacity : constant := 8;
   subtype Count is Natural range 0 .. Capacity;
   subtype Limit is Count range 1 .. Capacity;
   type Events is array (Limit) of IQ.Event;
   type Batch is record
      Length : Count := 0;
      Items : Events := [others => (others => <>)];
      Through : Word := 0;
      More : Boolean := False;
   end record;

   function Available
     (Q : IQ.Queue; Close : IQ.Event; After : Word) return Boolean is
     (IQ.Has_After (Q, After) or else
        (Close.Valid and then Close.Serial > After));

   function Next
     (Q : IQ.Queue; Close : IQ.Event; After : Word) return IQ.Event
   with Post =>
     Next'Result.Valid = Available (Q, Close, After) and then
     (if Next'Result.Valid then
        Next'Result.Serial > After and then
        (Next'Result = Close or else (for some E of Q => E = Next'Result))
        and then (for all E of Q =>
          (if E.Valid and then E.Serial > After then
             Next'Result.Serial <= E.Serial))
        and then (if Close.Valid and then Close.Serial > After then
          Next'Result.Serial <= Close.Serial));

   function Snapshot
     (Q : IQ.Queue; Close : IQ.Event; After : Word;
      Maximum : Limit := Capacity) return Batch
   with Post =>
     Snapshot'Result.Length <= Maximum and then
     Snapshot'Result.Through =
       (if Snapshot'Result.Length = 0 then After
        else Snapshot'Result.Items (Snapshot'Result.Length).Serial) and then
     Snapshot'Result.More = Available (Q, Close, Snapshot'Result.Through)
     and then (if Snapshot'Result.Length < Maximum then not Snapshot'Result.More)
     and then (for all I in Limit =>
       (if I <= Snapshot'Result.Length then
          Snapshot'Result.Items (I).Valid and then
          Snapshot'Result.Items (I) = Next (Q, Close,
            (if I = 1 then After else Snapshot'Result.Items (I - 1).Serial))
        else Snapshot'Result.Items (I) = IQ.Event'(others => <>)));
end Compositor_Input_Batches;
