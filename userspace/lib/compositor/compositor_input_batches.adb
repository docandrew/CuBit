package body Compositor_Input_Batches with SPARK_Mode is
   function Next
     (Q : IQ.Queue; Close : IQ.Event; After : Word) return IQ.Event
   is
      Selected : constant IQ.Selection := IQ.Oldest_After (Q, After);
   begin
      -- Match the existing dequeue policy: a retained close precedes ordinary
      -- input only when its serial is strictly smaller.
      if Close.Valid and then Close.Serial > After and then
        (Selected = -1 or else Close.Serial < Q (Selected).Serial)
      then
         return Close;
      elsif Selected /= -1 then
         return Q (Selected);
      else
         return (others => <>);
      end if;
   end Next;

   function Snapshot
     (Q : IQ.Queue; Close : IQ.Event; After : Word;
      Maximum : Limit := Capacity) return Batch
   is
      Result : Batch := (Through => After, others => <>);
      Item : IQ.Event;
   begin
      for I in Limit range 1 .. Maximum loop
         Item := Next (Q, Close, Result.Through);
         exit when not Item.Valid;
         Result.Items (I) := Item;
         Result.Length := I;
         Result.Through := Item.Serial;
         pragma Loop_Invariant (Result.Length = I);
         pragma Loop_Invariant (Result.Through = Result.Items (I).Serial);
         pragma Loop_Invariant
           (for all J in Limit =>
             (if J <= I then Result.Items (J).Valid and then
                Result.Items (J) = Next (Q, Close,
                  (if J = 1 then After else Result.Items (J - 1).Serial))
              else Result.Items (J) = IQ.Event'(others => <>)));
      end loop;
      Result.More := Available (Q, Close, Result.Through);
      return Result;
   end Snapshot;
end Compositor_Input_Batches;
