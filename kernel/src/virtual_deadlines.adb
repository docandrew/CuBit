package body Virtual_Deadlines with SPARK_Mode => On is

   function Refill (Now : Deadline; Slice : Slice_Length) return Deadline is
     (if Now <= Deadline'Last - Slice then Now + Slice else Deadline'Last);

   function Place
     (Home    : CPU;
      Wakee   : Deadline;
      Pinned  : Boolean;
      Running : CPU_Keys;
      Queued  : CPU_Keys;
      Allowed : CPU_Flags;
      Margin  : Ticks) return CPU
   is
      Latest : CPU := Home;
      Found  : Boolean := False;
      C      : CPU;
   begin
      if Pinned or else
        (Preempts (Wakee, Running (Home), Margin) and then Queued (Home) >= Wakee)
      then
         return Home;
      end if;

      -- An idle CPU, nearest after Home first.
      C := Home;
      for Step in 1 .. Running'Last loop
         C := (if C = Running'Last then Running'First else C + 1);
         if Allowed (C) and then Idle (Running, Queued, C) then
            return C;
         end if;
         pragma Loop_Invariant (C in Running'Range);
      end loop;

      -- The latest running key the wakee preempts.
      for K in Running'Range loop
         if Allowed (K) and then Preempts (Wakee, Running (K), Margin) and then
           (not Found or else Running (K) > Running (Latest))
         then
            Latest := K;
            Found := True;
         end if;
         pragma Loop_Invariant
           (Latest in Running'Range and then
            (if Found then
               Allowed (Latest) and then
               Preempts (Wakee, Running (Latest), Margin)
             else Latest = Home));
      end loop;
      return Latest;
   end Place;

   function Choose
     (Own     : CPU;
      Heads   : CPU_Keys;
      Allowed : CPU_Flags;
      Margin  : Ticks) return CPU
   is
      Best : CPU := Own;
      Found : Boolean := False;
   begin
      -- The earliest allowed head.
      for K in Heads'Range loop
         if Allowed (K) and then (not Found or else Heads (K) < Heads (Best)) then
            Best := K;
            Found := True;
         end if;
         pragma Loop_Invariant
           (Best in Heads'Range and then
            (if Found then
               Allowed (Best) and then
               (for all C in Heads'First .. K =>
                  (if Allowed (C) then Heads (Best) <= Heads (C)))
             else
               Best = Own and then
               (for all C in Heads'First .. K => not Allowed (C))));
      end loop;
      if Best /= Own and then Heads (Best) /= Idle_Key and then
        Preempts (Heads (Best), Heads (Own), Margin)
      then
         return Best;
      end if;
      return Own;
   end Choose;

end Virtual_Deadlines;
