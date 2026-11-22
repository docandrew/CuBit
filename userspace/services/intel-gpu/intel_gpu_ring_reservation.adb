package body Intel_GPU_Ring_Reservation with SPARK_Mode is
   function Reserve (Retired_Head, Current_Tail, Bytes : Unsigned_32) return Plan is
      Distance, Space : Unsigned_32;
      Result : Plan;
   begin
      if Retired_Head >= Ring_Bytes or else Retired_Head mod 4 /= 0 or else
        Current_Tail >= Ring_Bytes or else Current_Tail mod 8 /= 0 or else
        Bytes = 0 or else Bytes mod 8 /= 0 or else Bytes > Ring_Bytes - Guard_Bytes
      then return Result; end if;
      -- Avoid modular underflow: a sub-cacheline gap is NOT almost a whole
      -- free ring. Such observations cannot authorize a write.
      Distance := (if Retired_Head > Current_Tail then Retired_Head - Current_Tail
                   else Ring_Bytes - Current_Tail + Retired_Head);
      Result.Status := No_Space;
      if Distance < Guard_Bytes then return Result; end if;
      Space := Distance - Guard_Bytes;
      Result.Start := Current_Tail;
      if Current_Tail > Ring_Bytes - Guard_Bytes - Bytes
      then
         Result.Padding := Ring_Bytes - Current_Tail;
         Result.Start := 0;
      end if;
      Result.Consumed := Result.Padding + Bytes;
      if Result.Consumed > Space then return (Status => No_Space, others => 0); end if;
      Result.Tail := Result.Start + Bytes;
      Result.Status := Ready;
      return Result;
   end Reserve;
end Intel_GPU_Ring_Reservation;
