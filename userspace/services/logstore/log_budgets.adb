package body Log_Budgets with SPARK_Mode is
   function Remaining
     (Item : Limiter; Budget : CuBit.Log_Protocol.Budget_Id) return Credits is
     (Item.Pools (Budget).Available);
   function Rejected
     (Item : Limiter; Budget : CuBit.Log_Protocol.Budget_Id) return Unsigned_64 is
     (Item.Pools (Budget).Dropped);

   procedure Advance_Time (Item : in out Limiter; Now_Ms : Unsigned_64) is
      Elapsed, Added : Unsigned_64;
   begin
      for Pool of Item.Pools loop
         if Now_Ms >= Pool.Last_Refill then
            Elapsed := Now_Ms - Pool.Last_Refill;
            if Pool.Available = Burst then
               --  No saved partial credit while full: no oversized burst
               --  immediately after first use or a long idle interval.
               Pool.Last_Refill := Now_Ms;
            else
               Added := Elapsed / Refill_Ms;
               if Added >= Burst - Pool.Available then
                  Pool.Available := Burst;
                  Pool.Last_Refill := Now_Ms;
               else
                  Pool.Available := Pool.Available + Added;
                  --  Preserve the fractional interval, without multiplying
                  --  or adding potentially unbounded clock values.
                  Pool.Last_Refill := Now_Ms - Elapsed mod Refill_Ms;
               end if;
            end if;
         end if;
      end loop;
   end Advance_Time;

   procedure Admit
     (Item : in out Limiter; Budget : CuBit.Log_Protocol.Budget_Id;
      Accepted : out Boolean) is
      Pool : Bucket renames Item.Pools (Budget);
   begin
      Accepted := Pool.Available > 0;
      if Accepted then
         Pool.Available := Pool.Available - 1;
      elsif Pool.Dropped < Unsigned_64'Last then
         Pool.Dropped := Pool.Dropped + 1;
      end if;
   end Admit;
end Log_Budgets;
