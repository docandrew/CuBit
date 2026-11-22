pragma Ada_2022;
package body Publisher_Slots with SPARK_Mode is
   function Choose (From, With_Authority : Unsigned_64; May_Grow : Boolean) return Choice is
      Last : constant Natural := Count;
   begin
      for I in 1 .. Last loop
         if Live (I) and then Owner (I) = From and then Authority (I) = With_Authority then
            return (Kind => Same_Publisher, Index => I);
         end if;
      end loop;
      for I in 1 .. Last loop
         if not Live (I) then
            return (Kind => Free_Entry, Index => I);
         end if;
         pragma Loop_Invariant (for all J in 1 .. I => Live (J));
      end loop;
      return (if May_Grow then (Kind => Grow) else (Kind => No_Room));
   end Choose;
end Publisher_Slots;
