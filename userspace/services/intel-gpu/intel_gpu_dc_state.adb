package body Intel_GPU_DC_State with SPARK_Mode is
   function Check (Before, After : Snapshot) return Outcome is
      Enable : constant Unsigned_32 := 16#80000000#;
      Ack : constant Unsigned_32 := 16#40000000#;
   begin
      if (for some F in Field => Before (F) = Unsigned_32'Last or After (F) = Unsigned_32'Last) then
         return Invalid_Read;
      end if;
      if Shift_Right (Before (Reference), 29) > 2 or else
        Shift_Right (After (Reference), 29) > 2 or else
        ((Before (PLL) and Enable) /= 0 and then (Before (PLL) and 255) = 0) or else
        ((After (PLL) and Enable) /= 0 and then (After (PLL) and 255) = 0)
      then return Invalid_Clock; end if;
      if Before (Clock_Control) /= After (Clock_Control) or else
        (Before (Reference) and 16#E0000000#) /= (After (Reference) and 16#E0000000#) or else
        (Before (PLL) and 16#800000FF#) /= (After (PLL) and 16#800000FF#)
      then return Clock_Changed; end if;
      -- Reject in-flight frequency-crawl requests/acks in either snapshot.
      if ((Before (PLL) or After (PLL)) and 16#00C00000#) /= 0 or else
        ((After (PLL) and Enable) /= 0) /= ((After (PLL) and Ack) /= 0)
      then return Clock_Unsettled; end if;
      for F in Buffer_0 .. Buffer_3 loop
         if (Before (F) and not Ack) /= (After (F) and not Ack) then
            return Buffer_Changed;
         end if;
         if ((After (F) and Enable) /= 0) /= ((After (F) and Ack) /= 0) then
            return Buffer_Unsettled;
         end if;
      end loop;
      return Preserved;
   end Check;
end Intel_GPU_DC_State;
