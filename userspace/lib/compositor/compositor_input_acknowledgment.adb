package body Compositor_Input_Acknowledgment with SPARK_Mode is
   procedure Apply (Q : in out IQ.Queue; Close : in out IQ.Word; After : IQ.Word) is
      Before : constant IQ.Queue := Q with Ghost;
   begin
      if Close <= After then Close := 0; end if;
      for I in IQ.Index loop
         if Q (I).Valid and then Q (I).Serial <= After then Q (I) := IQ.Cleared (Q (I)); end if;
         pragma Loop_Invariant
           (for all J in IQ.Index => Q (J) =
             (if J <= I and then Before (J).Valid and then Before (J).Serial <= After
              then IQ.Cleared (Before (J)) else Before (J)));
      end loop;
   end Apply;
end Compositor_Input_Acknowledgment;
