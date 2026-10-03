package body Compositor_Focus with SPARK_Mode is
   function Topmost (Items : Candidates) return Selection is
   begin
      for I in reverse Index loop
         if Items (I) then return (True, I); end if;
         pragma Loop_Invariant (for all J in Index => (if J >= I then not Items (J)));
      end loop;
      return (False, Index'First);
   end Topmost;
end Compositor_Focus;
