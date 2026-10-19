package body Compositor_Focus with SPARK_Mode is
   function Topmost (Items : Candidates) return Selection is
   begin
      for I in reverse Items'Range loop
         if Items (I) then return (True, I); end if;
         pragma Loop_Invariant (for all J in Items'Range => (if J >= I then not Items (J)));
      end loop;
      return (False, 0);
   end Topmost;
end Compositor_Focus;
