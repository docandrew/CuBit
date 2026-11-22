package body Compositor_Layout_Restore with SPARK_Mode is
   function Choose (Saved, Fresh : L.Layout) return L.Layout is
   begin
      return (if Compatible (Saved, Fresh) then Saved else Fresh);
   end Choose;
end Compositor_Layout_Restore;
