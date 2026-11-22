package body Compositor_Frame_Replacement with SPARK_Mode is
   procedure Replace
     (Transfer : in out CP.State; Pool : in out BP.State;
      Pending, Frame : in out D.State; Now : CP.ID; Replaced : out Boolean) is
   begin
      Replaced := Eligible (Transfer, Pool, Pending, Frame, Now);
      if not Replaced then return; end if;
      CP.Cancel (Transfer);
      BP.Retire_Display (Pool, BP.Displayed (Pool), True);
      D.Restore (Pending, Frame);
   end Replace;
end Compositor_Frame_Replacement;
