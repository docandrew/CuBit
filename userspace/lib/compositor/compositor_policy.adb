package body Compositor_Policy with SPARK_Mode is
   procedure Begin_Draw (S : in out State) is
   begin
      S := Reading;
   end Begin_Draw;
   procedure Finish_Draw (S : in out State; Result : Completion) is
   begin
      S := (case Result is
              when Rendered => Ready,
              when Access_Unknown => Restart_Required,
              when Rejected | Failed_Quiescent => Disabled);
   end Finish_Draw;
end Compositor_Policy;
