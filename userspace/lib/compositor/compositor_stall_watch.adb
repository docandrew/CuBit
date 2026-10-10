package body Compositor_Stall_Watch with SPARK_Mode is
   procedure Completed (S : in out State) is
   begin
      S.Armed := False;
   end Completed;
   procedure Retried (S : in out State; Now : Millis; Uploads : Count;
      Deadline : Millis; Stalled : out Boolean) is
   begin
      if Reopens (S, Now, Uploads, Deadline) then
         S := (True, Now, Now, Uploads); Stalled := False;
      else
         Stalled := Now >= S.Since and then Now - S.Since >= Deadline;
         S.Last := Now;
      end if;
   end Retried;
end Compositor_Stall_Watch;
