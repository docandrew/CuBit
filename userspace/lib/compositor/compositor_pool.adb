package body Compositor_Pool with SPARK_Mode is
   function Open (New_Epoch : Live_ID) return State is
     ((Generation => New_Epoch, others => <>));
   procedure Acquire (S : in out State; T : out Ticket) is
      B : Live_Slot;
   begin
      T := None;
      if S.Failed or S.W /= None then return; end if;
      if S.Generation = 0 or S.Sequence = ID'Last then
         S.Failed := True;
         return;
      end if;
      if S.R.Buffer /= 1 and S.D.Buffer /= 1 then B := 1;
      elsif S.R.Buffer /= 2 and S.D.Buffer /= 2 then B := 2;
      else B := 3;
      end if;
      S.Sequence := S.Sequence + 1;
      S.W := (B, S.Generation, S.Sequence);
      T := S.W;
   end Acquire;
   procedure Start_Render (S : in out State; T : Ticket) is
   begin
      if Writable (S, T) then S.GPU_Busy := True;
      else S.Failed := True;
      end if;
   end Start_Render;
   procedure Finish_Render (S : in out State; T : Ticket; Result : Render_Outcome) is
   begin
      if not S.Failed and S.GPU_Busy and T = S.W and Result /= Unknown then
         if Result = Completed then S.R := T; end if;
         S.W := None;
         S.GPU_Busy := False;
      else S.Failed := True;
      end if;
   end Finish_Render;
   procedure Present (S : in out State; T : out Ticket) is
   begin
      T := None;
      if not S.Failed and S.D = None and S.R /= None then
         S.D := S.R;
         S.R := None;
         T := S.D;
      end if;
   end Present;
   procedure Retire_Display (S : in out State; T : Ticket; Released : Boolean) is
   begin
      if not S.Failed and T /= None and T = S.D and Released then
         S.D := None;
      else S.Failed := True;
      end if;
   end Retire_Display;
end Compositor_Pool;
