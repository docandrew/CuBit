package body Compositor_Pool with SPARK_Mode is
   function Open (New_Epoch : Live_ID) return State is
     ((Generation => New_Epoch, others => <>));
   procedure Acquire
     (S : in out State; T : out Ticket; Replace_Ready : Boolean := False) is
      B : Live_Slot;
   begin
      T := None;
      if S.Failed or S.W /= None then return; end if;
      if S.Generation = 0 or S.Sequence = ID'Last then
         S.Failed := True;
         return;
      end if;
      if Free (S, 1) then B := 1;
      elsif Free (S, 2) then B := 2;
      elsif Free (S, 3) then B := 3;
      elsif Replace_Ready and S.R /= None then
         B := S.R.Buffer;
         S.R := None;
      else return;
      end if;
      S.Sequence := S.Sequence + 1;
      S.W := (B, S.Generation, S.Sequence);
      T := S.W;
   end Acquire;
   procedure Abandon_Writer (S : in out State; T : Ticket; Accepted : out Boolean) is
   begin
      Accepted := Writable (S, T);
      if Accepted then S.W := None; end if;
   end Abandon_Writer;
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
   procedure Discard_Ready (S : in out State; T : Ticket) is
   begin
      if not S.Failed and T /= None and T = S.R then S.R := None;
      else S.Failed := True;
      end if;
   end Discard_Ready;
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
   procedure Latch_Display
     (S : in out State; T, Previous : Ticket; Confirmed : Boolean) is
   begin
      if not S.Failed and T /= None and T = S.D and Previous = S.F and Confirmed then
         S.F := S.D;
         S.D := None;
      else S.Failed := True; end if;
   end Latch_Display;
   procedure Retire_Front (S : in out State; T : Ticket; Confirmed : Boolean) is
   begin
      if not S.Failed and T /= None and T = S.F and Confirmed then S.F := None;
      else S.Failed := True; end if;
   end Retire_Front;
   procedure Take_Readback (S : in out State; T : out Ticket) is
   begin
      T := None;
      if not S.Failed and S.B = None and S.R /= None then
         S.B := S.R;
         S.R := None;
         T := S.B;
      end if;
   end Take_Readback;
   procedure Retire_Readback
     (S : in out State; T : Ticket; Transfer_Complete, CPU_Drained : Boolean) is
   begin
      if not S.Failed and T /= None and T = S.B and
         Transfer_Complete and CPU_Drained
      then S.B := None;
      else S.Failed := True;
      end if;
   end Retire_Readback;
end Compositor_Pool;
