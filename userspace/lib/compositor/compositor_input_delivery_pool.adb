package body Compositor_Input_Delivery_Pool with SPARK_Mode is
   procedure Publish
     (S : in out State; Sender, Target : W.Identity;
      Grant : GR.Reference; Payload : W.Snapshot_Words;
      Result : out Delivery.Outcome)
   is
      Available : Slot := 1;
      Found : Boolean := False;
   begin
      for I in 1 .. S.Maximum loop
         if Pending (S, I) then
            if Owner (S, I) = Sender and then Surface (S, I) = Target then
               Result := Delivery.Busy;
               return;
            end if;
         elsif not Found then
            Available := I;
            Found := True;
         end if;
         pragma Loop_Invariant (if Found then Available <= S.Maximum and then not Pending (S, Available));
         pragma Loop_Invariant
           (for all J in 1 .. I =>
             not (Pending (S, J) and then Owner (S, J) = Sender and then Surface (S, J) = Target));
      end loop;
      if not Found then Result := Delivery.Busy; return; end if;
      Delivery.Deliver (S.Items (Available).Loan, Sender, Grant, Payload, Result);
      S.Items (Available).Owner := Sender;
      S.Items (Available).Surface := Target;
   end Publish;

   procedure Poll (S : in out State) is
   begin
      Delivery.Retire (S.Items (S.Current).Loan);
      S.Current := Next (S, S.Current);
   end Poll;

   procedure Grow (From : State; Into : out State) is
      Empty : Entry_State;
   begin
      Into.Items := [others => Empty];
      Into.Items (1 .. From.Maximum) := From.Items;
      Into.Current := From.Current;
   end Grow;
end Compositor_Input_Delivery_Pool;
