package body Compositor_Source_Loans with SPARK_Mode is
   procedure Reserve (S : in out State; T : out Ticket) is
   begin
      T := No_Ticket;
      if S.Last = Serial'Last then return; end if;
      for I in Slot loop
         if S.Items (I).Status = Released then
            S.Last := S.Last + 1;
            S.Items (I) := (S.Last, Reserved);
            T := (I, S.Last);
            return;
         end if;
      end loop;
   end Reserve;
   procedure Cancel (S : in out State; T : Ticket) is
   begin
      S.Items (T.Position).Status := Released;
   end Cancel;
   procedure Activate (S : in out State; T : Ticket) is
   begin
      S.Items (T.Position).Status := Attached;
   end Activate;
   procedure Retire (S : in out State; T : Ticket) is
   begin
      if S.Items (T.Position).Status = Attached then
         S.Items (T.Position).Status := Renderer_Pending;
      end if;
   end Retire;
   procedure Observe_Renderer (S : in out State; T : Ticket; Result : Renderer_Result) is
   begin
      case Result is
         when Retired => S.Items (T.Position).Status := Grant_Pending;
         when Busy => null;
         when Uncertain => S.Items (T.Position).Status := Quarantined;
      end case;
   end Observe_Renderer;
   procedure Observe_Grant (S : in out State; T : Ticket; Confirmed : Boolean) is
   begin
      S.Items (T.Position).Status := (if Confirmed then Released else Quarantined);
   end Observe_Grant;
end Compositor_Source_Loans;
