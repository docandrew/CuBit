package body Compositor_Backend_Selection with SPARK_Mode is
   procedure Select_Backend (S : in out State; Evidence : Readiness; Accepted : out Boolean) is
   begin
      Accepted := S.Selected = Unselected;
      if Accepted then S.Selected := (if Ready (Evidence) then GPU else Software); end if;
   end Select_Backend;
   procedure Begin_Output (S : in out State) is
   begin
      if S.Selected = Unselected then S.Selected := Software; end if;
   end Begin_Output;
   procedure Request_Recovery (S : in out State; Key : Recovery_Key; Accepted : out Boolean) is
   begin
      Accepted := S.Selected = GPU and S.Recovering = Idle and Valid (Key);
      if Accepted then S.Key := Key; S.Recovering := Draining; end if;
   end Request_Recovery;
   procedure Observe_Recovery (S : in out State; Key : Recovery_Key;
      Evidence : Drain_Evidence; Switched : out Boolean) is
   begin
      Switched := False;
      if S.Recovering /= Draining or else Key /= S.Key then return; end if;
      if Evidence.Uncertain then S.Recovering := Quarantined;
      elsif Drained (Evidence) then
         S.Selected := Software; S.Recovering := Recovered; Switched := True;
      end if;
   end Observe_Recovery;
end Compositor_Backend_Selection;
