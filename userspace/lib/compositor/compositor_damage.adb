package body Compositor_Damage with SPARK_Mode is
   procedure Clear (S : out State) is
   begin
      S := (others => <>);
   end Clear;
   procedure Capture (Pending, Frame : in out State; Accepted : out Boolean) is
   begin
      Accepted := Pending.Used > 0 and Frame.Used = 0;
      if not Accepted then return; end if;
      Frame := Pending;
      Clear (Pending);
   end Capture;
   procedure Restore (Pending, Frame : in out State) is
   begin
      if Frame.Used > 0 then Add (Pending, Frame.Extent); end if;
      Clear (Frame);
   end Restore;
   procedure Add (S : in out State; R : Box) is
      Collapse : Boolean := S.Used = Capacity;
      Combined : Box := R;
   begin
      if S.Used > 0 then
         Combined := Envelope (S.Extent, R);
      end if;
      for I in 1 .. S.Used loop
         if Contains (S.Region (I), R) then return; end if;
         Collapse := Collapse or Overlaps (S.Region (I), R);
         pragma Loop_Invariant (S.Used < Capacity or Collapse);
         pragma Loop_Invariant
           (if not Collapse then
              (for all J in 1 .. I => not Overlaps (S.Region (J), R)));
      end loop;
      if Collapse then
         S.Used := 1;
         S.Region (1) := Combined;
      else
         S.Used := S.Used + 1;
         S.Region (S.Used) := R;
      end if;
      S.Extent := Combined;
   end Add;
end Compositor_Damage;
