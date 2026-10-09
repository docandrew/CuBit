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
   procedure Merge_Local (S : in out State; R : Box; Merged : out Boolean)
     with Pre => Valid (S) and Valid (R),
       Post => Valid (S) and
         (if Merged then Count (S) = Count (S'Old) and Count (S) > 0 and Covers (S, R) and
            (for all I in 1 .. Count (S'Old) => Covers (S, Item (S'Old, I))) and
            Bounds (S) = Envelope (Bounds (S'Old), R)
          else S = S'Old);
   procedure Merge_Local (S : in out State; R : Box; Merged : out Boolean) is
      Local : Box;
      Isolated : Boolean;
   begin
      Merged := False;
      for I in 1 .. S.Used loop
         if Overlaps (S.Region (I), R) then
            Local := Envelope (S.Region (I), R);
            Isolated := True;
            for J in 1 .. S.Used loop
               if J /= I and then Overlaps (S.Region (J), Local) then Isolated := False; end if;
               pragma Loop_Invariant
                 (if Isolated then (for all K in 1 .. J =>
                    K = I or else not Overlaps (S.Region (K), Local)));
            end loop;
            if Isolated then
               S.Region (I) := Local;
               S.Extent := Envelope (S.Extent, R);
               Merged := True;
               return;
            end if;
         end if;
         pragma Loop_Invariant (S = S'Loop_Entry and not Merged);
      end loop;
   end Merge_Local;
   procedure Add (S : in out State; R : Box) is
      Collapse : Boolean := S.Used = Capacity;
      Combined : Box := R;
      Merged : Boolean;
   begin
      Merge_Local (S, R, Merged);
      if Merged then return; end if;
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
