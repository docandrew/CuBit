package body Compositor_Target_Damage with SPARK_Mode is
   function Open (Width, Height : Extent) return State is
      S : State;
   begin
      S.Limit := (0, 0, Width, Height);
      for I in P.Live_Slot loop
         D.Add (S.Dirty (I), S.Limit);
         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant (for all J in P.Live_Slot'First .. I => D.Covers (S.Dirty (J), S.Limit));
      end loop;
      return S;
   end Open;
   procedure Change (S : in out State; Region : D.Box) is
   begin
      for I in P.Live_Slot loop
         pragma Assert (D.Valid (S.Dirty (I)));
         D.Add (S.Dirty (I), Region);
         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant (for all J in P.Live_Slot'First .. I => D.Covers (S.Dirty (J), Region));
         pragma Loop_Invariant
           (for all J in P.Live_Slot =>
              (for all K in 1 .. D.Count (S.Dirty'Loop_Entry (J)) =>
                 D.Covers (S.Dirty (J), D.Item (S.Dirty'Loop_Entry (J), K))));
      end loop;
      if S.Writer /= 0 then
         pragma Assert (D.Valid (S.After_Paint));
         D.Add (S.After_Paint, Region);
      end if;
   end Change;
   procedure Begin_Paint (S : in out State; Target : P.Live_Slot) is
   begin
      S.Writer := Target; S.Plan := S.Dirty (Target); D.Clear (S.After_Paint);
   end Begin_Paint;
   procedure Finish (S : in out State; Result : Completion) is
   begin
      if Result = Unknown then S.Failed := True; return; end if;
      if Result = Completed then
         S.Dirty (S.Writer) := S.After_Paint; S.Warm (S.Writer) := True;
      end if;
      S.Writer := 0; D.Clear (S.Plan); D.Clear (S.After_Paint);
   end Finish;
end Compositor_Target_Damage;
