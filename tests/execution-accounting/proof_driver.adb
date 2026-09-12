package body Proof_Driver with SPARK_Mode is
   procedure Prove_Handoff (Start, Middle, Finish : Unsigned_64) is
      Whole, Split : A.Clock_State;
      All_Time, First, Second : A.Charge;
   begin
      A.Transition (Whole, 0, 1, Start, All_Time);
      A.Transition (Whole, 1, 0, Finish, All_Time);
      pragma Assert (All_Time.Ticks = Finish - Start);
      A.Transition (Split, 0, 1, Start, First);
      A.Transition (Split, 1, 2, Middle, First);
      pragma Assert (First.Ticks = Middle - Start);
      A.Transition (Split, 2, 0, Finish, Second);
      pragma Assert (Second.Ticks = Finish - Middle);
      pragma Assert (First.Accepted and Second.Accepted and All_Time.Accepted);
      pragma Assert (First.Charged_Owner = 1 and Second.Charged_Owner = 2);
      -- Exact boundary spans are proved above. The hosted chain test also
      -- checks their accumulated totals; no unproved modular-sum lemma kept.
   end Prove_Handoff;
end Proof_Driver;
