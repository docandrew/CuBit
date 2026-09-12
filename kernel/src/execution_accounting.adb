package body Execution_Accounting with SPARK_Mode is
   procedure Transition
     (S : in out Clock_State; Expected, Next_Owner : Owner_ID;
      Now : Unsigned_64; C : out Charge) is
   begin
      C := (Charged_Owner => S.Current, Ticks => 0, Accepted => False);
      if S.Condition /= Healthy then
         return;
      elsif Expected /= S.Current then
         S.Condition := Owner_Mismatch;
      elsif S.Started and then Now < S.Last then
         S.Condition := Clock_Reversed;
      else
         if S.Started then
            C.Ticks := Now - S.Last;
         end if;
         C.Accepted := True;
         S.Last := Now;
         S.Current := Next_Owner;
         S.Started := True;
      end if;
   end Transition;

   procedure Add_Time (T : in out Totals; Ticks : Unsigned_64) is
   begin
      if Ticks > Unsigned_64'Last - T.Residency_Ticks then
         T.Saturated := True;
      end if;
      T.Residency_Ticks := Saturating_Add (T.Residency_Ticks, Ticks);
   end Add_Time;

   procedure Dispatch (T : in out Totals; Kind : Dispatch_Kind) is
   begin
      case Kind is
         when Scheduled =>
            if T.Scheduled_Dispatches = Unsigned_64'Last then T.Saturated := True; end if;
            T.Scheduled_Dispatches := Saturating_Add (T.Scheduled_Dispatches, 1);
         when Direct_IPC =>
            if T.Direct_Dispatches = Unsigned_64'Last then T.Saturated := True; end if;
            T.Direct_Dispatches := Saturating_Add (T.Direct_Dispatches, 1);
      end case;
   end Dispatch;
end Execution_Accounting;
