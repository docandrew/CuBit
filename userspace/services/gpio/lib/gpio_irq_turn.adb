pragma Ada_2022;
package body GPIO_IRQ_Turn with SPARK_Mode is
   function After (Now, Window : Word) return Word is
     (if Now >= Last_Finite or else Window > Last_Finite - Now
      then Last_Finite else Now + Window);
   function Observe (S : State) return Report is (S.View);
   procedure Fail (S : in out State; Why : Failure) is
   begin
      S.View.At_Phase := Faulted; S.View.Reason := Why;
      S.View.Needs_Progress := False;
   end Fail;
   procedure Note (S : in out State; Authenticated_Parent : Boolean) is
   begin
      if not Authenticated_Parent or else S.View.At_Phase = Faulted then return; end if;
      S.View.Parent_Held := True; S.View.Needs_Progress := True;
      case S.View.At_Phase is
         when Idle => S.View.At_Phase := Sweep_Pending;
         when Sweep_Pending => null;
         when Sweeping => S.Sweep_Again := True;
         when Faulted => null;
      end case;
   end Note;
   procedure Turn (S : in out State) is
      Now : Word;
      Available, Accepted, Clear, Uncertain : Boolean;
   begin
      if S.View.At_Phase in Idle | Faulted then return; end if;
      Clock (Now, Available);
      if not Available then Fail (S, No_Clock); return;
      elsif S.Seen_Clock and then Now < S.Previous then Fail (S, Clock_Reversed); return;
      elsif Now >= Last_Finite then Fail (S, Clock_Exhausted); return; end if;
      S.Seen_Clock := True; S.Previous := Now;
      if S.View.At_Phase = Sweep_Pending then
         S.View.Deadline := After (Now, Word (Sweep_Window));
      end if;
      if Now >= S.View.Deadline then Fail (S, Deadline_Expired); return; end if;
      S.View.Wake_At := Word'Min (S.View.Deadline, After (Now, Word (Progress_Interval)));
      if S.View.At_Phase = Sweep_Pending then
         Start_Sweep (S.View.Deadline, Accepted);
         if not Accepted then Fail (S, Sweep_Refused); return; end if;
         S.View.At_Phase := Sweeping;
      end if;
      Advance;
      Inspect (Clear, Uncertain);
      if Uncertain then Fail (S, Source_Uncertain); return; end if;
      if not Clear then return; end if;
      -- MMIO advancement can consume time. Recheck at the parent-ACK boundary.
      Clock (Now, Available);
      if not Available then Fail (S, No_Clock); return;
      elsif Now < S.Previous then Fail (S, Clock_Reversed); return;
      elsif Now >= Last_Finite then Fail (S, Clock_Exhausted); return;
      elsif Now >= S.View.Deadline then Fail (S, Deadline_Expired); return; end if;
      S.Previous := Now;
      if S.Sweep_Again then
         S.Sweep_Again := False;
         Start_Sweep (S.View.Deadline, Accepted);
         if not Accepted then Fail (S, Sweep_Refused); end if;
         return;
      end if;
      Acknowledge (Accepted);
      if not Accepted then Fail (S, Acknowledge_Refused); return; end if;
      S.View := (At_Phase => Idle, Reason => None, others => <>);
   end Turn;
end GPIO_IRQ_Turn;
