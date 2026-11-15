pragma Ada_2022;
with Ada.Text_IO; with Interfaces; with GPIO_IRQ_Turn;
procedure IRQ_Turn_Tests is
   subtype Word is Interfaces.Unsigned_64; use type Word;
   Now : Word := 10;
   Clock_OK, Sweep_OK, Clear, Uncertain, ACK_OK : Boolean := True;
   Sweeps, Steps, Inspections, ACKs, Checks : Natural := 0;
   Last_Deadline : Word := 0;
   Step_Elapsed : Word := 0;
   procedure Clock (Value : out Word; Available : out Boolean) is
   begin Value := Now; Available := Clock_OK; end Clock;
   procedure Sweep (Deadline : Word; Accepted : out Boolean) is
   begin Sweeps := Sweeps + 1; Last_Deadline := Deadline; Accepted := Sweep_OK; end Sweep;
   procedure Step is begin Steps := Steps + 1; Now := Now + Step_Elapsed; end Step;
   procedure Inspect (Quiet, Unknown : out Boolean) is
   begin Inspections := Inspections + 1; Quiet := Clear; Unknown := Uncertain; end Inspect;
   procedure ACK (Accepted : out Boolean) is
   begin ACKs := ACKs + 1; Accepted := ACK_OK; end ACK;
   package Owner is new GPIO_IRQ_Turn (Clock, Sweep, Step, Inspect, ACK, 20, 2);
   use type Owner.Phase, Owner.Failure;
   procedure Check (Value : Boolean) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Natural'Image (Checks); end if; end Check;
   procedure Reset is
   begin Now := 10; Clock_OK := True; Sweep_OK := True; Clear := True;
      Uncertain := False; ACK_OK := True; Sweeps := 0; Steps := 0; Inspections := 0; ACKs := 0; Step_Elapsed := 0; end Reset;
begin
   Reset;
   declare S : Owner.State; begin
      Owner.Note (S, False); Owner.Turn (S);
      Check (Owner.Observe (S).At_Phase = Owner.Idle and Sweeps = 0 and ACKs = 0);
      Owner.Note (S, True); Owner.Note (S, True); Clear := False;
      Owner.Turn (S); Check (Sweeps = 1 and Steps = 1 and ACKs = 0 and Last_Deadline = 30);
      Check (Owner.Observe (S).Parent_Held and Owner.Observe (S).Wake_At = 12);
      Now := 12; Owner.Note (S, True); Owner.Turn (S);
      Check (Sweeps = 1 and Steps = 2 and Owner.Observe (S).Deadline = 30);
      Clear := True; Owner.Turn (S); Check (Sweeps = 2 and ACKs = 0 and Last_Deadline = 30);
      Owner.Turn (S); Check (ACKs = 1 and Owner.Observe (S).At_Phase = Owner.Idle);
      Owner.Turn (S); Check (ACKs = 1 and not Owner.Observe (S).Parent_Held);
      Now := 13; Owner.Note (S, True); Owner.Turn (S);
      Check (ACKs = 2 and Last_Deadline = 33);
   end;
   for Failure in Owner.Failure range Owner.No_Clock .. Owner.Acknowledge_Refused loop
      Reset;
      declare S : Owner.State; begin
         Owner.Note (S, True);
         case Failure is
            when Owner.No_Clock => Clock_OK := False;
            when Owner.Clock_Reversed => Clear := False; Owner.Turn (S); Now := 9;
            when Owner.Clock_Exhausted => Now := Word'Last;
            when Owner.Deadline_Expired => Clear := False; Owner.Turn (S); Owner.Note (S, True); Now := 30;
            when Owner.Sweep_Refused => Sweep_OK := False;
            when Owner.Source_Uncertain => Uncertain := True;
            when Owner.Acknowledge_Refused => ACK_OK := False;
         end case;
         Owner.Turn (S); Check (Owner.Observe (S).Reason = Failure);
         Check (Owner.Observe (S).Parent_Held and not Owner.Observe (S).Needs_Progress);
         Check (ACKs = (if Failure = Owner.Acknowledge_Refused then 1 else 0));
         declare Before : constant Natural := Steps + Sweeps + ACKs; begin
            Owner.Note (S, True); Owner.Turn (S); Check (Before = Steps + Sweeps + ACKs);
         end;
      end;
   end loop;
   Reset;
   declare S : Owner.State; begin
      Step_Elapsed := 20; Owner.Note (S, True); Owner.Turn (S);
      Check (Owner.Observe (S).Reason = Owner.Deadline_Expired and ACKs = 0);
   end;
   Reset;
   declare S : Owner.State; begin
      Now := Word'Last - 4; Clear := False; Owner.Note (S, True); Owner.Turn (S);
      Check (Last_Deadline = Owner.Last_Finite and Owner.Observe (S).Wake_At = Word'Last - 2);
      Now := Owner.Last_Finite; Owner.Turn (S); Check (Owner.Observe (S).Reason = Owner.Clock_Exhausted and ACKs = 0);
   end;
   Ada.Text_IO.Put_Line ("GPIO IRQ turn PASS" & Natural'Image (Checks));
end IRQ_Turn_Tests;
