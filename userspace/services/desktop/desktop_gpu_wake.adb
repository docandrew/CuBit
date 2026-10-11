with Compositor_Requests;
with Desktop_GPU_Wake_Hook;
with Desktop_Logs;
package body Desktop_GPU_Wake is
   package Hook renames Desktop_GPU_Wake_Hook;
   package CR renames Compositor_Requests;
   use Interfaces;

   Armed_Token : Unsigned_64 := 0;
   Looked_Again : Boolean := False;

   --  Evidence for the current report period.
   Report_Period_Ms : constant := 1_000;
   Report_Text_Bytes : constant := 512;
   type Counts is record
      Armed, Woken, Idle, Timer, Timer_Sleeps, Event_Sleeps, Event_Capped : Unsigned_64 := 0;
      Hints_Sent, Hints_Coalesced : Unsigned_64 := 0;
      Event_Sleep_Max_Ms, Timer_Sleep_Max_Ms : Unsigned_64 := 0;
   end record;
   Period : Counts;
   Period_Start_Ms : Unsigned_64 := 0;

   function Image (V : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (V);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Report (Now_Ms : Unsigned_64) is
      Mesa : String (1 .. Report_Text_Bytes);
      Last : Natural;
   begin
      if Period_Start_Ms = 0 or else Now_Ms < Period_Start_Ms then
         Period_Start_Ms := Now_Ms;
         return;
      end if;
      if Now_Ms - Period_Start_Ms < Report_Period_Ms then
         return;
      end if;
      Hook.Blocking_Report (Mesa, Last);
      declare
         Jobs : String (1 .. 2 * Report_Text_Bytes);
         Jobs_Last : Natural;
      begin
         Hook.Job_Report (Jobs, Jobs_Last);
         if Jobs_Last >= Jobs'First then
            Desktop_Logs.Write ("desktop: gpu jobs " & Jobs (Jobs'First .. Jobs_Last) & ASCII.LF);
         end if;
      end;
      if Period /= (others => <>) or else Last >= Mesa'First then
         Desktop_Logs.Write
           ("desktop: gpu waits armed=" & Image (Period.Armed) & " woken=" & Image (Period.Woken) &
            " idle=" & Image (Period.Idle) & " timer=" & Image (Period.Timer) &
            " event_sleeps=" & Image (Period.Event_Sleeps) &
            " event_max_ms=" & Image (Period.Event_Sleep_Max_Ms) &
            " event_capped=" & Image (Period.Event_Capped) &
            " timer_sleeps=" & Image (Period.Timer_Sleeps) &
            " timer_max_ms=" & Image (Period.Timer_Sleep_Max_Ms) &
            " boost_hints=" & Image (Period.Hints_Sent) & "/" & Image (Period.Hints_Coalesced) &
            " mesa-blocking: " & (if Last >= Mesa'First then Mesa (Mesa'First .. Last) else "none") &
            ASCII.LF);
      end if;
      Period := (others => <>);
      Period_Start_Ms := Now_Ms;
   end Report;

   procedure Prepare_Wait
     (Rendering : Boolean; Sequence : in out Unsigned_64; Now_Ms : Unsigned_64; Mode : out Wait_Mode)
   is
      Token : Unsigned_64;
   begin
      Report (Now_Ms);
      Mode := Timer;
      if not Rendering then
         Looked_Again := False;
         return;
      end if;
      if Armed_Token /= 0 then
         Mode := Event;
         return;
      end if;
      CR.Allocate (Sequence, Token);
      case Hook.Arm (Token) is
         when Hook.Armed =>
            Armed_Token := Token;
            Period.Armed := Period.Armed + 1;
            Looked_Again := False;
            Mode := Event;
         when Hook.Idle =>
            Period.Idle := Period.Idle + 1;
            --  Done already: look once more without sleeping. Pending work
            --  that is not the GPU's (a CPU renderer, a begin not yet
            --  submitted) keeps the timer after that.
            Mode := (if Looked_Again then Timer else Look_Again);
            Looked_Again := not Looked_Again;
         when Hook.Unavailable =>
            Period.Timer := Period.Timer + 1;
            Looked_Again := False;
      end case;
   end Prepare_Wait;

   procedure Woke (Mode : Wait_Mode; Slept_At_Ms, Now_Ms : Unsigned_64) is
      Slept : constant Unsigned_64 := (if Now_Ms >= Slept_At_Ms then Now_Ms - Slept_At_Ms else 0);
   begin
      case Mode is
         when Event =>
            Period.Event_Sleeps := Period.Event_Sleeps + 1;
            Period.Event_Sleep_Max_Ms := Unsigned_64'Max (Period.Event_Sleep_Max_Ms, Slept);
            --  Ended by the fallback with the wake still armed: the GPU
            --  stage outlasted it (an answer ends the sleep early).
            if Slept >= Event_Fallback_Ms and then Armed_Token /= 0 then
               Period.Event_Capped := Period.Event_Capped + 1;
            end if;
         when Timer =>
            Period.Timer_Sleeps := Period.Timer_Sleeps + 1;
            Period.Timer_Sleep_Max_Ms := Unsigned_64'Max (Period.Timer_Sleep_Max_Ms, Slept);
         when Look_Again => null;
      end case;
   end Woke;

   procedure Input_Hint is
   begin
      if Hook.Boost_Hint then
         Period.Hints_Sent := Period.Hints_Sent + 1;
      else
         Period.Hints_Coalesced := Period.Hints_Coalesced + 1;
      end if;
   end Input_Hint;

   function Matches (Token : Unsigned_64) return Boolean is
     (Token /= 0 and then Token = Armed_Token);

   procedure Collect (C : CuBit.Messages.CompletionEntry; Now_Ms : Unsigned_64) is
   begin
      if not Matches (C.token) then
         return;
      end if;
      --  Answered (reached, failed or ended): the loop looks at its fences.
      if Hook.Collect (C) then
         Period.Woken := Period.Woken + 1;
      end if;
      Armed_Token := 0;
      Report (Now_Ms);
   end Collect;
end Desktop_GPU_Wake;
