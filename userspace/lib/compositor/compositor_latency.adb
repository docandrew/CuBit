pragma Ada_2022;
package body Compositor_Latency with SPARK_Mode is
   package TH renames CuBit.Timing_Histograms;
   use type CuBit.Present_Timing.Latch_Source;

   function Elapsed (First, Last : Time_Us) return Unsigned_64 is
     (if First = Unavailable or else Last = Unavailable or else Last < First then 0
      else Last - First);
   function Known (First, Last : Time_Us) return Boolean is
     (First /= Unavailable and then Last /= Unavailable and then First <= Last);

   procedure Add (H : in out TH.Histogram; First, Last : Time_Us) is
   begin
      if Known (First, Last) then
         TH.Add (H, Last - First);
      end if;
   end Add;

   procedure Bump (Item : in out Unsigned_64) is
   begin
      if Item < Unsigned_64'Last then
         Item := Item + 1;
      end if;
   end Bump;

   procedure Raise_By (Item : in out Unsigned_64; By : Unsigned_64) is
   begin
      Item := (if By > Unsigned_64'Last - Item then Unsigned_64'Last else Item + By);
   end Raise_By;

   procedure Observe
     (L : in out Ledger; Kind : Observation_Kind; Value : Unsigned_64; Time : Time_Us;
      Of_Stage : Stage := Stage'First; Path : Render_Path := CPU);
   procedure Observe
     (L : in out Ledger; Kind : Observation_Kind; Value : Unsigned_64; Time : Time_Us;
      Of_Stage : Stage := Stage'First; Path : Render_Path := CPU) is
   begin
      if L.Queued < Observation_Capacity then
         L.Queued := L.Queued + 1;
         L.Queue (L.Queued) := (Kind => Kind, Of_Stage => Of_Stage, Path => Path, Value => Value, Time => Time);
      else
         declare
            R : Lost_Record renames L.Lost (Kind, Of_Stage, Path);
         begin
            if R.Count = 0 then
               R := (Count => 1, First_Us => Time, Last_Us => Time);
            else
               Bump (R.Count);
               R.First_Us := Unsigned_64'Min (R.First_Us, Time);
               R.Last_Us := Unsigned_64'Max (R.Last_Us, Time);
            end if;
         end;
      end if;
   end Observe;

   procedure Tick_Clock (L : in out Ledger; Now : Time_Us);
   procedure Tick_Clock (L : in out Ledger; Now : Time_Us) is
   begin
      if Now /= Unavailable and then Now > L.Clock then
         L.Clock := Now;
      end if;
   end Tick_Clock;

   procedure Drain_Observations (L : in out Ledger) is
   begin
      for I in 1 .. L.Queued loop
         Each (L.Queue (I));
      end loop;
      L.Queued := 0;
      for K in Observation_Kind loop
         for St in Stage loop
            for P in Render_Path loop
               if L.Lost (K, St, P).Count > 0 then
                  Lost (K, St, P, L.Lost (K, St, P).Count, L.Lost (K, St, P).First_Us, L.Lost (K, St, P).Last_Us);
                  L.Lost (K, St, P) := (others => 0);
               end if;
            end loop;
         end loop;
      end loop;
   end Drain_Observations;

   function Grid_At_Or_After (Time, Anchor, Period : Time_Us) return Time_Us is
   begin
      if Time <= Anchor then
         return Anchor - ((Anchor - Time) / Period) * Period;
      end if;
      return Anchor + ((Time - Anchor + Period - 1) / Period) * Period;
   end Grid_At_Or_After;

   function Phase_Of (Origin, Latch, Period : Time_Us) return Phase is
      Since : constant Unsigned_64 :=
        (if Latch >= Origin then (Period - (Latch - Origin) mod Period) mod Period
         else (Origin - Latch) mod Period);
   begin
      return Phase (Since * Phase_Count / Period);
   end Phase_Of;

   procedure Intake (L : in out Ledger; Origin, Now : Time_Us; Source_Sequence : Unsigned_64;
                     Item : out Slot) is
      Oldest : Live_Slot := Live_Slot'First;
   begin
      Item := No_Slot;
      Bump (L.Taken);
      Tick_Clock (L, Now);
      Observe (L, Input_Taken, 1, Now);
      for I in Live_Slot loop
         if L.Inputs (I).State = Free then
            Item := I;
            exit;
         end if;
         if L.Inputs (I).Intake < L.Inputs (Oldest).Intake then
            Oldest := I;
         end if;
      end loop;
      if Item = No_Slot then
         Item := Oldest;
         Bump (L.Evicted);
         Observe (L, Input_Evicted, 1, Now);
      end if;
      L.Inputs (Item) := (State => Routing, Source_Sequence => Source_Sequence,
                          Origin => Origin, Intake => Now, others => <>);
   end Intake;

   procedure Route (L : in out Ledger; Item : Slot; Target, Serial : Unsigned_64) is
   begin
      if Item /= No_Slot and then L.Inputs (Item).State = Routing then
         L.Inputs (Item).State := Queued;
         L.Inputs (Item).Target := Target;
         L.Inputs (Item).Serial := Serial;
      end if;
   end Route;

   procedure Forget_Unrouted (L : in out Ledger; Item : Slot) is
   begin
      if Item /= No_Slot and then L.Inputs (Item).State = Routing then
         L.Inputs (Item) := (others => <>);
      end if;
   end Forget_Unrouted;

   procedure Delivered (L : in out Ledger; Target, Serial : Unsigned_64; Now : Time_Us) is
   begin
      for R of L.Inputs loop
         if R.State = Queued and then R.Target = Target and then R.Serial = Serial then
            R.State := Taken;
            R.Delivered_At := Now;
         end if;
      end loop;
   end Delivered;

   procedure Published (L : in out Ledger; Target, Watermark : Unsigned_64; Now : Time_Us) is
   begin
      if Watermark = 0 then
         return;
      end if;
      for R of L.Inputs loop
         if R.State in Queued | Taken and then R.Target = Target and then R.Serial <= Watermark then
            R.State := Ready;
            R.Published_At := Now;
         end if;
      end loop;
   end Published;

   procedure Drew (L : in out Ledger; Started, Finished : Time_Us) is
   begin
      Raise_By (L.Pending_Draw_Us, Elapsed (Started, Finished));
   end Drew;

   procedure Compose (L : in out Ledger; Output : Output_Number; Path : Render_Path;
                      Pixels, Rectangles : Unsigned_64; Now : Time_Us) is
   begin
      L.Frames (Output) := (Active => True, Path => Path, Started => Now,
                            Draw_Us => L.Pending_Draw_Us, Pixels => Pixels,
                            Rectangles => Rectangles, others => <>);
      L.Pending_Draw_Us := 0;
      for R of L.Inputs loop
         if R.State = Ready then
            R.State := Composed;
            R.Output := Output;
            R.Composed_At := Now;
         end if;
      end loop;
   end Compose;

   procedure Frame_Ready (L : in out Ledger; Output : Output_Number; Now : Time_Us) is
   begin
      if L.Frames (Output).Active then
         L.Frames (Output).Ready_At := Now;
      end if;
   end Frame_Ready;

   procedure Submitted (L : in out Ledger; Output : Output_Number; Token : Unsigned_64;
                        Now : Time_Us) is
      F : Frame_Record := L.Frames (Output);
   begin
      if not F.Active then
         return;
      end if;
      F.Submitted_At := Now;
      F.Token := Token;
      L.Sent (Output) := (Used => True, Token => Token, Frame => F);
      L.Frames (Output).Active := False;
      for R of L.Inputs loop
         if R.State = Composed and then R.Output = Output then
            R.State := Sent;
            R.Token := Token;
            R.Submitted_At := Now;
         end if;
      end loop;
   end Submitted;

   procedure Dropped (L : in out Ledger; S : in out Stats; Token : Unsigned_64) is
   begin
      for O in Output_Number loop
         if L.Sent (O).Used and then L.Sent (O).Token = Token then
            L.Sent (O).Used := False;
            Bump (S.Dropped_Frames);
            Observe (L, Frame_Dropped, 1, L.Clock);
         end if;
      end loop;
      for R of L.Inputs loop
         if R.State = Sent and then R.Token = Token then
            R.State := Ready;
            R.Token := 0;
         end if;
      end loop;
   end Dropped;

   procedure Completed (L : in out Ledger; S : in out Stats; Token : Unsigned_64;
                        Timing : CuBit.Present_Timing.Timing; Now : Time_Us) is
      Timed : constant Boolean :=
        Timing.Source /= CuBit.Present_Timing.None and then CuBit.Present_Timing.Valid (Timing) and then
        Timing.Observed_Us <= Now and then Timing.Observed_Us <= Unavailable / 2;
      Hardware : constant Boolean := Timed and then Timing.Source = CuBit.Present_Timing.Hardware_Latch and then
        Timing.Period > 0;
      Period : constant Unsigned_64 :=
        (if Hardware then Timing.Period else Virtual_Period_Us);
      --  The frame is ready for the latch: armed (hardware), or the host has
      --  it (fence).
      Ready_Us : constant Time_Us :=
        (if not Timed then Unavailable elsif Hardware then Timing.Armed_Us else Timing.Latched_Us);
      --  The vblank that latched it: the hardware's, or the next one on the
      --  virtual grid.
      Latch_Us : constant Time_Us :=
        (if not Timed then Unavailable elsif Hardware then Timing.Latched_Us
         else Grid_At_Or_After (Timing.Latched_Us, 0, Virtual_Period_Us));
      Frame_Found : Boolean := False;
      --  A stage's time: in the interval's histogram and as an observation.
      procedure Stage_Time (St : Stage; First, Last : Time_Us) is
      begin
         if Known (First, Last) then
            TH.Add (S.Stages (St), Last - First);
            Observe (L, Stage_Sample, Last - First, Now, Of_Stage => St);
         end if;
      end Stage_Time;
   begin
      Tick_Clock (L, Now);
      for O in Output_Number loop
         if L.Sent (O).Used and then L.Sent (O).Token = Token then
            declare
               F : constant Frame_Record := L.Sent (O).Frame;
               P : Path_Stats renames S.Paths (F.Path);
            begin
               Frame_Found := True;
               Bump (P.Frames);
               Raise_By (P.Pixels, F.Pixels);
               Raise_By (P.Rectangles, F.Rectangles);
               Observe (L, Damage_Pixels, F.Pixels, Now, Path => F.Path);
               Observe (L, Damage_Rectangles, F.Rectangles, Now, Path => F.Path);
               if Known (F.Started, F.Ready_At) then
                  TH.Add (P.Render, Elapsed (F.Started, F.Ready_At) + F.Draw_Us);
                  Observe (L, Render_Cost, Elapsed (F.Started, F.Ready_At) + F.Draw_Us, Now, Path => F.Path);
               end if;
               Add (P.Submit_To_Ready, F.Submitted_At, Ready_Us);
               if Known (F.Submitted_At, Ready_Us) then
                  Observe (L, Path_Submit_To_Ready, Ready_Us - F.Submitted_At, Now, Path => F.Path);
               end if;
               if Timed and then Latch_Us /= Unavailable then
                  --  Frames against refreshes: how many vblanks since this
                  --  output's previous timed frame (1: every refresh).
                  if L.Last_Latch (O) /= Unavailable and then Latch_Us > L.Last_Latch (O) then
                     Observe (L, Frame_Vblanks, (Latch_Us - L.Last_Latch (O) + Period / 2) / Period, Now);
                  end if;
                  L.Last_Latch (O) := Latch_Us;
               end if;
            end;
            L.Sent (O).Used := False;
         end if;
      end loop;
      if not Frame_Found then
         return;
      end if;
      if Timed then
         Bump (S.Timed_Frames);
         S.Grid := (if Hardware then Hardware_Grid else Virtual_Grid);
         S.Period := Period;
      end if;
      for R of L.Inputs loop
         if R.State = Sent and then R.Token = Token then
            Bump (S.Shown);
            Observe (L, Input_Shown, 1, Now);
            Stage_Time (Driver_To_Intake, R.Origin, R.Intake);
            Stage_Time (Intake_To_Delivery, R.Intake, R.Delivered_At);
            Stage_Time (Delivery_To_Publish, R.Delivered_At, R.Published_At);
            Stage_Time (Publish_To_Compose, R.Published_At, R.Composed_At);
            Stage_Time (Compose_To_Submit, R.Composed_At, R.Submitted_At);
            if Timed then
               Stage_Time (Submit_To_Ready, R.Submitted_At, Ready_Us);
               Stage_Time (Ready_To_Latch, Ready_Us, Latch_Us);
               if Hardware then
                  Stage_Time (Latch_To_Observed, Timing.Latched_Us, Timing.Observed_Us);
               end if;
               Stage_Time (Input_To_Ready, R.Origin, Ready_Us);
               Stage_Time (Input_To_Latch, R.Origin, Latch_Us);
               if R.Origin /= Unavailable and then R.Origin <= Unavailable / 2 then
                  declare
                     Late : constant Late_Bucket := Bucket (Refreshes_Late (R.Origin, Latch_Us, Period));
                     When_In : constant Phase := Phase_Of (R.Origin, Latch_Us, Period);
                  begin
                     Bump (S.Late (Late));
                     Bump (S.By_Phase (When_In) (Late));
                     Observe (L, Late_Sample, Refreshes_Late (R.Origin, Latch_Us, Period), Now);
                  end;
               end if;
            else
               Bump (S.Untimed);
               Observe (L, Input_Untimed, 1, Now);
            end if;
            R := (others => <>);
         end if;
      end loop;
   end Completed;

   procedure Expire (L : in out Ledger; S : in out Stats; Now : Time_Us) is
   begin
      for R of L.Inputs loop
         if R.State /= Free and then Known (R.Intake, Now) and then Now - R.Intake > Expiry_Us
         then
            Bump (S.Unshown);
            Observe (L, Input_Unshown, 1, Now);
            R := (others => <>);
         end if;
      end loop;
   end Expire;

   procedure Take_Counts (L : in out Ledger; S : in out Stats) is
   begin
      Raise_By (S.Inputs, L.Taken);
      Raise_By (S.Evicted, L.Evicted);
      L.Taken := 0;
      L.Evicted := 0;
   end Take_Counts;

   procedure Reset (S : in out Stats; Now : Time_Us) is
   begin
      S.Started := Now;
      S.Inputs := 0;
      S.Shown := 0;
      S.Unshown := 0;
      S.Evicted := 0;
      S.Untimed := 0;
      S.Dropped_Frames := 0;
      S.Late := [others => 0];
      for P in Phase loop
         S.By_Phase (P) := [others => 0];
      end loop;
      for St in Stage loop
         S.Stages (St) := TH.Empty;
      end loop;
      for Path in Render_Path loop
         S.Paths (Path).Frames := 0;
         S.Paths (Path).Pixels := 0;
         S.Paths (Path).Rectangles := 0;
         S.Paths (Path).Render := TH.Empty;
         S.Paths (Path).Submit_To_Ready := TH.Empty;
      end loop;
      S.Grid := No_Grid;
      S.Period := 0;
      S.Timed_Frames := 0;
   end Reset;

   function In_Flight (L : Ledger) return Natural is
      N : Natural := 0;
   begin
      for R of L.Inputs loop
         if R.State /= Free then
            N := N + 1;
         end if;
      end loop;
      return N;
   end In_Flight;
end Compositor_Latency;
