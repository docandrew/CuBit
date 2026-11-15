with CuBit.Timing_Histograms;
with Desktop_Logs;
package body Desktop_Latency_Report with SPARK_Mode => Off is
   use Interfaces;
   use Compositor_Latency;
   package TH renames CuBit.Timing_Histograms;
   LF : constant Character := Character'Val (10);
   Percent_50 : constant := 50;
   Percent_90 : constant := 90;
   Percent_99 : constant := 99;
   Expired_At : Unsigned_64 := 0;

   function Decimal (Value : Unsigned_64) return String is
      Text : constant String := Value'Image;
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Decimal;

   function Quantiles (H : TH.Histogram) return String is
     ("n=" & Decimal (Unsigned_64 (TH.Count (H))) &
      " p50=" & Decimal (TH.Quantile_Upper (H, Percent_50)) &
      " p90=" & Decimal (TH.Quantile_Upper (H, Percent_90)) &
      " p99=" & Decimal (TH.Quantile_Upper (H, Percent_99)) &
      " max=" & Decimal (TH.Maximum (H)));

   function Grid_Name (Item : Grid_Kind) return String is
     (case Item is
        when No_Grid => "none", when Hardware_Grid => "hardware", when Virtual_Grid => "virtual");

   procedure Report (S : Stats; Now : Unsigned_64) is
      Elapsed : constant Unsigned_64 := (if Now > S.Started then Now - S.Started else 0);
      Vblanks : constant Unsigned_64 := (if S.Period = 0 then 0 else Elapsed / S.Period);
   begin
      Desktop_Logs.Write
        ("desktop: latency inputs=" & Decimal (S.Inputs) & " shown=" & Decimal (S.Shown) &
         " unshown=" & Decimal (S.Unshown) & " evicted=" & Decimal (S.Evicted) &
         " untimed=" & Decimal (S.Untimed) & " dropped_frames=" & Decimal (S.Dropped_Frames) &
         " grid=" & Grid_Name (S.Grid) & " period_us=" & Decimal (S.Period) &
         " frames_timed=" & Decimal (S.Timed_Frames) & " vblanks=" & Decimal (Vblanks) &
         " interval_us=" & Decimal (Elapsed) &
         " refreshes_late 0=" & Decimal (S.Late (0)) & " 1=" & Decimal (S.Late (1)) &
         " 2=" & Decimal (S.Late (2)) & " 3=" & Decimal (S.Late (3)) &
         " 4+=" & Decimal (S.Late (Late_Bucket'Last)) & LF);
      for P in Phase loop
         declare
            C : Late_Counts renames S.By_Phase (P);
            Total : constant Unsigned_64 := C (0) + C (1) + C (2) + C (3) + C (4);
         begin
            if Total > 0 then
               Desktop_Logs.Write
                 ("desktop: latency phase=" & Decimal (Unsigned_64 (P)) & "/" &
                  Decimal (Phase_Count) & " n=" & Decimal (Total) & " late0=" & Decimal (C (0)) &
                  " late1=" & Decimal (C (1)) & " late2+=" & Decimal (C (2) + C (3) + C (4)) & LF);
            end if;
         end;
      end loop;
      for St in Stage loop
         if TH.Count (S.Stages (St)) > 0 then
            Desktop_Logs.Write
              ("desktop: latency stage=" & Stage_Name (St) & " " & Quantiles (S.Stages (St)) &
               " budget_us=" & (if Budget_Us (St) = No_Budget then "none" else Decimal (Budget_Us (St))) &
               LF);
         end if;
      end loop;
      for Path in Render_Path loop
         declare
            P : Path_Stats renames S.Paths (Path);
         begin
            if P.Frames > 0 then
               Desktop_Logs.Write
                 ("desktop: latency path=" & (case Path is when CPU => "cpu", when GPU => "gpu") &
                  " frames=" & Decimal (P.Frames) & " px_avg=" & Decimal (P.Pixels / P.Frames) &
                  " rects_avg=" & Decimal (P.Rectangles / P.Frames) &
                  " render " & Quantiles (P.Render) &
                  " submit_to_ready " & Quantiles (P.Submit_To_Ready) & LF);
            end if;
         end;
      end loop;
   end Report;

   procedure Tick (L : in out Ledger; S : in out Stats; Now : Unsigned_64) is
   begin
      if Now = Unavailable then
         return;
      end if;
      if S.Started = Unavailable then
         S.Started := Now;
      end if;
      if Now >= Expired_At + Expire_Interval_Us or else Now < Expired_At then
         Expired_At := Now;
         Expire (L, S, Now);
      end if;
      if Now >= S.Started and then Now - S.Started < Report_Interval_Us then
         return;
      end if;
      Take_Counts (L, S);
      if S.Inputs > 0 or else S.Paths (CPU).Frames > 0 or else S.Paths (GPU).Frames > 0 then
         Report (S, Now);
      end if;
      Reset (S, Now);
   end Tick;
end Desktop_Latency_Report;
