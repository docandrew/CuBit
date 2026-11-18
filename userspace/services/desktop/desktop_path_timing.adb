------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Benchmark_Clock;
with CuBit.Published_Clock;
with Desktop_Timing_Policy;

package body Desktop_Path_Timing is

   Depth_Limit : constant := 16;
   type Stack_Array is array (1 .. Depth_Limit) of Path;
   Stack : Stack_Array := [others => Fill];
   Depth : Natural := 0;
   --  Ticks when the path on top of the stack last started running, and
   --  when the frame began.
   Running_Since, Frame_Since : Unsigned_64 := 0;
   In_Frame : Boolean := False;

   Ticks, Pixels : Path_Counts := [others => 0];
   Reasons : Reason_Counts := [others => 0];

   Last : Frame_Paths;
   Period : Frame_Paths;
   Period_Frames : Unsigned_64 := 0;

   function Now return Unsigned_64 is (CuBit.Benchmark_Clock.Read_Counter);

   function Micros (T : Unsigned_64) return Unsigned_64 is
      Hz : constant Unsigned_64 := CuBit.Published_Clock.Counter_Frequency;
   begin
      if Hz < 1_000_000 then
         return 0;
      end if;
      return T / (Hz / 1_000_000);
   end Micros;

   procedure Charge_Running (At_Tick : Unsigned_64) is
   begin
      if Depth > 0 and then At_Tick >= Running_Since then
         Ticks (Stack (Depth)) := Ticks (Stack (Depth)) + (At_Tick - Running_Since);
      end if;
      Running_Since := At_Tick;
   end Charge_Running;

   procedure Enter (P : Path) is
   begin
      if not Desktop_Timing_Policy.Enabled or else not In_Frame then
         return;
      end if;
      Charge_Running (Now);
      if Depth < Depth_Limit then
         Depth := Depth + 1;
         Stack (Depth) := P;
      end if;
   end Enter;

   procedure Leave (Pixels : Unsigned_64 := 0) is
   begin
      if not Desktop_Timing_Policy.Enabled or else not In_Frame or else Depth = 0 then
         return;
      end if;
      Charge_Running (Now);
      Desktop_Path_Timing.Pixels (Stack (Depth)) := Desktop_Path_Timing.Pixels (Stack (Depth)) + Pixels;
      Depth := Depth - 1;
   end Leave;

   procedure Sampler_Because (Why : Sampler_Reason; Pixels : Unsigned_64) is
   begin
      if Desktop_Timing_Policy.Enabled and then In_Frame then
         Reasons (Why) := Reasons (Why) + Pixels;
      end if;
   end Sampler_Because;

   procedure Begin_Frame is
   begin
      if not Desktop_Timing_Policy.Enabled then
         return;
      end if;
      Ticks := [others => 0];
      Pixels := [others => 0];
      Reasons := [others => 0];
      Depth := 0;
      Frame_Since := Now;
      Running_Since := Frame_Since;
      In_Frame := True;
   end Begin_Frame;

   procedure End_Frame (Merged_Pixels : Unsigned_64) is
      End_Tick : Unsigned_64;
      Claimed, Written : Unsigned_64 := 0;
      F : Frame_Paths;
   begin
      if not Desktop_Timing_Policy.Enabled or else not In_Frame then
         return;
      end if;
      End_Tick := Now;
      Charge_Running (End_Tick);
      In_Frame := False;
      Depth := 0;
      for P in Path loop
         F.Pixels (P) := Pixels (P);
         F.Micros (P) := Micros (Ticks (P));
         Claimed := Claimed + Ticks (P);
         --  A layer build writes the private layer, not the frame.
         if P /= Backdrop_Build then
            Written := Written + Pixels (P);
         end if;
      end loop;
      F.Sampler_Pixels := Reasons;
      F.Total_Micros := (if End_Tick >= Frame_Since then Micros (End_Tick - Frame_Since) else 0);
      F.Other_Micros :=
        (if End_Tick >= Frame_Since and then End_Tick - Frame_Since >= Claimed
         then Micros (End_Tick - Frame_Since - Claimed) else 0);
      F.Overdraw_Pixels := (if Written > Merged_Pixels then Written - Merged_Pixels else 0);
      Last := F;
      for P in Path loop
         Period.Pixels (P) := Period.Pixels (P) + F.Pixels (P);
         Period.Micros (P) := Period.Micros (P) + F.Micros (P);
      end loop;
      for R in Sampler_Reason loop
         Period.Sampler_Pixels (R) := Period.Sampler_Pixels (R) + F.Sampler_Pixels (R);
      end loop;
      Period.Other_Micros := Period.Other_Micros + F.Other_Micros;
      Period.Total_Micros := Period.Total_Micros + F.Total_Micros;
      Period.Overdraw_Pixels := Period.Overdraw_Pixels + F.Overdraw_Pixels;
      Period_Frames := Period_Frames + 1;
   end End_Frame;

   function Last_Frame return Frame_Paths is (Last);

   procedure Take_Period (Totals : out Frame_Paths; Frames : out Unsigned_64) is
   begin
      Totals := Period;
      Frames := Period_Frames;
      Period := (others => <>);
      Period_Frames := 0;
   end Take_Period;

   function Name (P : Path) return String is
     (case P is
        when Client_Row_Copy => "client_row_copy", when Client_Sampler => "client_sampler",
        when Fill => "fill", when Backdrop => "backdrop",
        when Backdrop_Build => "backdrop_build", when Blend => "blend",
        when Text => "text", when Icon => "icon", when Chrome => "chrome",
        when Deskbar => "deskbar", when Cursor => "cursor");

   function Name (R : Sampler_Reason) return String is
     (case R is
        when Scaled => "scaled", when Size_Differs => "size_differs",
        when Rotated => "rotated", when Other_Reason => "other");

end Desktop_Path_Timing;
