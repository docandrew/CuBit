with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Input;
with CuBit.Desktop_Protocol; use CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages; use CuBit.Desktop_Messages;
with CuBit.Benchmark_Clock;
with CuBit.Timing_Histograms;

--  Closed-loop diagnostic, NOT an interrupt-to-app or open-loop SLA test.
--  The same process publishes a normalized key and receives the focused
--  surface event through the production desktop protocol. No timing fields
--  or test bypasses are added to either protocol. One outstanding transition
--  makes timeout, resync, wrong key, duplicate and loss fatal, not censored.
procedure Main is
   package Clock renames CuBit.Benchmark_Clock;
   package Timing renames CuBit.Timing_Histograms;
   package Input renames CuBit.Input;
   Consumer : ProcessID := NO_PROCESS;
   Surface : Live_Surface_Name := 1;
   Serial, Sequence, Rate, Ignored : Unsigned_64 := 0;
   Passed : Boolean := True;
   type Scenario is (Continuous, Paced, Repainting);
   Sample_Count : constant := 2_048;
   -- The freestanding runtime discards enumeration image names.
   function Name (Mode : Scenario) return String is
     (case Mode is
         when Continuous => "CONTINUOUS",
         when Paced => "PACED",
         when Repainting => "REPAINTING");

   procedure Fail (Reason : String) is
   begin
      Passed := False;
      debugPrint ("BENCH: FAIL input " & Reason & ASCII.LF);
   end Fail;

   function Send (Wire : Wire_Message) return Wire_Message is
      Msg : Message := From_Wire (Wire);
   begin
      Msg.tag := capCall (CAP_SLOT_DESKTOP, Msg);
      return To_Wire (Msg);
   end Send;

   procedure Run (Mode : Scenario) is
      Samples : Timing.Histogram;
      Publication, Reception : Timing.Histogram;
      Misses : Natural := 0;
      Started, Published, Finished, Deadline : Unsigned_64;
      Report : Input.Source_Report := Input.NULL_SOURCE_REPORT;
      Request : Message;
      Response : Wire_Message;
      Event : Input_Result;
      Expected : Input_Event_Kind;
   begin
      for I in 1 .. Sample_Count loop
         if Mode = Paced then
            Ignored := syscall (SYSCALL_SLEEP, 1);
         elsif Mode = Repainting then
            -- Force ordinary surface damage while the compositor is receiving
            -- input. Completion of this request is not a scanout fence.
            Response := Send (Encode_Present ((Surface, (0, 0, 320, 200))));
            if Decode_Status (Response, Present_Surface) /= Success then
               Fail ("repaint"); return;
            end if;
         end if;
         Sequence := Input.Next_Sequence (Sequence);
         Expected := (if I mod 2 = 1 then Key_Pressed else Key_Released);
         Report.sequence := Sequence;
         Report.generation := 1;
         Report.device := Input.KEYBOARD;
         Report.delivery := Input.ORDERED_TRANSITION;
         -- Up arrow: no additional text event or desktop modifier shortcut.
         Report.payload := (if Expected = Key_Pressed then 16#48# else 16#C8#);
         Request := Input.Encode (Report);
         Deadline := syscall (SYSCALL_GETTIME) + 1_000;
         Started := Clock.Read_Counter;
         if not trySendEvent (Consumer, Request) then
            Fail ("publication rejected"); return;
         end if;
         Published := Clock.Read_Counter;
         Response := Send (Encode_Input_Request
           ((Wait_Input, Surface, Serial, Deadline)));
         Finished := Clock.Read_Counter;
         Event := Decode_Input_Result (Response, Wait_Input);
         if Event.Status /= Success then
            Fail ("input response"); return;
         elsif Event.Value.Kind /= Expected or else
           Event.Value.Payload0 /= 16#48# or else
           Event.Value.Serial /= Serial + 1 or else Event.Value.More_Pending
         then
            Fail ("timeout/loss/duplicate/unexpected event"); return;
         elsif Published < Started or else Finished < Published then
            Fail ("counter backwards"); return;
         end if;
         Serial := Event.Value.Serial;
         Timing.Add (Samples, Finished - Started);
         Timing.Add (Publication, Published - Started);
         Timing.Add (Reception, Finished - Published);
         -- Exact threshold count: histogram buckets are upper bounds and may
         -- straddle 1 ms. Equality misses the strictly-less-than target.
         if Finished - Started >= Rate then Misses := Misses + 1; end if;
      end loop;
      Clock.Report ("input-" & Name (Mode), Samples);
      Clock.Report ("input-publish-call-" & Name (Mode), Publication);
      Clock.Report ("input-receive-call-" & Name (Mode), Reception);
      debugPrint ("INPUT-BENCH: scenario=" & Name (Mode) &
        " delivered=" & Sample_Count'Image & " misses_1ms=" & Misses'Image &
        " failures=0" & ASCII.LF);
   end Run;

   procedure Exercise is
      Created : Creation_Result;
      Response : Wire_Message;
      Event : Input_Result;
   begin
      for Attempt in 1 .. 200 loop
         Consumer := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_KEYBOARD);
         exit when Consumer /= NO_PROCESS and then Consumer =
           getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP);
         Ignored := syscall (SYSCALL_SLEEP, 10);
      end loop;
      if Consumer = NO_PROCESS or else Consumer /=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP)
      then Fail ("desktop unavailable"); return; end if;
      Created := Decode_Creation_Result
        (Send (Encode_Create ((320, 200, Window_Surface))));
      if Created.Status /= Success then Fail ("create"); return; end if;
      Surface := Created.Surface;
      -- Drain startup/configure events before accepting a strictly contiguous
      -- sequence of measured key transitions. Subsequent changes fail.
      loop
         Event := Decode_Input_Result (Send (Encode_Input_Request
           ((Poll_Input, Surface, Serial))), Poll_Input);
         if Event.Status /= Success then Fail ("startup input"); return; end if;
         exit when Event.Value.Kind = No_Input;
         Serial := Event.Value.Serial;
      end loop;
      Clock.Calibrate (Rate);
      if Rate = 0 then Fail ("uncalibrated clock"); return; end if;
      debugPrint ("INPUT-BENCH: START boundary=publication-to-app closed_loop=1" & ASCII.LF);
      for Mode in Scenario loop
         Run (Mode);
         exit when not Passed;
      end loop;
      if Passed then
         debugPrint ("INPUT-BENCH: COMPLETE" & ASCII.LF);
         debugPrint ("BENCH: PASS input integrity" & ASCII.LF);
      end if;
      Response := Send (Encode_Destroy ((Surface => Surface)));
      if Decode_Status (Response, Destroy_Surface) /= Success then
         Fail ("destroy");
      end if;
   end Exercise;
begin
   Exercise;
   Ignored := syscall (SYSCALL_EXIT, (if Passed then 0 else 1));
end Main;
