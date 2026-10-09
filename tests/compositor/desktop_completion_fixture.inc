   -- Test-only declarations inserted into a private copy of Desktop main.
   -- The real renderer completes normally; the fixture withholds observation.
   type Completion_Probe is record
      Active, Injected, Publish_Expected : Boolean := False;
      Frames, Polls : Natural := 0;
      Writer : BP.Ticket := BP.None;
      Buffer : System.Address := System.Null_Address;
      Frame : Compositor_Damage.State;
      Started : Unsigned_64 := 0;
   end record;
   Completion_Probes : array (Output_Index) of Completion_Probe;
   Fixture_Unsafe : constant Boolean := @UNSAFE@;
   Fixture_Damage : constant Compositor_Damage.Box := (0, 0, 1, 1);

   procedure Probe_Check (Condition : Boolean; Detail : String) is
   begin
      if not Condition then
         debugPrint ("FAIL completion fixture: " & Detail & LF);
         exitCompositor (97);
      end if;
   end Probe_Check;

   type Begin_Probe is record
      Attempts : Natural := 0;
      Opened : Boolean := False;
      Damage, Frame : Compositor_Damage.State;
      Repaint : RP.State;
      Pool : BP.State;
      Buffer : System.Address := System.Null_Address;
   end record;
   Begin_Probes : array (Output_Index) of Begin_Probe;

   procedure Probe_Begin
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Writer : BP.Ticket; Screen : DG.Output; Secondary : Boolean;
      Result : out Desktop_Compositor.Output_Start;
      Capture_Repair : in out Compositor_Damage.State; Writer_Repair : Compositor_Damage.State) is
      Output : constant Output_Index := (if Secondary then 1 else 0);
      P : Output_Presentation renames presentations (Output);
      S : Begin_Probe renames Begin_Probes (Output);
      use type Desktop_Compositor.Output_Start;
   begin
      Probe_Check (not S.Opened and not Completion_Probes (Output).Active,
                   "begin during pending frame");
      S.Attempts := S.Attempts + 1;
      S.Damage := P.Damage;
      S.Frame := P.Frame_Damage;
      S.Repaint := P.Repaint;
      S.Pool := P.Pool;
      S.Buffer := Target.Pixels;
      if S.Attempts <= 3 then
         Result := Desktop_Compositor.Deferred;
         return;
      end if;
      Probe_Check (Writer = BP.Writer (P.Pool), "begin writer mismatch");
      Desktop_Compositor.Begin_Output (Target, Target_Bytes, Writer, Screen, Secondary, Result, Capture_Repair, Writer_Repair);
      Probe_Check (Result = Desktop_Compositor.Started, "real begin failed");
      S.Opened := True;
   end Probe_Begin;

   procedure Probe_Deferred (Output : Output_Index) is
      P : Output_Presentation renames presentations (Output);
      S : Begin_Probe renames Begin_Probes (Output);
      use type Compositor_Damage.State, RP.State, BP.State;
   begin
      Probe_Check (not S.Opened and P.Damage = S.Damage and
                   P.Frame_Damage = S.Frame and P.Repaint = S.Repaint and
                   P.Pool = S.Pool and P.Buffer = S.Buffer,
                   "deferred begin consumed damage or changed ownership");
      if S.Attempts = 3 then
         debugPrint ("PASS begin fixture: deferred damage and ownership output=" &
                     Output'Image & LF);
      end if;
   end Probe_Deferred;

   procedure Probe_Completion
     (Target : System.Address; Writer : BP.Ticket; Secondary, Poll : Boolean;
      Result : out Desktop_Compositor.Render_Completion) is
      Output : constant Output_Index := (if Secondary then 1 else 0);
      P : Output_Presentation renames presentations (Output);
      S : Completion_Probe renames Completion_Probes (Output);
      use type Compositor_Damage.State, Desktop_Compositor.Render_Completion;
   begin
      Probe_Check (Begin_Probes (Output).Opened, "completion without begin");
      Probe_Check (Writer = BP.Writer (P.Pool), "completion writer mismatch");
      Desktop_Compositor.Complete_Output (Target, Writer, Secondary, Poll, Result);
      Probe_Check (Result = Desktop_Compositor.Complete, "real renderer unsafe");
      Probe_Check (BP.Rendering (P.Pool), "writer not rendering");
      if not Poll then
         Probe_Check (not S.Active and not S.Publish_Expected, "duplicate start");
         S.Frames := S.Frames + 1;
         S.Polls := 0;
         S.Active := True;
         S.Writer := BP.Writer (P.Pool);
         S.Buffer := Target;
         S.Frame := P.Frame_Damage;
         S.Started := nowMs;
         S.Injected := S.Frames = 1;
         if S.Injected then
            -- Synthetic fresh damage uses the same incoming state as input.
            Compositor_Damage.Add (P.Damage, Fixture_Damage);
            RP.Invalidate (P.Repaint, Fixture_Damage);
            debugPrint ("completion fixture: held output=" & Output'Image & LF);
         end if;
      else
         Probe_Check (S.Active, "poll without held frame");
         Probe_Check (BP.Writer (P.Pool) = S.Writer and Target = S.Buffer,
                      "held writer changed");
         Probe_Check (P.Frame_Damage = S.Frame, "held damage changed");
         S.Polls := S.Polls + 1;
      end if;
      if S.Frames <= 8 and then (S.Polls < 3 or else nowMs - S.Started < 10) then
         Result := Desktop_Compositor.Pending;
         return;
      end if;
      if Fixture_Unsafe and S.Frames = 1 then
         debugPrint ("completion fixture: injecting unsafe output=" & Output'Image & LF);
         Result := Desktop_Compositor.Unsafe;
         return;
      end if;
      Begin_Probes (Output).Opened := False;
      S.Active := False;
      S.Publish_Expected := True;
   end Probe_Completion;

   procedure Probe_Published (Output : Output_Index) is
      S : Completion_Probe renames Completion_Probes (Output);
      P : Output_Presentation renames presentations (Output);
   begin
      Probe_Check (not S.Active and S.Publish_Expected, "publication before completion");
      Probe_Check (Compositor_Damage.Count (P.Frame_Damage) = 0,
                   "submitted snapshot not cleared");
      S.Publish_Expected := False;
      if S.Injected then
         Probe_Check (Compositor_Damage.Covers (P.Damage, Fixture_Damage),
                      "fresh damage lost on publication");
         debugPrint ("PASS completion fixture: retained writer and fresh damage output=" &
                     Output'Image & " polls=" & S.Polls'Image & LF);
      end if;
      if S.Frames = 2 then
         Probe_Check (Compositor_Damage.Covers (S.Frame, Fixture_Damage),
                      "fresh damage absent from next frame");
         debugPrint ("PASS completion fixture: fresh damage rendered output=" & Output'Image & LF);
      end if;
   end Probe_Published;

   procedure Probe_Idle is
   begin
      for Output in Output_Index loop
         if presentations (Output).Enabled then
            Probe_Check (Begin_Probes (Output).Attempts = 0 or else
                         Begin_Probes (Output).Attempts > 3,
                         "indefinite idle wait during deferred begin");
         end if;
      end loop;
   end Probe_Idle;

