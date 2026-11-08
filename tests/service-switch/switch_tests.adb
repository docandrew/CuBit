--  Hosted tests of the service-switch decisions (CuBit.Service_Replacement),
--  procmgr's switch machine (Service_Switches) and manifest fingerprints
--  (Manifest_Fingerprints). Run: tests/service-switch/run.sh.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Service_Replacement;
with Service_Switches;
with Manifest_Fingerprints;

procedure Switch_Tests is
   package SR renames CuBit.Service_Replacement;
   package SS renames Service_Switches;
   package MF renames Manifest_Fingerprints;
   use type SR.Refusal;
   use type SR.Outcome;
   use type SR.Health_Failure;
   use type SS.Action;
   use type SS.Phase;
   use type MF.Reading;

   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Put_Line ((if Condition then "PASS: " else "FAIL: ") & What);
      if not Condition then
         Failures := Failures + 1;
      end if;
   end Check;

   Good : constant SR.Authority_Facts :=
     (Entry_Known => True, Entry_Replaceable => True, Requester_Stages => True,
      Switch_On => True, Signature => SR.Unsigned, In_Progress => False);

   function Refused (F : SR.Authority_Facts; Why : SR.Refusal) return Boolean is
      D : constant SR.Decision := SR.Authorize (F);
   begin
      return not D.Admitted and then D.Why = Why;
   end Refused;

   Old_Pid : constant := 16#100_0005#;
   New_Pid : constant := 16#200_0006#;
   Back_Pid : constant := 16#300_0007#;
   T0 : constant := 1_000;
   Health : constant := 3_000;
   Grace : constant := 2_000;
   S : SS.Switch;
   Next : SS.Action;
begin
   ---------------------------------------------------------------------------
   --  Admission
   ---------------------------------------------------------------------------
   Check (SR.Authorize (Good).Admitted, "authorize: every fact allows it");
   Check (Refused ((Good with delta Entry_Known => False), SR.Unknown_Service), "authorize: an unknown entry");
   Check (Refused ((Good with delta Entry_Replaceable => False), SR.Not_Replaceable),
          "authorize: an entry whose restart contract is reboot");
   Check (Refused ((Good with delta Requester_Stages => False), SR.Not_Granted),
          "authorize: a requester that does not stage the program");
   Check (Refused ((Good with delta Switch_On => False), SR.Switch_Off),
          "authorize: Allow app replacement off");
   Check (Refused ((Good with delta Signature => SR.Signature_Invalid), SR.Unsigned_Refused),
          "authorize: an invalid signature (AUTH-001 seam)");
   Check (Refused ((Good with delta In_Progress => True), SR.Busy), "authorize: another switch in progress");
   Check (Refused ((Good with delta Switch_On => False, Entry_Replaceable => False), SR.Not_Replaceable),
          "authorize: policy is reported before the switch");
   Check (SR.Content_Admits (True, True, True).Admitted, "content: same manifest");
   Check (not SR.Content_Admits (True, True, False).Admitted
          and then SR.Content_Admits (True, True, False).Why = SR.Manifest_Changed,
          "content: a changed manifest is refused");
   Check (SR.Content_Admits (False, True, True).Why = SR.Image_Unreadable, "content: no image");
   Check (SR.Content_Admits (True, False, False).Why = SR.Program_Unreadable, "content: no program");
   Check (SR.Request_Valid (8, 40, 0, 3_000), "request: valid numbers");
   Check (not SR.Request_Valid (0, 40, 0, 3_000), "request: no service name");
   Check (not SR.Request_Valid (8, 40, 0, 50), "request: a health window under the minimum");
   Check (not SR.Request_Valid (8, 40, 0, 30_001), "request: a health window over the ceiling");
   Check (not SR.Request_Valid (8, 40, 10_001, 3_000), "request: a stop grace over the ceiling");
   Check (SR.Grace_Of (SR.Deadlines_Word (2_000, 30_000)) = 2_000
          and then SR.Health_Window_Of (SR.Deadlines_Word (2_000, 30_000)) = 30_000,
          "wire: both deadlines in one word");
   Check (SR.Refusal_Of (SR.Refusal'Enum_Rep (SR.Manifest_Changed)) = SR.Manifest_Changed,
          "wire: refusal codes round-trip");
   Check (SR.Answer_Bound_Ms (SR.Maximum_Stop_Grace_Ms, SR.Maximum_Health_Window_Ms)
            = SR.Maximum_Answer_Bound_Ms, "wire: the answer bound's ceiling");

   ---------------------------------------------------------------------------
   --  The machine
   ---------------------------------------------------------------------------
   --  Healthy switch, the old instance ended at once (grace 0).
   SS.Begin_Switch (S, Old_Pid, False, 0, Health, T0, Next);
   Check (S.State = SS.Ending_Old and then Next = SS.End_Old, "switch: grace 0 ends the old instance at once");
   SS.Exited (S, 16#999#, True, Next);
   Check (Next = SS.Nothing and then S.State = SS.Ending_Old, "switch: another process's exit is ignored");
   SS.Exited (S, Old_Pid, True, Next);
   Check (S.State = SS.Starting and then Next = SS.Start_Program, "switch: the old instance gone, start");
   SS.Started (S, New_Pid, True, T0 + 10, Next);
   Check (S.State = SS.Checking and then Next = SS.Nothing and then S.Deadline = T0 + 10 + Health,
          "switch: started, the health window is open");
   SS.Tick (S, T0 + 100, True, Next);
   Check (Next = SS.Nothing and then S.State = SS.Checking, "switch: registered early, the window stays open");
   SS.Tick (S, T0 + 10 + Health, True, Next);
   Check (Next = SS.Answer and then S.State = SS.Finished and then S.Result = SR.Switched,
          "switch: registered and alive at the end of the window: switched");
   SS.Answered (S);
   Check (not SS.Active (S), "switch: idle after the answer");

   --  With a grace: Stop first, ended at the grace deadline.
   SS.Begin_Switch (S, Old_Pid, False, Grace, Health, T0, Next);
   Check (S.State = SS.Stopping_Old and then Next = SS.Ask_Stop, "switch: a grace asks Stop first");
   SS.Tick (S, T0 + Grace - 1, False, Next);
   Check (Next = SS.Nothing, "switch: before the grace deadline nothing happens");
   SS.Tick (S, T0 + Grace, False, Next);
   Check (S.State = SS.Ending_Old and then Next = SS.End_Old, "switch: at the grace deadline it is ended");
   SS.Tick (S, T0 + Grace + SR.End_Deadline_Ms, False, Next);
   Check (S.State = SS.Finished and then Next = SS.Answer and then S.Result = SR.Stop_Failed,
          "switch: an instance that never ends: Stop_Failed, nothing started");
   SS.Answered (S);

   --  The program ends inside its window: rolled back.
   SS.Begin_Switch (S, Old_Pid, False, 0, Health, T0, Next);
   SS.Exited (S, Old_Pid, True, Next);
   SS.Started (S, New_Pid, True, T0, Next);
   SS.Exited (S, New_Pid, True, Next);
   Check (S.State = SS.Restoring and then Next = SS.Start_Previous and then S.Failure = SR.Exited_Early,
          "rollback: the program ended inside its window, the one before starts");
   SS.Started (S, Back_Pid, True, T0 + 50, Next);
   Check (S.State = SS.Finished and then Next = SS.Answer and then S.Result = SR.Rolled_Back,
          "rollback: the program before runs again: Rolled_Back");
   SS.Answered (S);

   --  The program never registers: ended at the window's end, rolled back.
   SS.Begin_Switch (S, Old_Pid, False, 0, Health, T0, Next);
   SS.Exited (S, Old_Pid, True, Next);
   SS.Started (S, New_Pid, True, T0, Next);
   SS.Tick (S, T0 + Health, False, Next);
   Check (S.State = SS.Ending_New and then Next = SS.End_New and then S.Failure = SR.Not_Registered,
          "rollback: not registered at the window's end: it is ended");
   SS.Exited (S, New_Pid, True, Next);
   Check (S.State = SS.Restoring and then Next = SS.Start_Previous, "rollback: then the one before starts");
   SS.Started (S, SS.No_Process, False, T0 + Health + 10, Next);
   Check (S.State = SS.Finished and then S.Result = SR.Rollback_Failed,
          "rollback: the one before does not start: Rollback_Failed");
   SS.Answered (S);

   --  A role-less program: alive at the end suffices.
   SS.Begin_Switch (S, SS.No_Process, False, 0, Health, T0, Next);
   Check (S.State = SS.Starting and then Next = SS.Start_Program, "switch: nothing running, start at once");
   SS.Started (S, New_Pid, False, T0, Next);
   SS.Tick (S, T0 + Health, False, Next);
   Check (S.Result = SR.Switched and then Next = SS.Answer, "switch: no role to register: alive is healthy");
   SS.Answered (S);

   --  The program does not start.
   SS.Begin_Switch (S, Old_Pid, False, 0, Health, T0, Next);
   SS.Exited (S, Old_Pid, True, Next);
   SS.Started (S, SS.No_Process, True, T0, Next);
   Check (S.State = SS.Restoring and then Next = SS.Start_Previous and then S.Failure = SR.Start_Refused,
          "rollback: a program that does not start");
   SS.Started (S, Back_Pid, True, T0, Next);
   Check (S.Result = SR.Rolled_Back, "rollback: after a refused start");
   SS.Answered (S);

   --  A stuck new instance.
   SS.Begin_Switch (S, Old_Pid, False, 0, Health, T0, Next);
   SS.Exited (S, Old_Pid, True, Next);
   SS.Started (S, New_Pid, True, T0, Next);
   SS.Tick (S, T0 + Health, False, Next);
   SS.Tick (S, T0 + Health + SR.End_Deadline_Ms, False, Next);
   Check (S.Result = SR.Rollback_Failed and then Next = SS.Answer,
          "rollback: a failed program that does not end: Rollback_Failed, answered");
   SS.Answered (S);

   --  Graceful contract: an instance that quiesces (exits 0) in its grace.
   SS.Begin_Switch (S, Old_Pid, True, Grace, Health, T0, Next);
   Check (S.State = SS.Stopping_Old and then Next = SS.Ask_Stop, "graceful: Stop first");
   SS.Exited (S, Old_Pid, True, Next);
   Check (S.State = SS.Starting and then Next = SS.Start_Program, "graceful: exited cleanly, start");
   S := (others => <>);
   --  One that does not quiesce in time is never ended: it keeps running.
   SS.Begin_Switch (S, Old_Pid, True, Grace, Health, T0, Next);
   SS.Tick (S, T0 + Grace, False, Next);
   Check (S.State = SS.Finished and then Next = SS.Answer and then S.Result = SR.Stop_Failed,
          "graceful: no clean exit in the grace: refused, never ended, nothing starts");
   SS.Answered (S);
   --  One that exits failing: the program before is started, not the new one.
   SS.Begin_Switch (S, Old_Pid, True, Grace, Health, T0, Next);
   SS.Exited (S, Old_Pid, False, Next);
   Check (S.State = SS.Restoring and then Next = SS.Start_Previous and then S.Old_Unclean,
          "graceful: an unclean exit does not start the new program");
   SS.Started (S, Back_Pid, True, T0, Next);
   Check (S.Result = SR.Stop_Failed and then Next = SS.Answer,
          "graceful: the program before runs again: Stop_Failed");
   SS.Answered (S);
   --  A new instance that fails its window is asked to Stop first.
   SS.Begin_Switch (S, Old_Pid, True, Grace, Health, T0, Next);
   SS.Exited (S, Old_Pid, True, Next);
   SS.Started (S, New_Pid, True, T0, Next);
   SS.Tick (S, T0 + Health, False, Next);
   Check (S.State = SS.Stopping_New and then Next = SS.Ask_Stop_New,
          "graceful: a failed new instance is asked to Stop");
   SS.Tick (S, T0 + Health + Grace, False, Next);
   Check (S.State = SS.Ending_New and then Next = SS.End_New, "graceful: ended only after the grace");
   SS.Exited (S, New_Pid, False, Next);
   SS.Started (S, Back_Pid, True, T0, Next);
   Check (S.Result = SR.Rolled_Back, "graceful: rolled back");
   SS.Answered (S);
   --  Transparent: an unclean exit of the old one is fine (it was ended).
   SS.Begin_Switch (S, Old_Pid, False, 0, Health, T0, Next);
   SS.Exited (S, Old_Pid, False, Next);
   Check (S.State = SS.Starting, "transparent: an ended instance makes way");
   S := (others => <>);

   ---------------------------------------------------------------------------
   --  Manifest fingerprints, on a small hand-made ELF64 image: sections
   --  0 (null), 1 .shstrtab, 2 .text, 3 .cubit.id, 4 .cubit.caps.
   ---------------------------------------------------------------------------
   declare
      Names : constant String := ASCII.NUL & ".shstrtab" & ASCII.NUL & ".text" & ASCII.NUL &
        ".cubit.id" & ASCII.NUL & ".cubit.caps" & ASCII.NUL;
      Shoff : constant := 512;
      Size : constant := Shoff + 5 * 64;
      Image : MF.Bytes (1 .. Size) := [others => 0];
      Into : MF.Bytes (1 .. MF.Maximum_Fingerprint_Bytes);
      Length : MF.Fingerprint_Length;
      Result : MF.Reading;

      procedure Put (Offset : Natural; Value : Unsigned_64; Width : Positive) is
      begin
         for K in 0 .. Width - 1 loop
            Image (Offset + K + 1) := Unsigned_8 (Shift_Right (Value, 8 * K) and 16#FF#);
         end loop;
      end Put;
      procedure Section (Index : Natural; Name, Kind, Offset, Bytes : Unsigned_64) is
         Base : constant Natural := Shoff + 64 * Index;
      begin
         Put (Base, Name, 4);
         Put (Base + 4, Kind, 4);
         Put (Base + 24, Offset, 8);
         Put (Base + 32, Bytes, 8);
      end Section;
      First, Second : MF.Bytes (1 .. MF.Maximum_Fingerprint_Bytes);
      First_Length : MF.Fingerprint_Length;
   begin
      Put (40, Shoff, 8);
      Put (58, 64, 2);
      Put (60, 5, 2);
      Put (62, 1, 2);
      for K in Names'Range loop
         Image (256 + K) := Character'Pos (Names (K));
      end loop;
      Section (1, 1, 3, 256, Names'Length);
      Section (2, 11, 1, 300, 16);       --  .text: code, not manifest
      Section (3, 17, 1, 340, 8);        --  .cubit.id
      Section (4, 27, 1, 360, 4);        --  .cubit.caps
      for K in 1 .. 16 loop
         Image (300 + K) := 16#90#;
      end loop;
      for K in 1 .. 8 loop
         Image (340 + K) := Unsigned_8 (K);
      end loop;
      for K in 1 .. 4 loop
         Image (360 + K) := Unsigned_8 (100 + K);
      end loop;
      MF.Fingerprint (Image, First, First_Length, Result);
      Check (Result = MF.Read and then First_Length = (9 + 1 + 8 + 8) + (11 + 1 + 8 + 4),
             "fingerprint: only the .cubit. sections, with names and sizes");
      --  Code changes: the same manifest.
      Image (305) := 16#CC#;
      MF.Fingerprint (Image, Second, Length, Result);
      Check (Result = MF.Read and then MF.Same (First (1 .. First_Length), Second (1 .. Length)),
             "fingerprint: a code change keeps the manifest");
      --  A manifest byte changes: different.
      Image (362) := 0;
      MF.Fingerprint (Image, Second, Length, Result);
      Check (not MF.Same (First (1 .. First_Length), Second (1 .. Length)),
             "fingerprint: a manifest change is seen");
      --  A section outside the image.
      Section (4, 27, 1, Size - 2, 4);
      MF.Fingerprint (Image, Into, Length, Result);
      Check (Result = MF.Section_Outside and then Length = 0, "fingerprint: a section leaving the image");
      --  Not ELF64 section headers.
      Put (58, 40, 2);
      MF.Fingerprint (Image, Into, Length, Result);
      Check (Result = MF.Not_ELF64, "fingerprint: a wrong section header size");
      Check (MF.Same (First (1 .. 0), Second (1 .. 0)), "fingerprint: two empty manifests are the same");
   end;

   if Failures = 0 then
      Put_Line ("SERVICE-SWITCH: PASS");
   else
      Put_Line ("SERVICE-SWITCH: FAIL" & Failures'Image);
      raise Program_Error;
   end if;
end Switch_Tests;
