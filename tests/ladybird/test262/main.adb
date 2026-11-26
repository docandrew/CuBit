--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  test262 on CuBit (WEB-006 phase a): Ladybird's test262-runner, built
--  unmodified as a Linux/musl program (ladybird-test262.app), over the
--  tests in the staged list (linked in: list.S, the stage's list.txt), in
--  batches of 250 as libjs-test262's driver runs it on Linux. The runner
--  reads a batch's paths from its standard input, the unix.stdin inlet:
--  a ring lent here and fed a line at a time as it reads
--  (CuBit.Inlet_Rings), then finished; it writes one RESULT line per test
--  to its unix.stdout outlet, read here through another lent ring. As the Linux driver
--  does, a runner that ends early is started again after its last result;
--  the test it was running counts as a process error unless its own result
--  said why it stopped. Every result that is not a pass goes to the kernel
--  console with its test, then the totals.
pragma Ada_2022;
with CuBit.Console_Route;
with Wait_ABI;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
with CuBit.Inlet_Rings;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Launching;
with Startup;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Outlet_Rings;
with CuBit.Program_Descriptions;
with CuBit.Stream_Regions;
with CuBit.Streams;
with CuBit.Monotonic;
with CuBit.Process_Events;
with System.Storage_Elements;

procedure Main is
   Launcher : constant CuBit.Launching.Launcher := CuBit.Launching.Using (Startup.Process_Manager);
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PP renames CuBit.Program_Descriptions;
   use type CuBit.Launching.Launch_Result;
   use type CuBit.Child_Exits.Termination_Kind;
   use type CuBit.Inlet_Rings.Put_Result;

   Program  : constant String := "ladybird-test262.app";
   Suite    : constant String := "@nvme:0/test262";
   Data     : constant String := "@nvme:0/ladybird";
   Batch    : constant := 250;
   --  libjs-test262's default: a test that runs longer is stopped. CuBit's
   --  libc delivers no signals, so the runner's own alarm never fires; the
   --  limit is passed as on Linux and recorded here.
   Timeout_Seconds : constant String := "10";
   --  So the limit is kept here: a runner that reports no result for this
   --  long is ended and its running test counted as a timeout, as the Linux
   --  driver counts its SIGALRM. Six times Linux's 10 s, for the guest.
   Stall_Microseconds : constant := 60_000_000;
   Last_Result : Unsigned_64 := 0;

   --  The results test262-runner reports, and the two this driver adds.
   type Outcome is (Passed, Failed, Skipped, Metadata_Error, Harness_Error,
                    Timeout_Error, Process_Error, Todo_Error);
   function Name (O : Outcome) return String is
     (case O is
        when Passed => "passed", when Failed => "failed", when Skipped => "skipped",
        when Metadata_Error => "metadata_error", when Harness_Error => "harness_error",
        when Timeout_Error => "timeout", when Process_Error => "assert_fail",
        when Todo_Error => "todo_error");
   Totals : array (Outcome) of Natural := [others => 0];

   --  Lent rings for the runner's unix.stdout (its results), unix.stderr
   --  (shown, up to Stderr_Shown_Limit lines in all) and unix.stdin (the
   --  batch's paths), each of the pages ladybird-test262.app's manifest
   --  declares.
   Ring_Pages : constant := 128;
   Stderr_Pages : constant := 4;
   Inlet_Pages : constant := 16;
   Inlet_Base : Unsigned_64 := 0;
   Inlet_Reference : CuBit.Memory_Grants.Grant_Reference;

   --  The list of tests, one path per line (list.S).
   List_Start : constant Character with Import, External_Name => "ladybird_test262_list_start";
   List_End : constant Character with Import, External_Name => "ladybird_test262_list_end";
   List_Bytes : constant Natural :=
     Natural (System.Storage_Elements."-" (List_End'Address, List_Start'Address));
   List : constant String (1 .. List_Bytes) with Import, Address => List_Start'Address;
   --  Where each line starts (Starts (Lines + 1) is just past the list).
   Maximum_Lines : constant := 100_000;
   Starts : array (1 .. Maximum_Lines + 1) of Positive := [others => 1];
   Lines : Natural := 0;
   Rings : CuBit.Outlet_Rings.Table;
   Ring_Base : Unsigned_64 := 0;
   Ring_Reference : CuBit.Memory_Grants.Grant_Reference;
   Stderr_Base : Unsigned_64 := 0;
   Stderr_Reference : CuBit.Memory_Grants.Grant_Reference;
   Stderr_Lent : Boolean := False;
   Stderr_Shown : Natural := 0;
   Stderr_Shown_Limit : constant := 200;

   --  This batch: results seen, and whether the last one stopped the runner.
   Batch_Results : Natural := 0;
   Stopping_Result : Boolean := False;
   Done_Seen : Boolean := False;
   Done_Count : Natural := 0;

   procedure Show (Text : String) is
   begin
      debugPrint ("ladybird-test262: " & Text & ASCII.LF);
   end Show;

   function Image (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   --  The string value of "Key":"..." in Line, or "".
   function Field (Line, Key : String) return String is
      Pattern : constant String := '"' & Key & """:""";
   begin
      for First in Line'First .. Line'Last - Pattern'Length + 1 loop
         if Line (First .. First + Pattern'Length - 1) = Pattern then
            for Last in First + Pattern'Length .. Line'Last loop
               if Line (Last) = '"' and then Line (Last - 1) /= '\' then
                  return Line (First + Pattern'Length .. Last - 1);
               end if;
            end loop;
            return "";
         end if;
      end loop;
      return "";
   end Field;

   procedure Count (Line : String) is
      Value : constant String := Field (Line, "result");
      Test : constant String := Field (Line, "test");
      Found : Boolean := False;
   begin
      for O in Outcome loop
         if Value = Name (O) then
            Totals (O) := Totals (O) + 1;
            Found := True;
            Stopping_Result := O in Timeout_Error | Process_Error;
            if O /= Passed then
               Show (Value & " " & Test);
            end if;
         end if;
      end loop;
      if not Found then
         Totals (Failed) := Totals (Failed) + 1;
         Show ("failed " & Test & " (result " & Value & ")");
      end if;
      Batch_Results := Batch_Results + 1;
      Last_Result := CuBit.Monotonic.Microseconds;
   end Count;

   procedure Lend is
      Descriptor : PP.Bytes (1 .. PP.Maximum_Descriptor_Bytes);
      Length : PP.Descriptor_Length;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      S : PP.Signature;
      Index : PP.Connector_Index;
      Ok, Found : Boolean;
      Grant : Unsigned_64;
   begin
      Rings.Count := 0;
      CuBit.Launching.Describe (Launcher, Program, Descriptor, Length, Result, Failure);
      if Result /= CuBit.Launching.Launched or else Length = 0 then
         return;
      end if;
      PP.Decode (Descriptor (1 .. Length), S, Ok);
      if not Ok then
         return;
      end if;
      PP.Find_Connector (S, "unix.stdout", Index, Found);
      if not Found then
         return;
      end if;
      CuBit.Launching.Lend_Ring
        (Launcher, Ring_Pages, CuBit.Streams.TYPE_TEXT_LINE, Ring_Base, Grant, Ring_Reference, Ok);
      if Ok then
         Rings.Count := 1;
         Rings.Entries (1) := (Outlet => Index, Grant => Grant);
      end if;
      PP.Find_Connector (S, "unix.stdin", Index, Found);
      if not Found or else Rings.Count = 0 then
         Rings.Count := 0;
         return;
      end if;
      CuBit.Launching.Lend_Ring
        (Launcher, Inlet_Pages, CuBit.Streams.TYPE_TEXT_LINE, Inlet_Base, Grant, Inlet_Reference, Ok);
      if not Ok then
         Rings.Count := 0;
         return;
      end if;
      Rings.Count := 2;
      Rings.Entries (2) := (Outlet => Index, Grant => Grant);
      Stderr_Lent := False;
      PP.Find_Connector (S, "unix.stderr", Index, Found);
      if Found then
         CuBit.Launching.Lend_Ring
           (Launcher, Stderr_Pages, CuBit.Streams.TYPE_TEXT_LINE, Stderr_Base, Grant, Stderr_Reference, Ok);
         if Ok then
            Rings.Count := 3;
            Rings.Entries (3) := (Outlet => Index, Grant => Grant);
            Stderr_Lent := True;
         end if;
      end if;
   end Lend;

   Line : String (1 .. Ring_Pages * 4096);

   --  One line of the runner's output.
   procedure Take (Text : String) is
   begin
      if Text'Length > 7 and then Text (Text'First .. Text'First + 6) = "RESULT " then
         Count (Text);
      elsif Text'Length > 5 and then Text (Text'First .. Text'First + 4) = "DONE " then
         Done_Seen := True;
         Done_Count := 0;
         for K in Text'First + 5 .. Text'Last loop
            exit when Text (K) not in '0' .. '9' or else Done_Count > Natural'Last / 10 - 9;
            Done_Count := Done_Count * 10 + (Character'Pos (Text (K)) - Character'Pos ('0'));
         end loop;
      end if;
   end Take;

   --  A record may hold several lines (one write): each is taken alone.
   procedure Drain is
      Read : Natural;
      First : Positive;
   begin
      if Rings.Count = 0 then
         return;
      end if;
      if Stderr_Lent then
         loop
            Read := CuBit.Stream_Regions.Read_Owned (Stderr_Base, Stderr_Pages, Line'Address, Line'Length);
            exit when Read = 0;
            if Stderr_Shown < Stderr_Shown_Limit then
               Stderr_Shown := Stderr_Shown + 1;
               Show ("runner stderr: " & Line (1 .. Read));
            end if;
         end loop;
      end if;
      loop
         Read := CuBit.Stream_Regions.Read_Owned (Ring_Base, Ring_Pages, Line'Address, Line'Length);
         exit when Read = 0;
         First := 1;
         for Last in 1 .. Read loop
            if Line (Last) = ASCII.LF or else Last = Read then
               declare
                  Ends : constant Natural := (if Line (Last) = ASCII.LF then Last - 1 else Last);
               begin
                  if Ends >= First then
                     Take (Line (First .. Ends));
                  end if;
               end;
               First := Last + 1;
            end if;
         end loop;
      end loop;
   end Drain;

   procedure Sleep_Milliseconds (Milliseconds : Natural) is
      Microseconds_Per_Millisecond : constant := 1_000;
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Wait, Wait_ABI.Timer_Sources, Wait_ABI.Forever,
         CuBit.Monotonic.Microseconds
           + Unsigned_64 (Milliseconds) * Microseconds_Per_Millisecond);
   end Sleep_Milliseconds;

   Not_Started : constant Integer := -2;
   Stalled     : constant Integer := -3;
   Not_Exited  : constant Integer := -1;

   --  The runner over lines First .. First + Batch - 1 of the list: its
   --  exit code, Not_Exited if stopped or killed, Not_Started.
   function Run (First : Natural) return Integer is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Added, Accepted : Boolean;
      Child : CuBit.Launching.Child;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Ended : CuBit.Child_Exits.Report;
      Has_Ended : Boolean;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      Revoked : Boolean;
      Ended_By_Us : Boolean := False;
      End_Result : CuBit.Launching.Launch_Result;
      --  The lines given so far, as list indices (from 0), and the end.
      Fed : Natural := First;
      Last : constant Natural := Natural'Min (Lines, First + Batch);
      procedure Argument (Text : String) is
      begin
         LA.Add_Argument (Block, Text, Added);
         Accepted := Accepted and Added;
      end Argument;
      procedure Environment (Text : String) is
      begin
         LA.Add_Environment (Block, Text, Added);
         Accepted := Accepted and Added;
      end Environment;
   begin
      Accepted := True;
      LA.Start (Block);
      Argument ("test262-runner");
      Argument ("--harness-location");
      Argument (Suite & "/harness");
      Argument ("-t");
      Argument (Timeout_Seconds);
      Environment ("LADYBIRD_ICU_DATA=" & Data & "/icudt78l.dat");
      LA.Finish (Block, Length, Added);
      Accepted := Accepted and Added;
      LG.Start (Places);
      LG.Add (Places, LG.Read_Right, Suite, Added);
      Accepted := Accepted and Added;
      LG.Add (Places, LG.Read_Right, Data, Added);
      Accepted := Accepted and Added;
      LG.Finish (Places, Region, Region_Length);
      if not Accepted then
         return Not_Started;
      end if;
      Lend;
      Last_Result := CuBit.Monotonic.Microseconds;
      CuBit.Launching.Launch
        (Launcher, Program, Block.Data (1 .. Length), Region (1 .. Region_Length), Child, Result, Failure,
         Rings);
      if Result /= CuBit.Launching.Launched then
         Rings.Count := 0;
         return Not_Started;
      end if;
      loop
         --  The batch's paths, a line per record, as the runner frees room.
         while Fed < Last loop
            declare
               Path : constant String := List (Starts (Fed + 1) .. Starts (Fed + 2) - 1);
            begin
               exit when CuBit.Inlet_Rings.Write (Inlet_Base, Inlet_Pages, Path'Address, Path'Length)
                 /= CuBit.Inlet_Rings.Put;
               Fed := Fed + 1;
               if Fed = Last then
                  CuBit.Inlet_Rings.Finish (Inlet_Base);
               end if;
            end;
         end loop;
         --  The rings' grant events (lent with notify) are read and let go
         --  each turn: CuBit.Process_Events keeps at most Kept_Events unread,
         --  and an exit queued behind them would never be taken (the run
         --  stopped after 12 batches of 3 rings).
         CuBit.Process_Events.Serve_Defaults;
         CuBit.Launching.Poll_Exit (Child, Has_Ended, Ended);
         exit when Has_Ended;
         Drain;
         if not Ended_By_Us and then CuBit.Monotonic.Microseconds - Last_Result > Stall_Microseconds then
            CuBit.Launching.End_Child (Child, End_Result);
            Ended_By_Us := True;
         end if;
         Sleep_Milliseconds (1);
      end loop;
      Drain;
      if Rings.Count > 0 then
         CuBit.Memory_Grants.Revoke (Inlet_Reference, Revoked);
         CuBit.Memory_Grants.Revoke (Ring_Reference, Revoked);
         if Stderr_Lent then
            CuBit.Memory_Grants.Revoke (Stderr_Reference, Revoked);
            Stderr_Lent := False;
         end if;
         Rings.Count := 0;
      end if;
      if Ended_By_Us then
         return Stalled;
      end if;
      return (if Ended.Kind = CuBit.Child_Exits.Exited then Integer (Ended.Code) else Not_Exited);
   end Run;

   Next : Natural := 0;
   Code : Integer;
   Started : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   Total : Natural := 0;
   Launch_Failed : Boolean := False;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   --  Index the list's lines (each line includes its newline).
   for K in List'Range loop
      exit when Lines = Maximum_Lines;
      if List (K) = ASCII.LF then
         Lines := Lines + 1;
         Starts (Lines + 1) := K + 1;
      end if;
   end loop;
   Show ("tests listed" & Natural'Image (Lines));
   loop
      exit when Next >= Lines;
      Batch_Results := 0;
      Stopping_Result := False;
      Done_Seen := False;
      Done_Count := 0;
      Code := Run (Next);
      if Code = Not_Started then
         Show ("the runner could not be started at test " & Image (Next));
         Launch_Failed := True;
         exit;
      end if;
      Next := Next + Batch_Results;
      if Code = Stalled then
         --  The test it was running took too long: a timeout, as on Linux.
         Totals (Timeout_Error) := Totals (Timeout_Error) + 1;
         if Next < Lines then
            Show ("timeout " & List (Starts (Next + 1) .. Starts (Next + 2) - 2));
         end if;
         Next := Next + 1;
      elsif Code = 0 and then Done_Seen then
         --  The list ends inside this batch.
         exit when Done_Count < Batch;
      else
         --  Ended early: the test it was running failed it, unless its
         --  own result (timeout, assert_fail) already said so.
         if not Stopping_Result then
            Totals (Process_Error) := Totals (Process_Error) + 1;
            Show ("process_error at list line " & Image (Next) & " (exit" & Integer'Image (Code) & ")");
            Next := Next + 1;
         end if;
      end if;
      if Next / 5_000 /= (Next - Natural'Min (Next, Batch_Results + 1)) / 5_000 then
         Show ("progress " & Image (Next));
      end if;
   end loop;
   for O in Outcome loop
      Total := Total + Totals (O);
   end loop;
   Show ("tests" & Natural'Image (Total) & ", passed" & Natural'Image (Totals (Passed)) &
         ", failed" & Natural'Image (Totals (Failed)) & ", skipped" & Natural'Image (Totals (Skipped)) &
         ", metadata" & Natural'Image (Totals (Metadata_Error)) &
         ", harness" & Natural'Image (Totals (Harness_Error)) &
         ", timeout" & Natural'Image (Totals (Timeout_Error)) &
         ", process" & Natural'Image (Totals (Process_Error)) &
         ", todo" & Natural'Image (Totals (Todo_Error)) &
         ", seconds" & Unsigned_64'Image ((CuBit.Monotonic.Microseconds - Started) / 1_000_000));
   debugPrint ((if Launch_Failed or else Total = 0 then "LADYBIRD-TEST262: FAIL" else "LADYBIRD-TEST262: DONE")
               & ASCII.LF);
end Main;
