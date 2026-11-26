--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  test262 on CuBit (WEB-006 phase a): Ladybird's test262-runner, built
--  unmodified as a Linux/musl program (ladybird-test262.app), over the
--  tests listed in @nvme:0/test262/list.txt, in batches of 250 as
--  libjs-test262's driver runs it on Linux. The runner reads a batch's
--  paths from its standard input, which the port's compatibility layer
--  gives from the list file (LADYBIRD_STDIN_FILE, _FIRST, _COUNT in the
--  launch block's environment), and writes one RESULT line per test to its
--  unix.stdout outlet, read here through a lent ring. As the Linux driver
--  does, a runner that ends early is started again after its last result;
--  the test it was running counts as a process error unless its own result
--  said why it stopped. Every result that is not a pass goes to the kernel
--  console with its test, then the totals.
pragma Ada_2022;
with CuBit.Console_Route;
with Wait_ABI;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
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

procedure Main is
   Launcher : constant CuBit.Launching.Launcher := CuBit.Launching.Using (Startup.Process_Manager);
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PP renames CuBit.Program_Descriptions;
   use type CuBit.Launching.Launch_Result;
   use type CuBit.Child_Exits.Termination_Kind;

   Program  : constant String := "ladybird-test262.app";
   Suite    : constant String := "@nvme:0/test262";
   Data     : constant String := "@nvme:0/ladybird";
   Batch    : constant := 250;
   --  libjs-test262's default: a test that runs longer is stopped. CuBit's
   --  libc delivers no signals, so the runner's own alarm never fires; the
   --  limit is passed as on Linux and recorded here.
   Timeout_Seconds : constant String := "10";

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

   --  One lent ring for the runner's unix.stdout; its stderr is not lent.
   Ring_Pages : constant := 16;
   Rings : CuBit.Outlet_Rings.Table;
   Ring_Base : Unsigned_64 := 0;
   Ring_Reference : CuBit.Memory_Grants.Grant_Reference;

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
   end Lend;

   Line : String (1 .. Ring_Pages * 4096);

   procedure Drain is
      Read : Natural;
   begin
      if Rings.Count = 0 then
         return;
      end if;
      loop
         Read := CuBit.Stream_Regions.Read_Owned (Ring_Base, Ring_Pages, Line'Address, Line'Length);
         exit when Read = 0;
         if Read > 7 and then Line (1 .. 7) = "RESULT " then
            Count (Line (1 .. Read));
         elsif Read > 5 and then Line (1 .. 5) = "DONE " then
            Done_Seen := True;
            Done_Count := 0;
            for K in 6 .. Read loop
               exit when Line (K) not in '0' .. '9' or else Done_Count > Natural'Last / 10 - 9;
               Done_Count := Done_Count * 10 + (Character'Pos (Line (K)) - Character'Pos ('0'));
            end loop;
         end if;
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
      Environment ("LADYBIRD_STDIN_FILE=" & Suite & "/list.txt");
      Environment ("LADYBIRD_STDIN_FIRST=" & Image (First));
      Environment ("LADYBIRD_STDIN_COUNT=" & Image (Batch));
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
      CuBit.Launching.Launch
        (Launcher, Program, Block.Data (1 .. Length), Region (1 .. Region_Length), Child, Result, Failure,
         Rings);
      if Result /= CuBit.Launching.Launched then
         Rings.Count := 0;
         return Not_Started;
      end if;
      loop
         CuBit.Launching.Poll_Exit (Child, Has_Ended, Ended);
         exit when Has_Ended;
         Drain;
         Sleep_Milliseconds (1);
      end loop;
      Drain;
      if Rings.Count > 0 then
         CuBit.Memory_Grants.Revoke (Ring_Reference, Revoked);
         Rings.Count := 0;
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
   loop
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
      if Code = 0 and then Done_Seen then
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
      if Next mod 5_000 < Batch then
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
