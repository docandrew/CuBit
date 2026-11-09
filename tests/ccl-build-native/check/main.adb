--  ccl-build on CuBit (docs/ccl-build.md, "On CuBit"; SELF-002): runs the
--  native ccl-build.app on ps2.drv's sources and checks
--    1. an interrupted build: ccl-build is ended by its process identity
--       while it builds (SEND_CONTROL Interrupt, which only a launcher may
--       send without process authority; the libc ends the process at once,
--       as SIGINT's default action does: CuBit.Libc_Process.Note_Event);
--    2. the next build recovers: it completes, its ps2.drv is byte for byte
--       the program ccl-build built on Linux from the same sources, and
--       the store verifies;
--    3. a build after it runs no step: every step is answered by the cache;
--    4. the runtime place (PERF-010): steps read the runtime in place, in
--       its composite tree in the Storehouse, delegated read-only to each
--       step that names it. The gcc driver, started with a step's places,
--       compiles against it; writing into it is refused; and without it
--       delegated, the same compile cannot see the runtime.
--  ccl-build gets the places it needs delegated (decision D2), so the
--  toolchain's programs it starts get them too: the Storehouse
--  (read/write/create), the toolchain and the source checkout (read), and
--  the work place (read/write/create) for the exported products. What it
--  writes to its outlets is shown on the kernel console. Results go to the
--  kernel console, ending with CCL-BUILD-NATIVE: PASS or FAIL.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
with CuBit.Control_Events;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Launching;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Outlet_Rings;
with CuBit.Program_Descriptions;
with CuBit.Stream_Regions;
with CuBit.Streams;
with Startup;

procedure Main is
   Launcher : constant CuBit.Launching.Launcher := CuBit.Launching.Using (Startup.Process_Manager);
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PP renames CuBit.Program_Descriptions;
   use type CuBit.Launching.Launch_Result;
   use type CuBit.Launching.Control_Result;
   use type CuBit.Child_Exits.Termination_Kind;

   Failed : Boolean := False;

   procedure Say (Text : String; Pass : Boolean) is
   begin
      debugPrint ("ccl-build-check: " & Text & (if Pass then " PASS" else " FAIL") & ASCII.LF);
      Failed := Failed or else not Pass;
   end Say;

   procedure Note (Text : String) is
   begin
      debugPrint ("ccl-build-check: " & Text & ASCII.LF);
   end Note;

   --  The places, as ccl-build's manifest names them (absolute POSIX names
   --  are the system volume's).
   Storehouse : constant String := "@system/storehouse";
   Toolchain  : constant String := "@system/toolchain";
   Sources    : constant String := "@system/cubit-src";
   Work       : constant String := "@system/work";
   Program    : constant String := "ccl-build.app";
   LF : constant Character := ASCII.LF;
   --  ccl-build's command line (one argument per line, after argv[0]): ps2
   --  from the checkout, the toolchain recorded in place on first use
   --  (build-params.ccl's CuBit pin), the products exported to the work
   --  place.
   Build_Arguments : constant String :=
     "build" & LF & "--store" & LF & "/storehouse" & LF & "--source-root" & LF & "/cubit-src" & LF &
     "--import" & LF & "gnat-15.3.0-cubit=/toolchain" & LF & "--out" & LF & "/work/out" & LF &
     "/cubit-src/userspace/services/ps2/component.ccl";
   Verify_Arguments : constant String := "verify" & LF & "--store" & LF & "/storehouse";
   Built_Program : constant String := Work & "/out/userspace/services/ps2/build/ps2.drv";
   Expected_Program : constant String := Work & "/expected-ps2.drv";

   --  ccl-build's two text outlets, each a ring lent for the run (revoked
   --  after it) and shown on the kernel console as lines arrive.
   type Outlet_Index is range 1 .. 2;
   Outlet_Names : constant array (Outlet_Index) of access constant String :=
     [new String'("unix.stdout"), new String'("unix.stderr")];
   Ring_Pages : constant := 4;
   Ring_Bases : array (Outlet_Index) of Unsigned_64 := [others => 0];
   Ring_References : array (Outlet_Index) of CuBit.Memory_Grants.Grant_Reference;
   Lent : array (Outlet_Index) of Boolean := [others => False];
   Rings : CuBit.Outlet_Rings.Table;

   --  What the current run reported on standard output.
   Steps_Run : Natural := 0;            --  "ran  <identity>  <label>" lines
   Summary_Seen : Boolean := False;     --  "ccl-build: N ran, M answered by the cache"
   Summary_Ran, Summary_Answered : Natural := 0;
   Verified_Clean : Boolean := False;   --  "..., 0 damaged"

   procedure Reset_Report is
   begin
      Steps_Run := 0;
      Summary_Seen := False;
      Summary_Ran := 0;
      Summary_Answered := 0;
      Verified_Clean := False;
   end Reset_Report;

   function Starts (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length and then Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);
   function Ends (Text, Suffix : String) return Boolean is
     (Text'Length >= Suffix'Length and then Text (Text'Last - Suffix'Length + 1 .. Text'Last) = Suffix);

   --  The decimal number at Text (From ..), and where it ends.
   procedure Number (Text : String; From : Positive; Value : out Natural; Last : out Natural) is
   begin
      Value := 0;
      Last := From - 1;
      while Last < Text'Last and then Text (Last + 1) in '0' .. '9' loop
         Last := Last + 1;
         Value := Value * 10 + (Character'Pos (Text (Last)) - Character'Pos ('0'));
      end loop;
   end Number;

   Ran_Prefix : constant String := "ran  ";
   --  "made  <identity>  ...": a tree ccl-build made for steps to read in
   --  place (the runtime's composite tree, Build_Store.Ensure_Tree).
   Made_Prefix : constant String := "made  ";
   Identity_Digits : constant := 64;
   Runtime_Tree : String (1 .. Identity_Digits) := [others => ' '];
   Runtime_Tree_Seen : Boolean := False;
   Summary_Prefix : constant String := "ccl-build: ";
   Ran_Word : constant String := " ran, ";
   Clean_Suffix : constant String := " 0 damaged";

   procedure Scan (Line : String) is
      Value, Last : Natural;
   begin
      if Starts (Line, Ran_Prefix) then
         Steps_Run := Steps_Run + 1;
      elsif Starts (Line, Made_Prefix)
        and then Line'Length >= Made_Prefix'Length + Identity_Digits
        and then (for all C of Line (Line'First + Made_Prefix'Length ..
                                     Line'First + Made_Prefix'Length + Identity_Digits - 1) =>
                    C in '0' .. '9' | 'a' .. 'f')
      then
         Runtime_Tree := Line (Line'First + Made_Prefix'Length ..
                               Line'First + Made_Prefix'Length + Identity_Digits - 1);
         Runtime_Tree_Seen := True;
      elsif Starts (Line, Summary_Prefix) then
         if Ends (Line, Clean_Suffix) then
            Verified_Clean := True;
         end if;
         Number (Line, Line'First + Summary_Prefix'Length, Value, Last);
         if Last >= Line'First + Summary_Prefix'Length
           and then Starts (Line (Last + 1 .. Line'Last), Ran_Word)
         then
            Summary_Ran := Value;
            Number (Line, Last + 1 + Ran_Word'Length, Summary_Answered, Last);
            Summary_Seen := True;
         end if;
      end if;
   end Scan;

   procedure Lend_Outlets is
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
      for O in Outlet_Index loop
         PP.Find_Connector (S, Outlet_Names (O).all, Index, Found);
         if Found then
            CuBit.Launching.Lend_Ring
              (Launcher, Ring_Pages, CuBit.Streams.TYPE_TEXT_LINE, Ring_Bases (O), Grant,
               Ring_References (O), Ok);
            if Ok then
               Rings.Count := Rings.Count + 1;
               Rings.Entries (Rings.Count) := (Outlet => Index, Grant => Grant);
               Lent (O) := True;
            end if;
         end if;
      end loop;
   end Lend_Outlets;

   Line_Bytes : constant := 512;

   procedure Drain_Outlets is
      Buffer : String (1 .. Line_Bytes);
      Read : Natural;
   begin
      for O in Outlet_Index loop
         if Lent (O) then
            loop
               Read := CuBit.Stream_Regions.Read_Owned
                 (Ring_Bases (O), Ring_Pages, Buffer'Address, Buffer'Length);
               exit when Read = 0;
               if Buffer (Read) = LF then
                  Read := Read - 1;
               end if;
               debugPrint ("ccl-build-check: ccl-build: " & Buffer (1 .. Read) & LF);
               if O = Outlet_Index'First then
                  Scan (Buffer (1 .. Read));
               end if;
            end loop;
         end if;
      end loop;
   end Drain_Outlets;

   procedure Revoke_Outlets is
      Revoked : Boolean;
   begin
      Drain_Outlets;
      for O in Outlet_Index loop
         if Lent (O) then
            CuBit.Memory_Grants.Revoke (Ring_References (O), Revoked);
            Lent (O) := False;
         end if;
      end loop;
      Rings.Count := 0;
   end Revoke_Outlets;

   Microseconds_Per_Millisecond : constant := 1_000;
   Poll_Milliseconds : constant := 1;

   function Now_Milliseconds return Unsigned_64 is
     (CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Read_Monotonic_Microseconds) / Microseconds_Per_Millisecond);

   procedure Sleep_Milliseconds (Milliseconds : Natural) is
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Sleep_Until_Monotonic_Microsecond,
         CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Read_Monotonic_Microseconds)
           + Unsigned_64 (Milliseconds) * Microseconds_Per_Millisecond);
   end Sleep_Milliseconds;

   --  Starts ccl-build with Arguments (one per line) and the four places.
   procedure Start (Arguments : String; Started : out CuBit.Launching.Child; Ok : out Boolean) is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Added, Accepted : Boolean;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      First : Positive := Arguments'First;
   begin
      Reset_Report;
      LA.Start (Block);
      LA.Add_Argument (Block, Program, Accepted);
      for K in Arguments'Range loop
         if Arguments (K) = LF or else K = Arguments'Last then
            LA.Add_Argument (Block, Arguments (First .. (if Arguments (K) = LF then K - 1 else K)), Added);
            Accepted := Accepted and Added;
            First := K + 1;
         end if;
      end loop;
      LA.Finish (Block, Length, Added);
      Accepted := Accepted and Added;
      LG.Start (Places);
      LG.Add (Places, LG.All_Rights, Storehouse, Added);
      Accepted := Accepted and Added;
      LG.Add (Places, LG.Read_Right, Toolchain, Added);
      Accepted := Accepted and Added;
      LG.Add (Places, LG.Read_Right, Sources, Added);
      Accepted := Accepted and Added;
      LG.Add (Places, LG.All_Rights, Work, Added);
      Accepted := Accepted and Added;
      LG.Finish (Places, Region, Region_Length);
      Ok := False;
      if not Accepted then
         return;
      end if;
      Lend_Outlets;
      CuBit.Launching.Launch
        (Launcher, Program, Block.Data (1 .. Length), Region (1 .. Region_Length), Started, Result, Failure,
         Rings);
      if Result /= CuBit.Launching.Launched then
         Revoke_Outlets;
         return;
      end if;
      Ok := True;
   end Start;

   procedure Wait_Draining (Started : CuBit.Launching.Child; Ended : out CuBit.Child_Exits.Report) is
      Has_Ended : Boolean;
   begin
      loop
         CuBit.Launching.Poll_Exit (Started, Has_Ended, Ended);
         exit when Has_Ended;
         Drain_Outlets;
         Sleep_Milliseconds (Poll_Milliseconds);
      end loop;
      Revoke_Outlets;
   end Wait_Draining;

   Not_Exited : constant Integer := -1;

   --  A whole run: ccl-build's exit code, or Not_Exited.
   function Run (Arguments : String) return Integer is
      Started : CuBit.Launching.Child;
      Ended : CuBit.Child_Exits.Report;
      Ok : Boolean;
   begin
      Start (Arguments, Started, Ok);
      if not Ok then
         return Not_Exited;
      end if;
      Wait_Draining (Started, Ended);
      return (if Ended.Kind = CuBit.Child_Exits.Exited then Integer (Ended.Code) else Not_Exited);
   end Run;

   --  binutils-compare (tests/binutils): exit code 0 when two files are equal.
   function Same (A, B : String) return Boolean is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Added, Accepted : Boolean;
      Started : CuBit.Launching.Child;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      Ended : CuBit.Child_Exits.Report;
      Compare : constant String := "binutils-compare.app";
   begin
      LA.Start (Block);
      LA.Add_Argument (Block, Compare, Accepted);
      LA.Add_Argument (Block, A, Added);
      Accepted := Accepted and Added;
      LA.Add_Argument (Block, B, Added);
      Accepted := Accepted and Added;
      LA.Finish (Block, Length, Added);
      LG.Start (Places);
      LG.Finish (Places, Region, Region_Length);
      if not (Accepted and Added) then
         return False;
      end if;
      CuBit.Launching.Launch
        (Launcher, Compare, Block.Data (1 .. Length), Region (1 .. Region_Length), Started, Result, Failure);
      if Result /= CuBit.Launching.Launched then
         return False;
      end if;
      CuBit.Launching.Wait (Started, Ended);
      return Ended.Kind = CuBit.Child_Exits.Exited and then Ended.Code = 0;
   end Same;

   --  4. The gcc driver as a ccl-build step runs (Build_Steps.Places):
   --  a writable directory (here the work place, also its working
   --  directory), the toolchain and the runtime's composite tree read-only,
   --  plus the directory of the unit it compiles; the runtime only when
   --  With_Runtime. It compiles a unit of the input library that withs only
   --  the runtime into Output (named after the unit, as gnat1 requires):
   --  its exit code, or Not_Exited.
   function Runtime_Place return String is (Storehouse & "/trees/" & Runtime_Tree);
   function Runtime_Name return String is ("/storehouse/trees/" & Runtime_Tree);
   Unit_Place : constant String := Sources & "/userspace/lib/input";
   function Probe (Output : String; With_Runtime : Boolean) return Integer is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Added, Accepted : Boolean := True;
      Started : CuBit.Launching.Child;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      Ended : CuBit.Child_Exits.Report;
      Driver : constant String := "toolchain/bin/gcc";
      procedure Argument (Text : String) is
      begin
         LA.Add_Argument (Block, Text, Added);
         Accepted := Accepted and Added;
      end Argument;
      procedure Place (Rights : Unsigned_8; Name : String) is
      begin
         LG.Add (Places, Rights, Name, Added);
         Accepted := Accepted and Added;
      end Place;
   begin
      LA.Start (Block);
      Argument ("/" & Driver);
      Argument ("-c");
      Argument ("-x");
      Argument ("ada");
      Argument ("-gnatA");
      Argument ("--RTS=" & Runtime_Name & "/userspace/runtime");
      Argument ("/cubit-src/userspace/lib/input/input_pending.adb");
      Argument ("-o");
      Argument (Output);
      LA.Add_Directory (Block, Work, Added);
      Accepted := Accepted and Added;
      LA.Finish (Block, Length, Added);
      Accepted := Accepted and Added;
      LG.Start (Places);
      Place (LG.All_Rights, Work);
      Place (LG.Read_Right, Toolchain);
      Place (LG.Read_Right, Unit_Place);
      if With_Runtime then
         Place (LG.Read_Right, Runtime_Place);
      end if;
      LG.Finish (Places, Region, Region_Length);
      if not Accepted then
         return Not_Exited;
      end if;
      CuBit.Launching.Launch
        (Launcher, Driver, Block.Data (1 .. Length), Region (1 .. Region_Length), Started, Result, Failure);
      if Result /= CuBit.Launching.Launched then
         Note ("the probe compile could not be started");
         return Not_Exited;
      end if;
      CuBit.Launching.Wait (Started, Ended);
      return (if Ended.Kind = CuBit.Child_Exits.Exited then Integer (Ended.Code) else Not_Exited);
   end Probe;

   --  The interrupted build: ended once it has run this many steps (of
   --  ps2's eleven), so some steps are answered on recovery and some run.
   Steps_Before_Interrupt : constant := 4;
   --  Bounds, not expected durations (KVM; TCG is about ten times slower).
   Interrupt_Deadline_Milliseconds : constant := 600_000;
   --  ccl-build's steps are processes it started; ending ccl-build does not
   --  end them (no process groups on CuBit, as SIGKILL on Linux leaves its
   --  children). One step is one compile or link: let the one that may run
   --  finish before the next build reuses its step directory's name.
   Orphan_Step_Milliseconds : constant := 60_000;
   --  The libc ends a process its launcher interrupts as SIGINT would, with
   --  exit code 128 + SIGINT (<signal.h>).
   Signal_Exit_Base : constant := 128;
   SIGINT : constant := 2;
   Interrupted_Exit_Code : constant := Signal_Exit_Base + SIGINT;
begin
   --  1. Interrupted.
   declare
      Started : CuBit.Launching.Child;
      Ended : CuBit.Child_Exits.Report;
      Ok, Has_Ended : Boolean := False;
      Sent : CuBit.Launching.Control_Result;
      Deadline : constant Unsigned_64 := Now_Milliseconds + Interrupt_Deadline_Milliseconds;
      Steps_At_Interrupt : Natural := 0;
   begin
      Start (Build_Arguments, Started, Ok);
      Say ("ccl-build starts (first build, from an empty store)", Ok);
      if Ok then
         loop
            CuBit.Launching.Poll_Exit (Started, Has_Ended, Ended);
            exit when Has_Ended or else Steps_Run >= Steps_Before_Interrupt
              or else Now_Milliseconds > Deadline;
            Drain_Outlets;
            Sleep_Milliseconds (Poll_Milliseconds);
         end loop;
         Steps_At_Interrupt := Steps_Run;
         if not Has_Ended then
            CuBit.Launching.Send_Control (Started, CuBit.Control_Events.Interrupt, Sent);
            Note ("interrupted ccl-build after" & Steps_At_Interrupt'Image & " steps ran");
            Wait_Draining (Started, Ended);
         else
            Revoke_Outlets;
         end if;
         Say ("ccl-build is ended while it builds (interrupted by its process identity)",
              not Has_Ended and then Sent = CuBit.Launching.Sent
              and then Ended.Kind = CuBit.Child_Exits.Exited and then Ended.Code = Interrupted_Exit_Code
              and then not Summary_Seen);
         Note ("letting a step it left running finish");
         Sleep_Milliseconds (Orphan_Step_Milliseconds);
      end if;
   end;

   --  2. Recovered: the same build completes, with the same program as
   --  Linux's, and the store verifies.
   Say ("the next build completes (exit code 0)", Run (Build_Arguments) = 0);
   Say ("it reports its steps", Summary_Seen and then Summary_Ran + Summary_Answered > 0);
   Note ("recovery: " & Summary_Ran'Image & " ran," & Summary_Answered'Image & " answered");
   Say ("ps2.drv is byte for byte the Linux build's", Same (Built_Program, Expected_Program));
   Say ("ccl-build verify finds no damage", Run (Verify_Arguments) = 0 and then Verified_Clean);

   --  3. Unchanged: every step answered.
   Say ("a build after it completes (exit code 0)", Run (Build_Arguments) = 0);
   Say ("it runs no step: every step is answered by the cache",
        Summary_Seen and then Summary_Ran = 0 and then Steps_Run = 0 and then Summary_Answered > 0);
   Say ("its ps2.drv is still the Linux build's", Same (Built_Program, Expected_Program));

   --  4. The runtime place.
   Say ("ccl-build made the runtime's tree, for its steps to read in place", Runtime_Tree_Seen);
   Note ("runtime place " & Runtime_Place);
   Say ("a step given the runtime place compiles against it (control)",
        Probe ("/work/input_pending.o", With_Runtime => True) = 0);
   Say ("a step writing into the runtime place is refused",
        Probe (Runtime_Name & "/userspace/runtime/input_pending.o", With_Runtime => True)
          not in 0 | Not_Exited);
   Say ("a step not given the runtime place cannot see it",
        Probe ("/work/input_pending.o", With_Runtime => False) not in 0 | Not_Exited);
   debugPrint ((if Failed then "CCL-BUILD-NATIVE: FAIL" else "CCL-BUILD-NATIVE: PASS") & LF);
end Main;
