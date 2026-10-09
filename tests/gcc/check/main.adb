--  GCC on CuBit (docs/self-hosting.md, "GCC and GNAT"). Stage 2: cc1
--  compiles hello.c to assembly and as.app assembles it. Stage 3: the gcc
--  driver does both itself (gcc -c), starting cc1 and as through the libc's
--  posix_spawn, then builds the whole program (cc1, as, ld), which runs. binutils-compare checks every output against the same GCC
--  15.3.0 and binutils on Linux. cc1 and the driver take a raw argv and
--  work only in the places delegated to them here: the work place and the
--  toolchain (decision D2). Results go to the kernel console.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
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

procedure Main is
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PP renames CuBit.Program_Descriptions;
   use type CuBit.Launching.Launch_Result;
   use type PP.Check_Result;
   use type CuBit.Child_Exits.Termination_Kind;

   Failed : Boolean := False;

   procedure Say (Text : String; Pass : Boolean) is
   begin
      debugPrint ("gcc-check: " & Text & (if Pass then " PASS" else " FAIL") & ASCII.LF);
      Failed := Failed or else not Pass;
   end Say;

   --  The places cc1 and the driver work in: the work place, and the
   --  toolchain read-only.
   Work      : constant String := "@nvme:0/work";
   Toolchain : constant String := "@nvme:0/toolchain";

   --  What cc1 and the driver write to their unix.stderr outlet: a ring
   --  lent for each run (Lend_Stderr), shown on the kernel console after it
   --  (Show_Stderr). The pages are reused; the grant is revoked each time.
   Stderr_Rings : CuBit.Outlet_Rings.Table;
   Not_Exited : constant Integer := -1;
   Stderr_Base : Unsigned_64 := 0;
   Stderr_Reference : CuBit.Memory_Grants.Grant_Reference;
   Stderr_Pages : constant := 4;
   --  Records read from the lent rings so far (each step checks it grew).
   Stderr_Records : Natural := 0;

   procedure Lend_Stderr (Program : String) is
      Descriptor : PP.Bytes (1 .. PP.Maximum_Descriptor_Bytes);
      Length : PP.Descriptor_Length;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      S : PP.Signature;
      Index : PP.Connector_Index;
      Ok, Found : Boolean;
      Grant : Unsigned_64;
   begin
      Stderr_Rings.Count := 0;
      CuBit.Launching.Describe (Program, Descriptor, Length, Result, Failure);
      if Result /= CuBit.Launching.Launched or else Length = 0 then
         return;
      end if;
      PP.Decode (Descriptor (1 .. Length), S, Ok);
      if not Ok then
         return;
      end if;
      PP.Find_Connector (S, "unix.stderr", Index, Found);
      if not Found then
         return;
      end if;
      CuBit.Launching.Lend_Ring
        (Stderr_Pages, CuBit.Streams.TYPE_TEXT_LINE, Stderr_Base, Grant,
         Stderr_Reference, Ok);
      if Ok then
         Stderr_Rings.Count := 1;
         Stderr_Rings.Entries (1) := (Outlet => Index, Grant => Grant);
      end if;
   end Lend_Stderr;

   --  Show what is in the ring now (the run goes on: a full ring would
   --  block the program writing to it).
   procedure Drain_Stderr (Program : String) is
      Buffer : String (1 .. 512);
      Read : Natural;
   begin
      if Stderr_Rings.Count = 0 then
         return;
      end if;
      loop
         Read := CuBit.Stream_Regions.Read_Owned
           (Stderr_Base, Stderr_Pages, Buffer'Address, Buffer'Length);
         exit when Read = 0;
         Stderr_Records := Stderr_Records + 1;
         debugPrint ("gcc-check: " & Program & " stderr: " & Buffer (1 .. Read) & ASCII.LF);
      end loop;
   end Drain_Stderr;

   --  The rest, once the run is over; the ring's grant is revoked.
   procedure Show_Stderr (Program : String) is
      Revoked : Boolean;
   begin
      if Stderr_Rings.Count = 0 then
         return;
      end if;
      Drain_Stderr (Program);
      CuBit.Memory_Grants.Revoke (Stderr_Reference, Revoked);
      Stderr_Rings.Count := 0;
   end Show_Stderr;

   Poll_Milliseconds : constant := 1;

   procedure Sleep_Milliseconds (Milliseconds : Natural) is
      Microseconds_Per_Millisecond : constant := 1_000;
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Sleep_Until_Monotonic_Microsecond,
         CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Read_Monotonic_Microseconds)
           + Unsigned_64 (Milliseconds) * Microseconds_Per_Millisecond);
   end Sleep_Milliseconds;

   --  Wait for Started, draining its stderr meanwhile.
   procedure Wait_Draining (Program : String; Started : CuBit.Launching.Child;
                            Ended : out CuBit.Child_Exits.Report) is
      Has_Ended : Boolean;
   begin
      loop
         CuBit.Launching.Poll_Exit (Started, Has_Ended, Ended);
         exit when Has_Ended;
         Drain_Stderr (Program);
         Sleep_Milliseconds (Poll_Milliseconds);
      end loop;
   end Wait_Draining;

   --  Launch Program with raw arguments (Argv: one per line, after argv[0]
   --  = Program, or Name when given), the Environment (one per line) and
   --  the working Directory (none when empty): its exit code, or
   --  Not_Exited when it was not started or was stopped. Delegate: the work
   --  and toolchain places, else none.
   function Run_Code (Program : String; Argv : String; Delegate : Boolean := False;
                      Name : String := ""; Environment : String := "";
                      Directory : String := "") return Integer is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Accepted : Boolean;
      Added : Boolean;
      Child : CuBit.Launching.Child;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Ended : CuBit.Child_Exits.Report;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;

      --  Add each line of Lines, as arguments or environment strings.
      procedure Add_Lines (Lines : String; As_Environment : Boolean) is
         First : Positive := Lines'First;
      begin
         for K in Lines'Range loop
            if Lines (K) = ASCII.LF or else K = Lines'Last then
               declare
                  Line : constant String :=
                    Lines (First .. (if Lines (K) = ASCII.LF then K - 1 else K));
               begin
                  if As_Environment then
                     LA.Add_Environment (Block, Line, Added);
                  else
                     LA.Add_Argument (Block, Line, Added);
                  end if;
               end;
               Accepted := Accepted and Added;
               First := K + 1;
            end if;
         end loop;
      end Add_Lines;
   begin
      LA.Start (Block);
      LA.Add_Argument (Block, (if Name = "" then Program else Name), Added);
      Accepted := Added;
      Add_Lines (Argv, As_Environment => False);
      Add_Lines (Environment, As_Environment => True);
      if Directory /= "" then
         LA.Add_Directory (Block, Directory, Added);
         Accepted := Accepted and Added;
      end if;
      LA.Finish (Block, Length, Added);
      LG.Start (Places);
      if Delegate then
         LG.Add (Places, LG.All_Rights, Work, Added);
         Accepted := Accepted and Added;
         LG.Add (Places, LG.Read_Right, Toolchain, Added);
         Accepted := Accepted and Added;
      end if;
      LG.Finish (Places, Region, Region_Length);
      if not (Accepted and Added) then
         return Not_Exited;
      end if;
      if Delegate then
         Lend_Stderr (Program);
      end if;
      CuBit.Launching.Launch
        (Program, Block.Data (1 .. Length), Region (1 .. Region_Length), Child, Result, Failure,
         Stderr_Rings);
      if Result /= CuBit.Launching.Launched then
         Stderr_Rings.Count := 0;
         return Not_Exited;
      end if;
      Wait_Draining (Program, Child, Ended);
      Show_Stderr (Program);
      return (if Ended.Kind = CuBit.Child_Exits.Exited then Integer (Ended.Code) else Not_Exited);
   end Run_Code;

   function Run_Raw (Program : String; Argv : String; Delegate : Boolean := False;
                     Name : String := ""; Environment : String := "";
                     Directory : String := "") return Boolean is
     (Run_Code (Program, Argv, Delegate, Name, Environment, Directory) = 0);

   function Same (A, B : String) return Boolean is
     (Run_Raw ("binutils-compare.app", A & ASCII.LF & B));

   --  as.app with its typed parameters, delegating the two files.
   function Assemble (Source, Output : String) return Boolean is
      Descriptor : PP.Bytes (1 .. PP.Maximum_Descriptor_Bytes);
      Length : PP.Descriptor_Length;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      S : PP.Signature;
      V : PP.Values;
      Ok : Boolean;
      Block : LA.Builder;
      Grants : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      Checked : PP.Check_Result;
      Child : CuBit.Launching.Child;
      Ended : CuBit.Child_Exits.Report;
      procedure Set (Name, Value : String) is
         Index : PP.Parameter_Index;
         Found, Added : Boolean;
      begin
         PP.Find (S, Name, Index, Found);
         if Found then PP.Add (V, Index, Value, Added); end if;
         Ok := Ok and then Found and then Added;
      end Set;
   begin
      CuBit.Launching.Describe ("as.app", Descriptor, Length, Result, Failure);
      Ok := Result = CuBit.Launching.Launched and then Length > 0;
      if not Ok then return False; end if;
      PP.Decode (Descriptor (1 .. Length), S, Ok);
      if not Ok then return False; end if;
      PP.Clear (V);
      Set ("output", Output);
      Set ("source", Source);
      if not Ok then return False; end if;
      PP.Render (S, V, "as.app", Block, Grants, Checked);
      if Checked /= PP.Matches then return False; end if;
      LG.Finish (Grants, Region, Region_Length);
      CuBit.Launching.Launch
        ("as.app", Block.Data (1 .. Block.Used), Region (1 .. Region_Length), Child, Result, Failure);
      if Result /= CuBit.Launching.Launched then return False; end if;
      CuBit.Launching.Wait (Child, Ended);
      return Ended.Kind = CuBit.Child_Exits.Exited and then Ended.Code = 0;
   end Assemble;
   Cc1    : constant String := "toolchain/libexec/gcc/x86_64-linux-musl/15.3.0/cc1";
   Driver : constant String := "toolchain/bin/gcc";
begin
   --  The same options and names the Linux reference uses (tests/gcc/build.sh).
   --  Relative names, from the work place: GNU tools read an argument that
   --  starts with '@' as a response file, so a CuBit name in argv
   --  ("@nvme:0/...") is first tried as one.
   Say ("cc1 compiles hello.c",
        Run_Raw (Cc1,
                 "-quiet" & ASCII.LF & "-O2" & ASCII.LF & "-fno-pie" & ASCII.LF &
                 "hello.c" & ASCII.LF & "-o" & ASCII.LF & "hello.s",
                 Delegate => True, Directory => Work));
   Say ("cc1 output matches Linux", Same (Work & "/hello.s", Work & "/expected-hello.s"));
   Say ("as assembles it", Assemble (Work & "/hello.s", Work & "/hello.o"));
   Say ("as output matches Linux", Same (Work & "/hello.o", Work & "/expected-hello.o"));
   --  Stage 3: gcc -c in the work place, temporary files there too. The
   --  driver finds its programs from argv[0], as installed.
   Say ("gcc -c compiles and assembles hello.c",
        Run_Raw (Driver,
                 "-O2" & ASCII.LF & "-fno-pie" & ASCII.LF & "-c" & ASCII.LF &
                 "hello.c" & ASCII.LF & "-o" & ASCII.LF & "hello-driver.o",
                 Delegate => True, Name => "/toolchain/bin/gcc",
                 Environment => "TMPDIR=/work", Directory => Work));
   Say ("gcc -c output matches Linux",
        Same (Work & "/hello-driver.o", Work & "/expected-hello-driver.o"));
   --  Diagnostic: the libc archive ld reads, walked three ways.
   declare
      Before : constant Natural := Stderr_Records;
   begin
      Say ("archive-walk reads libc.a",
           Run_Raw ("archive-walk.app", Toolchain & "/lib/libc.a", Delegate => True));
      Say ("a child's stderr arrives through the lent ring", Stderr_Records > Before);
   end;
   --  The whole program: compiled, assembled (its manifest too) and linked
   --  on CuBit, then run: hello.c's main returns 42.
   Say ("gcc links hello",
        Run_Raw (Driver,
                 "-O2" & ASCII.LF & "-fno-pie" & ASCII.LF & "hello.c" & ASCII.LF &
                 "hello-manifest.s" & ASCII.LF & "-o" & ASCII.LF & "hello",
                 Delegate => True, Name => "/toolchain/bin/gcc",
                 Environment => "TMPDIR=/work", Directory => Work));
   Say ("gcc link output matches Linux", Same (Work & "/hello", Work & "/expected-hello"));
   Say ("hello runs and exits with 42", Run_Code (Program => "work/hello", Argv => "") = 42);
   --  A compile error, as the console's gcc demo runs it: cc1's diagnostic
   --  comes back through the driver, which exits with 1.
   declare
      Before : constant Natural := Stderr_Records;
   begin
      Say ("gcc reports a compile error and exits with 1",
           Run_Code (Driver,
                     "-save-temps=obj" & ASCII.LF & "-o" & ASCII.LF & Work & "/bad" & ASCII.LF &
                     Work & "/bad.c",
                     Delegate => True, Name => "/toolchain/bin/gcc") = 1);
      Say ("cc1's diagnostic arrives through the driver", Stderr_Records > Before);
   end;
   debugPrint ((if Failed then "GCC-CHECK: FAIL" else "GCC-CHECK: PASS") & ASCII.LF);
end Main;
