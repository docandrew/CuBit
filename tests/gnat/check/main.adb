--  GNAT on CuBit (docs/self-hosting.md, "GCC and GNAT", plan steps 4 and
--  5). The gcc driver compiles an Ada program (gnat1, then as), gnatbind
--  binds it, and the driver compiles the binder unit and links the whole
--  (ld), without gnatlink (decision D5). The program then runs: it uses
--  Ada.Text_IO, exceptions, a controlled type and Ada.Containers.
--  binutils-compare checks each output against the same GNAT 15.3.0 and
--  binutils on Linux, and the program's transcript against the one it
--  writes on Linux. The tools take a raw argv and work only in the places
--  delegated to them here: the work place and the toolchain (decision D2).
--  Results go to the kernel console.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
with CuBit.Handles.Startup;
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
   use type CuBit.Child_Exits.Termination_Kind;

   Failed : Boolean := False;

   procedure Say (Text : String; Pass : Boolean) is
   begin
      debugPrint ("gnat-check: " & Text & (if Pass then " PASS" else " FAIL") & ASCII.LF);
      Failed := Failed or else not Pass;
   end Say;

   --  The places the tools and the program work in: the work place, and
   --  the toolchain read-only.
   Work      : constant String := "@nvme:0/work";
   Toolchain : constant String := "@nvme:0/toolchain";

   --  What a program writes to one of its text outlets (unix.stderr for
   --  the tools, unix.stdout for the program): a ring lent for each run
   --  (Lend_Outlet), shown on the kernel console as it arrives
   --  (Drain_Outlet, Show_Outlet). The grant is revoked after each run.
   Rings : CuBit.Outlet_Rings.Table;
   Not_Exited : constant Integer := -1;
   Ring_Base : Unsigned_64 := 0;
   Ring_Reference : CuBit.Memory_Grants.Grant_Reference;
   Ring_Pages : constant := 4;
   --  Records read from the lent rings so far (each step checks it grew).
   Records : Natural := 0;

   --  The program's first and last lines on standard output.
   Line_Bytes : constant := 128;
   subtype Line_Length is Natural range 0 .. Line_Bytes;
   First_Line, Last_Line : String (1 .. Line_Bytes);
   First_Length, Last_Length : Line_Length := 0;

   procedure Lend_Outlet (Program, Outlet : String) is
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
      CuBit.Launching.Describe (Program, Descriptor, Length, Result, Failure);
      if Result /= CuBit.Launching.Launched or else Length = 0 then
         return;
      end if;
      PP.Decode (Descriptor (1 .. Length), S, Ok);
      if not Ok then
         return;
      end if;
      PP.Find_Connector (S, Outlet, Index, Found);
      if not Found then
         return;
      end if;
      CuBit.Launching.Lend_Ring
        (Ring_Pages, CuBit.Streams.TYPE_TEXT_LINE, Ring_Base, Grant, Ring_Reference, Ok);
      if Ok then
         Rings.Count := 1;
         Rings.Entries (1) := (Outlet => Index, Grant => Grant);
      end if;
   end Lend_Outlet;

   --  Show what is in the ring now (the run goes on: a full ring would
   --  block the program writing to it), keeping the first and last lines.
   --  Each record is a line, with its line feed.
   procedure Drain_Outlet (Program : String) is
      Buffer : String (1 .. 512);
      Read : Natural;
   begin
      if Rings.Count = 0 then
         return;
      end if;
      loop
         Read := CuBit.Stream_Regions.Read_Owned
           (Ring_Base, Ring_Pages, Buffer'Address, Buffer'Length);
         exit when Read = 0;
         Records := Records + 1;
         if Buffer (Read) = ASCII.LF then
            Read := Read - 1;
         end if;
         debugPrint ("gnat-check: " & Program & ": " & Buffer (1 .. Read) & ASCII.LF);
         Last_Length := Natural'Min (Read, Line_Bytes);
         Last_Line (1 .. Last_Length) := Buffer (1 .. Last_Length);
         if First_Length = 0 then
            First_Length := Last_Length;
            First_Line := Last_Line;
         end if;
      end loop;
   end Drain_Outlet;

   --  The rest, once the run is over; the ring's grant is revoked.
   procedure Show_Outlet (Program : String) is
      Revoked : Boolean;
   begin
      if Rings.Count = 0 then
         return;
      end if;
      Drain_Outlet (Program);
      CuBit.Memory_Grants.Revoke (Ring_Reference, Revoked);
      Rings.Count := 0;
   end Show_Outlet;

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

   --  Wait for Started, draining its outlet meanwhile.
   procedure Wait_Draining (Program : String; Started : CuBit.Launching.Child;
                            Ended : out CuBit.Child_Exits.Report) is
      Has_Ended : Boolean;
   begin
      loop
         CuBit.Launching.Poll_Exit (Started, Has_Ended, Ended);
         exit when Has_Ended;
         Drain_Outlet (Program);
         Sleep_Milliseconds (Poll_Milliseconds);
      end loop;
   end Wait_Draining;

   --  Launch Program with raw arguments (Argv: one per line, after argv[0]
   --  = Program, or Name when given), the Environment (one per line) and
   --  the working Directory (none when empty): its exit code, or
   --  Not_Exited when it was not started or was stopped. Delegate: the work
   --  place, and the toolchain unless Work_Only; else none. Outlet: the
   --  outlet whose lines are read (none when empty).
   function Run_Code (Program : String; Argv : String; Delegate : Boolean := False;
                      Work_Only : Boolean := False;
                      Name : String := ""; Environment : String := "";
                      Directory : String := ""; Outlet : String := "unix.stderr")
                      return Integer is
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
         if not Work_Only then
            LG.Add (Places, LG.Read_Right, Toolchain, Added);
            Accepted := Accepted and Added;
         end if;
      end if;
      LG.Finish (Places, Region, Region_Length);
      if not (Accepted and Added) then
         return Not_Exited;
      end if;
      if Delegate and then Outlet /= "" then
         Lend_Outlet (Program, Outlet);
      end if;
      CuBit.Launching.Launch
        (Program, Block.Data (1 .. Length), Region (1 .. Region_Length), Child, Result, Failure,
         Rings);
      if Result /= CuBit.Launching.Launched then
         Rings.Count := 0;
         return Not_Exited;
      end if;
      Wait_Draining (Program, Child, Ended);
      Show_Outlet (Program);
      return (if Ended.Kind = CuBit.Child_Exits.Exited then Integer (Ended.Code) else Not_Exited);
   end Run_Code;

   function Same (A, B : String) return Boolean is
     (Run_Code ("binutils-compare.app", A & ASCII.LF & B) = 0);

   Driver      : constant String := "toolchain/bin/gcc";
   Driver_Name : constant String := "/toolchain/bin/gcc";
   Binder      : constant String := "toolchain/bin/gnatbind";
   Binder_Name : constant String := "/toolchain/bin/gnatbind";
   Temporary   : constant String := "TMPDIR=/work";
   --  Where gnatbind's .ali files and the archives for the link are
   --  (gcc/ada/rts as installed).
   Ada_Library : constant String := "/toolchain/lib/gcc/x86_64-linux-musl/15.3.0/adalib";

   --  The gcc driver in the work place on relative names, with Arguments
   --  (one per line): its exit code.
   function Gcc (Arguments : String) return Integer is
     (Run_Code (Driver, Arguments, Delegate => True, Name => Driver_Name,
                Environment => Temporary, Directory => Work));

   --  What hello exits with (Ada.Command_Line.Set_Exit_Status in hello.adb).
   Hello_Exit_Code : constant := 7;
   LF : constant Character := ASCII.LF;
begin
   --  procmgr, the "process-manager" startup entry the manifest asks for.
   CuBit.Launching.Bind (CuBit.Handles.Startup.Endpoint_Named ("process-manager"));
   --  The same options and names the Linux reference uses (tests/gnat/build.sh).
   --  Relative names, from the work place: GNU tools read an argument that
   --  starts with '@' as a response file (D8).
   Say ("gcc -c compiles hello.adb (gnat1, as)",
        Gcc ("-O2" & LF & "-fno-pie" & LF & "-c" & LF & "hello.adb") = 0);
   Say ("hello.o matches Linux", Same (Work & "/hello.o", Work & "/expected-hello.o"));
   Say ("gnatbind binds hello.ali",
        Run_Code (Binder, "-x" & LF & "hello.ali", Delegate => True, Name => Binder_Name,
                  Directory => Work) = 0);
   Say ("gcc -c compiles the binder unit",
        Gcc ("-O2" & LF & "-fno-pie" & LF & "-c" & LF & "-gnatA" & LF & "-gnatWb" & LF &
             "-gnatiw" & LF & "-gnatws" & LF & "b~hello.adb") = 0);
   Say ("b~hello.o matches Linux", Same (Work & "/b~hello.o", Work & "/expected-b~hello.o"));
   Say ("gcc links hello (ld, libgnat)",
        Gcc ("-O2" & LF & "-fno-pie" & LF & "b~hello.o" & LF & "hello.o" & LF &
             "hello-manifest.s" & LF & "-L" & Ada_Library & LF & "-lgnat" & LF &
             "-o" & LF & "hello") = 0);
   Say ("hello matches Linux", Same (Work & "/hello", Work & "/expected-hello"));
   --  The program: it writes to standard output and to transcript.txt in
   --  the work place, delegated to it (and nothing else).
   declare
      Before : constant Natural := Records;
      Expected_First : constant String := "hello from Ada";
      Expected_Last  : constant String := "total 25";
   begin
      First_Length := 0;
      Say ("hello runs and exits with 7",
           Run_Code ("work/hello", "", Delegate => True, Work_Only => True,
                     Directory => Work, Outlet => "unix.stdout") = Hello_Exit_Code);
      Say ("hello's standard output arrives",
           Records > Before
             and then First_Line (1 .. First_Length) = Expected_First
             and then Last_Line (1 .. Last_Length) = Expected_Last);
      Say ("hello's transcript matches Linux",
           Same (Work & "/transcript.txt", Work & "/expected-transcript.txt"));
   end;
   --  A compile error: gnat1's diagnostic comes back through the driver,
   --  which exits with 1.
   declare
      Before : constant Natural := Records;
   begin
      Say ("gcc reports an Ada compile error and exits with 1",
           Gcc ("-c" & LF & "bad.adb") = 1);
      Say ("gnat1's diagnostic arrives through the driver", Records > Before);
   end;
   debugPrint ((if Failed then "GNAT-CHECK: FAIL" else "GNAT-CHECK: PASS") & ASCII.LF);
end Main;
