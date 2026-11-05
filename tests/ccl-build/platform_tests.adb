with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Interfaces.C;
with Interfaces.C.Strings;
with Build_Platform;

--  Build_Platform's contract, the same checks for each body (tests/ccl-
--  build/run.sh builds this test with the Linux body and, hosted on Linux,
--  with the CuBit body: CCL_BUILD_PLATFORM=cubit). What the store relies
--  on, whatever the platform does underneath:
--    - Rename_New places a file or a directory only where nothing is, and
--      of N processes racing for one name exactly one places it, whole;
--    - Link and Link_New put Existing's bytes under the new name, and
--      Link_New never replaces;
--    - a lock conflicts across processes, and is released when its holder
--      exits or is killed;
--    - Process_Alive: a process that marked itself alive is alive until it
--      exits or is killed;
--    - Run starts a program in a directory and reports its exit status.
--
--    platform_tests SCRATCH          the checks (prints a summary)
--    platform_tests child MODE ...   a helper process (below)
procedure Platform_Tests is
   use Ada.Text_IO;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);
   function Argument (N : Positive) return String renames Ada.Command_Line.Argument;

   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL " & What);
      end if;
   end Check;

   procedure Write (Name, Text : String) is
      use Ada.Streams.Stream_IO;
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Create (File, Out_File, Name);
      String'Write (Stream (File), Text);
      Close (File);
   end Write;

   function Read (Name : String) return String is
      use Ada.Streams.Stream_IO;
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Open (File, In_File, Name);
      declare
         Text : String (1 .. Natural (Size (File)));
      begin
         String'Read (Stream (File), Text);
         Close (File);
         return Text;
      end;
   end Read;

   function Image (N : Integer) return String is
      Text : constant String := Integer'Image (N);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --  Waits (at most Limit_Ms) until Name exists.
   function Appears (Name : String; Limit_Ms : Natural) return Boolean is
      Step_Ms : constant := 10;
   begin
      for Round in 0 .. Limit_Ms / Step_Ms loop
         if Ada.Directories.Exists (Name) then
            return True;
         end if;
         delay Duration (Step_Ms) / 1000.0;
      end loop;
      return False;
   end Appears;

   Self : constant String := GNAT.OS_Lib.Normalize_Pathname (Ada.Command_Line.Command_Name);
   Ready_Limit_Ms : constant := 10_000;
   Hold_Seconds : constant := 1.0;
   Racers : constant := 8;

   --  A helper process, started without waiting; its process number.
   function Start (Arguments : GNAT.OS_Lib.Argument_List) return GNAT.OS_Lib.Process_Id is
     (GNAT.OS_Lib.Non_Blocking_Spawn (Self, Arguments));
begin
   if Ada.Command_Line.Argument_Count >= 2 and then Argument (1) = "child" then
      declare
         Mode : constant String := Argument (2);
      begin
         if Mode = "rename" then
            --  child rename SOURCE TARGET RESULT: Rename_New, then "placed"
            --  or "lost" into RESULT.
            declare
               Placed : Boolean;
            begin
               Build_Platform.Rename_New (Argument (3), Argument (4), Placed);
               Write (Argument (5), (if Placed then "placed" else "lost"));
            end;
         elsif Mode = "hold" then
            --  child hold LOCK-DIRECTORY LOCK READY: marks itself alive,
            --  holds LOCK exclusively, writes its process number to READY,
            --  then sleeps (to be killed, or to exit holding both).
            declare
               Held : Build_Platform.Lock;
            begin
               Build_Platform.Mark_Alive (Argument (3));
               Held := Build_Platform.Acquire (Argument (4), Build_Platform.Exclusive, Wait => False);
               if Build_Platform.Held (Held) then
                  Write (Argument (5) & ".part", Image (Integer (Build_Platform.Process_Id)));
                  Ada.Directories.Rename (Argument (5) & ".part", Argument (5));
                  delay Hold_Seconds;
               end if;
            end;
         elsif Mode = "exit" then
            --  child exit CODE NAME: writes NAME (in its working directory).
            Write (Argument (4), "here");
            GNAT.OS_Lib.OS_Exit (Integer'Value (Argument (3)));
         end if;
      end;
      return;
   end if;

   declare
      Scratch : constant String := Argument (1);
      Locks : constant String := Scratch / "locks";
   begin
      Ada.Directories.Create_Path (Locks);
      Build_Platform.Mark_Alive (Locks);
      Check (Build_Platform.Process_Alive (Build_Platform.Process_Id), "this process is alive");

      --  Which imported trees are recorded in place, by host.
      case Build_Platform.This_Host is
         when Build_Platform.Linux =>
            Check (Build_Platform.Records_In_Place ("/nix/store/0-gnat", Toolchain => True)
                   and then Build_Platform.Records_In_Place ("/nix/store/0-crate", Toolchain => False)
                   and then not Build_Platform.Records_In_Place (Scratch, Toolchain => True),
                   "linux: trees in the Nix store, and only those, are recorded in place");
         when Build_Platform.CuBit =>
            Check (Build_Platform.Records_In_Place ("/toolchain", Toolchain => True)
                   and then not Build_Platform.Records_In_Place ("/toolchain", Toolchain => False),
                   "cubit: toolchains are recorded in place, a library's seed is copied");
      end case;

      --  Rename_New: files and directories, never replacing.
      declare
         Placed : Boolean;
      begin
         Write (Scratch / "a", "first");
         Build_Platform.Rename_New (Scratch / "a", Scratch / "entry", Placed);
         Check (Placed and then Read (Scratch / "entry") = "first", "rename_new places a file");
         Write (Scratch / "b", "second");
         Build_Platform.Rename_New (Scratch / "b", Scratch / "entry", Placed);
         Check (not Placed and then Read (Scratch / "entry") = "first", "rename_new never replaces a file");
         Check (Ada.Directories.Exists (Scratch / "b"), "the loser keeps its source");
         Ada.Directories.Create_Path (Scratch / "tree/sub");
         Write (Scratch / "tree/sub/x", "x");
         Build_Platform.Rename_New (Scratch / "tree", Scratch / "tree-entry", Placed);
         Check (Placed and then Read (Scratch / "tree-entry/sub/x") = "x", "rename_new places a directory");
         Ada.Directories.Create_Path (Scratch / "tree2");
         Build_Platform.Rename_New (Scratch / "tree2", Scratch / "tree-entry", Placed);
         Check (not Placed and then Ada.Directories.Exists (Scratch / "tree-entry/sub/x"),
                "rename_new never replaces a directory");
      end;

      --  Link and Link_New.
      declare
         Placed : Boolean;
      begin
         Write (Scratch / "source", "bytes");
         Build_Platform.Link (Scratch / "source", Scratch / "linked");
         Check (Read (Scratch / "linked") = "bytes", "link: the same bytes under the new name");
         Write (Scratch / "other", "other");
         Build_Platform.Link_New (Scratch / "other", Scratch / "linked", Placed);
         Check (not Placed and then Read (Scratch / "linked") = "bytes", "link_new never replaces");
         Build_Platform.Link_New (Scratch / "other", Scratch / "linked2", Placed);
         Check (Placed and then Read (Scratch / "linked2") = "other", "link_new places");
      end;

      --  Racers for one name: exactly one places it, whole.
      declare
         use GNAT.OS_Lib;
         Children : array (1 .. Racers) of Process_Id;
         Winners : Natural := 0;
         Success : Boolean;
         Done : Process_Id;
      begin
         for R in Children'Range loop
            Write (Scratch / ("racer-" & Image (R)), "racer " & Image (R));
         end loop;
         for R in Children'Range loop
            declare
               Arguments : Argument_List :=
                 [new String'("child"), new String'("rename"),
                  new String'(Scratch / ("racer-" & Image (R))), new String'(Scratch / "raced"),
                  new String'(Scratch / ("result-" & Image (R)))];
            begin
               Children (R) := Start (Arguments);
               for A of Arguments loop
                  Free (A);
               end loop;
            end;
         end loop;
         for R in Children'Range loop
            Wait_Process (Done, Success);
         end loop;
         for R in Children'Range loop
            if Read (Scratch / ("result-" & Image (R))) = "placed" then
               Winners := Winners + 1;
               Check (Read (Scratch / "raced") = "racer " & Image (R), "the winner's bytes are the entry");
            end if;
         end loop;
         Check (Winners = 1, "exactly one racer places the name (" & Image (Winners) & ")");
      end;

      --  Locks and liveness across processes: a holder that exits, one
      --  that is killed.
      for Killed in Boolean loop
         declare
            use GNAT.OS_Lib;
            Name : constant String := (if Killed then "killed" else "exits");
            Lock_Name : constant String := Scratch / (Name & ".lock");
            Ready : constant String := Scratch / (Name & ".ready");
            Arguments : Argument_List :=
              [new String'("child"), new String'("hold"), new String'(Locks),
               new String'(Lock_Name), new String'(Ready)];
            Child : constant Process_Id := Start (Arguments);
            Probe : Build_Platform.Lock;
            Holder : Interfaces.C.int := 0;
            Done : Process_Id;
            Success : Boolean;
         begin
            for A of Arguments loop
               Free (A);
            end loop;
            if Appears (Ready, Ready_Limit_Ms) then
               Holder := Interfaces.C.int'Value (Read (Ready));
               Probe := Build_Platform.Acquire (Lock_Name, Build_Platform.Exclusive, Wait => False);
               Check (not Build_Platform.Held (Probe), Name & ": the lock conflicts while held");
               Check (Build_Platform.Process_Alive (Holder), Name & ": the holder is alive");
               if Killed then
                  Kill (Child, Hard_Kill => True);
               end if;
               Wait_Process (Done, Success);
               Check (Done = Child, Name & ": the holder ended");
               Check (not Build_Platform.Process_Alive (Holder), Name & ": the holder is dead");
               Probe := Build_Platform.Acquire (Lock_Name, Build_Platform.Exclusive, Wait => False);
               Check (Build_Platform.Held (Probe), Name & ": its lock was released");
               if Build_Platform.Held (Probe) then
                  Build_Platform.Release (Probe, Delete => True);
               end if;
            else
               Check (False, Name & ": the holder never got ready");
               Kill (Child, Hard_Kill => True);
               Wait_Process (Done, Success);
            end if;
         end;
      end loop;

      --  Run: in a directory, with an exit status.
      declare
         use Interfaces.C.Strings;
         Directory : constant String := Scratch / "run";
         Status : Integer;
         Arguments : Build_Platform.Argument_List :=
           [New_String (Self), New_String ("child"), New_String ("exit"), New_String ("7"),
            New_String ("marker")];
      begin
         Ada.Directories.Create_Path (Directory);
         Build_Platform.Run (Self, Arguments, Directory, "", Status);
         Check (Status = 7, "run: the exit status");
         Check (Ada.Directories.Exists (Directory / "marker"), "run: in its directory");
         for A of Arguments loop
            Free (A);
         end loop;
      end;
   end;

   Put_Line ("platform:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Platform_Tests;
