with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Build_POSIX;
with Build_Steps;
with Build_Store;
with Storehouse;
with Storehouse.Hex;

--  Hosted checks of the Storehouse and the step executor (docs/ccl-build.md):
--  put and lookup, immutability, tree identities, cache hits and misses,
--  early cutoff, refusal of undeclared inputs and of paths in literals,
--  detection of tampered entries and of non-reproducible steps, and the
--  crash-safety rules (simulated: work of a dead process, a crash between
--  storing outputs and recording the action). tests/ccl-build/run.sh adds a
--  real SIGKILL during an import.
--
--  The tools are small /bin/sh scripts (host programs, assumed present).
procedure Store_Tests is
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Build_Steps;
   use Storehouse;

   Failures : Natural := 0;
   Checks : Natural := 0;
   --  A process id above Linux's largest pid_max (2^22): never alive.
   Dead_Process : constant := 999_999_999;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL " & What);
      end if;
   end Check;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);

   Scratch : constant String := Ada.Command_Line.Argument (1);
   Sources : constant String := Scratch / "sources";
   Store_Root : constant String := Scratch / "store";

   --  Exactly Content: Text_IO would add a line terminator on closing.
   procedure Write (Name, Content : String; Executable : Boolean := False) is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Directories.Create_Path (Ada.Directories.Containing_Directory (Name));
      if Ada.Directories.Exists (Name) then
         Ada.Directories.Delete_File (Name);
      end if;
      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Name);
      String'Write (Ada.Streams.Stream_IO.Stream (File), Content);
      Ada.Streams.Stream_IO.Close (File);
      if Executable then
         Build_POSIX.Change_Mode (Name, 8#755#);
      end if;
   end Write;

   function Read (Name : String) return String is
      File : File_Type;
      Result : Unbounded_String;
   begin
      Open (File, In_File, Name);
      while not End_Of_File (File) loop
         Append (Result, Get_Line (File) & ASCII.LF);
      end loop;
      Close (File);
      return To_String (Result);
   end Read;

   LF : constant Character := ASCII.LF;
   Store : Build_Store.Store;

   function Put (Name : String; Kind : Entry_Kind := Regular_File) return Digest is
      Identity : Digest;
   begin
      Store.Put_File (Name, Kind, Identity);
      return Identity;
   end Put;

   --  A step running Script on Source, writing its stdout to out.txt.
   function Script_Step (Label, Script, Source : String) return Step is
      S : Step := (Label => To_Unbounded_String (Label), Directory => To_Unbounded_String ("work"), others => <>);
      Tool : constant Input_Number := Add_Input (S, "tools/run", Executable_File, Put (Script, Executable_File));
      Input : constant Input_Number := Add_Input (S, "work/in.txt", Regular_File, Put (Source));
      Output_File : constant Output_Number := Add_Output (S, "work/out.txt");
   begin
      S.Tool := (Kind => Input_Program, Index => Tool, others => <>);
      S.Arguments.Append (Piece'(Kind => Local_Input, Index => Input, others => <>));
      S.Capture := Output_File;
      return S;
   end Script_Step;

   function Output_Text (O : Outcome) return String is
     (Read (Store.Path_Of (Regular_File, O.Outputs (1))));

   Copy_Script : constant String := Sources / "copy.sh";
   Strip_Script : constant String := Sources / "strip.sh";
   Random_Script : constant String := Sources / "random.sh";
   Peek_Script : constant String := Sources / "peek.sh";
begin
   Ada.Directories.Create_Path (Sources);
   Write (Copy_Script, "#!/bin/sh" & LF & "exec /bin/cat ""$1""" & LF, Executable => True);
   Write (Strip_Script, "#!/bin/sh" & LF & "exec /bin/grep -v '^#' ""$1""" & LF, Executable => True);
   Write (Random_Script, "#!/bin/sh" & LF & "exec /usr/bin/od -An -N8 -tx8 /dev/urandom" & LF,
          Executable => True);
   Write (Peek_Script, "#!/bin/sh" & LF & "exec /bin/cat secret.txt" & LF, Executable => True);
   Store := Build_Store.Open (Store_Root);

   --  Files: named by content, read-only, stored once.
   Write (Sources / "a.txt", "alpha" & LF);
   declare
      A : constant Digest := Put (Sources / "a.txt");
   begin
      --  sha256sum of "alpha\n".
      Check (Hex.Image (A) = "b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060", "file identity is its SHA-256");
      Check (Store.Contains (Regular_File, A), "stored file is present");
      Check (not Store.Contains (Executable_File, A), "kinds are separate namespaces");
      Check (not GNAT.OS_Lib.Is_Writable_File (Store.Path_Of (Regular_File, A)), "entry is read-only");
      Check (Put (Sources / "a.txt") = A, "storing again gives the same entry");
   end;

   --  Trees: a Merkle identity over names, kinds and contents.
   Write (Sources / "tree/sub/b.txt", "beta" & LF);
   Write (Sources / "tree/tool", "#!/bin/sh" & LF, Executable => True);
   Build_POSIX.Symbolic_Link ("sub/b.txt", Sources / "tree/link");
   declare
      T : Digest;
      Before : constant Digest := Build_Store.Hash_Tree (Sources / "tree");
   begin
      Store.Put_Tree (Sources / "tree", T);
      Check (T = Before, "tree identity is its listing's hash");
      Check (Build_Store.Hash_Tree (Store.Path_Of (Tree, T)) = T, "stored tree hashes to its name");
      Check (not GNAT.OS_Lib.Is_Writable_File (Store.Path_Of (Tree, T) / "sub/b.txt"), "tree files read-only");
      Check (Build_POSIX.Is_Executable (Store.Path_Of (Tree, T) / "tool"), "tree keeps the executable bit");
      Write (Sources / "tree/sub/b.txt", "beta!" & LF);
      Check (Build_Store.Hash_Tree (Sources / "tree") /= T, "a changed file changes the tree identity");
      declare
         Edited : constant Digest := Build_Store.Hash_Tree (Sources / "tree");
      begin
         Build_POSIX.Change_Mode (Sources / "tree/sub/b.txt", 8#755#);
         Check (Build_Store.Hash_Tree (Sources / "tree") /= Edited, "the executable bit is part of a tree");
      end;
   end;

   --  Cache: a hit runs nothing; a changed input runs again.
   Write (Sources / "in1.txt", "one" & LF);
   declare
      First, Second, Third : Outcome;
   begin
      Run (Store, Script_Step ("copy", Copy_Script, Sources / "in1.txt"), First);
      Check (First.Ran and then Output_Text (First) = "one" & LF, "first run runs");
      Run (Store, Script_Step ("copy", Copy_Script, Sources / "in1.txt"), Second);
      Check (not Second.Ran and then Second.Outputs (1) = First.Outputs (1), "second run is a hit");
      Check (Second.Step_Identity = First.Step_Identity, "same step, same identity");
      Write (Sources / "in1.txt", "two" & LF);
      Run (Store, Script_Step ("copy", Copy_Script, Sources / "in1.txt"), Third);
      Check (Third.Ran and then Output_Text (Third) = "two" & LF, "changed input runs again");
      --  The label is for people only.
      declare
         Renamed : Outcome;
      begin
         Run (Store, Script_Step ("another label", Copy_Script, Sources / "in1.txt"), Renamed);
         Check (not Renamed.Ran, "label is not part of the identity");
      end;
   end;

   --  Early cutoff: a comment change reruns the stripper only.
   Write (Sources / "c.txt", "# first comment" & LF & "body" & LF);
   declare
      Strip_1, Copy_1, Strip_2, Copy_2 : Outcome;
      function Copy_Of (Stripped : Outcome) return Step is
         S : Step := (Label => To_Unbounded_String ("copy stripped"),
                      Directory => To_Unbounded_String ("work"), others => <>);
         Tool : constant Input_Number := Add_Input (S, "tools/run", Executable_File, Put (Copy_Script, Executable_File));
         Input : constant Input_Number := Add_Input (S, "work/in.txt", Regular_File, Stripped.Outputs (1));
      begin
         S.Tool := (Kind => Input_Program, Index => Tool, others => <>);
         S.Arguments.Append (Piece'(Kind => Local_Input, Index => Input, others => <>));
         S.Capture := Add_Output (S, "work/out.txt");
         return S;
      end Copy_Of;
   begin
      Run (Store, Script_Step ("strip", Strip_Script, Sources / "c.txt"), Strip_1);
      Run (Store, Copy_Of (Strip_1), Copy_1);
      Write (Sources / "c.txt", "# second comment" & LF & "body" & LF);
      Run (Store, Script_Step ("strip", Strip_Script, Sources / "c.txt"), Strip_2);
      Run (Store, Copy_Of (Strip_2), Copy_2);
      Check (Strip_1.Ran and then Copy_1.Ran, "early cutoff: first build runs both");
      Check (Strip_2.Ran, "early cutoff: the edited input reruns its step");
      Check (Strip_2.Outputs (1) = Strip_1.Outputs (1), "early cutoff: same output bytes");
      Check (not Copy_2.Ran, "early cutoff: the consumer is answered by the cache");
   end;

   --  Only declared inputs are in the step directory.
   Write (Sources / "secret.txt", "secret" & LF);
   declare
      S : Step := (Label => To_Unbounded_String ("peek"), Directory => To_Unbounded_String ("work"), others => <>);
      Tool : constant Input_Number := Add_Input (S, "tools/run", Executable_File, Put (Peek_Script, Executable_File));
      Result : Outcome;
      Refused : Boolean := False;
   begin
      S.Tool := (Kind => Input_Program, Index => Tool, others => <>);
      S.Capture := Add_Output (S, "work/out.txt");
      begin
         Run (Store, S, Result);
      exception
         when Step_Error => Refused := True;
      end;
      Check (Refused, "an undeclared input is absent");
      Declare_Input (S, "work/secret.txt", Regular_File, Put (Sources / "secret.txt"));
      Run (Store, S, Result);
      Check (Result.Ran and then Output_Text (Result) = "secret" & LF, "the declared input is present");
   end;

   --  Literal arguments cannot name files.
   declare
      S : Step := Script_Step ("absolute", Copy_Script, Sources / "a.txt");
      Refused : Boolean := False;
      Ignored : Digest;
   begin
      S.Arguments.Append (Text ("/etc/passwd"));
      begin
         Ignored := Identity (S);
      exception
         when Step_Error => Refused := True;
      end;
      Check (Refused, "a path in a literal is refused");
   end;

   --  A step whose outputs differ between runs is reported.
   declare
      First, Again : Outcome;
      Deleted : Boolean;
   begin
      Run (Store, Script_Step ("random", Random_Script, Sources / "a.txt"), First);
      --  Collect its output (as garbage collection would), then rebuild.
      GNAT.OS_Lib.Delete_File (Store.Path_Of (Regular_File, First.Outputs (1)), Deleted);
      Run (Store, Script_Step ("random", Random_Script, Sources / "a.txt"), Again);
      Check (Deleted and then Again.Ran, "a collected output runs the step again");
      Check (Again.Not_Reproducible, "differing outputs are reported");
   end;

   --  A tampered entry is found by verification.
   declare
      Damaged : Natural := 0;
      Checked : Natural;
      Victim : constant Digest := Put (Sources / "a.txt");
      procedure Report (Kind : Entry_Kind; Name : String) is
      begin
         Damaged := Damaged + 1;
         Check (Kind = Regular_File and then Name = Hex.Image (Victim), "the damaged entry is named");
      end Report;
      procedure Verify_All is new Build_Store.Verify (Report);
   begin
      Verify_All (Store, Checked);
      Check (Damaged = 0, "a clean store verifies");
      Build_POSIX.Change_Mode (Store.Path_Of (Regular_File, Victim), 8#644#);
      Write (Store.Path_Of (Regular_File, Victim), "tampered" & LF);
      Verify_All (Store, Checked);
      Check (Damaged = 1 and then Checked > 1, "a tampered entry is reported");
      Ada.Directories.Delete_File (Store.Path_Of (Regular_File, Victim));
      Check (Put (Sources / "a.txt") = Victim, "a removed entry is stored again");
   end;

   --  Crash safety, simulated.
   declare
      Garbage : constant String := Store_Root / "work" /
        (Integer'Image (Dead_Process) (2 .. Integer'Image (Dead_Process)'Last) & "-1");
      Own : constant String := Store.New_Work;
      Content : constant String := "staged but never renamed" & LF;
      Staged_Identity : Digest;
   begin
      --  A crash after staging an entry, before its rename: the entry is
      --  absent, and the next open removes the staging.
      Write (Garbage / "entry", Content);
      Write (Sources / "staged.txt", Content);
      Staged_Identity := Build_Store.Hash_File (Sources / "staged.txt");
      Check (not Store.Contains (Regular_File, Staged_Identity), "a staged entry is not an entry");
      Store := Build_Store.Open (Store_Root);
      Check (not Ada.Directories.Exists (Garbage), "work of a dead process is removed");
      Check (Ada.Directories.Exists (Own), "work of a live process is kept");
      Check (Put (Sources / "staged.txt") = Staged_Identity, "the entry is stored after recovery");
      Build_Store.Remove (Own);
   end;
   declare
      --  A crash after the outputs were stored, before the action record:
      --  the step runs again and gives the same entries.
      Source : constant String := Sources / "late.txt";
      Ran : Outcome;
   begin
      Write (Source, "recorded late" & LF);
      Check (Put (Source) /= Put (Copy_Script, Executable_File), "distinct entries");
      Run (Store, Script_Step ("late", Copy_Script, Source), Ran);
      Ada.Directories.Delete_File (Store_Root / "actions" / Hex.Image (Ran.Step_Identity));
      declare
         Again : Outcome;
      begin
         Run (Store, Script_Step ("late", Copy_Script, Source), Again);
         Check (Again.Ran and then not Again.Not_Reproducible and then Again.Outputs (1) = Ran.Outputs (1),
                "a missing action record reruns the step to the same outputs");
      end;
   end;

   --  A damaged action record is refused, not trusted.
   declare
      S : constant Step := Script_Step ("copy", Copy_Script, Sources / "a.txt");
      Result : Outcome;
      Refused : Boolean := False;
   begin
      Run (Store, S, Result);
      Build_POSIX.Change_Mode (Store_Root / "actions" / Hex.Image (Result.Step_Identity), 8#644#);
      Write (Store_Root / "actions" / Hex.Image (Result.Step_Identity), "garbage");
      begin
         Run (Store, S, Result);
      exception
         when Step_Error => Refused := True;
      end;
      Check (Refused, "a damaged action record is refused");
   end;

   Check (Build_POSIX.Process_Alive (Build_POSIX.Process_Id), "own process is alive");
   Put_Line ("store:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      raise Program_Error with "store tests failed";
   end if;
end Store_Tests;
