--  Hosted tests of system.disks and system.install (CCL.Interfaces.Install,
--  CCL_Install_Bindings): typed at a CCL session, decoded into the
--  installer protocol's request, refused or answered as typed records.
--  system.install answers with a task at once; (wait t) stops the entry
--  until the bindings' Pump completes the task, and the entry then
--  finishes with the report (as the console resumes it).
with Ada.Text_IO;
with Interfaces;
with CCL.Catalog;
with CCL.Host_Replay;
with CCL.Host_Values;
with CCL.Language;
with CCL.Sessions;
with CCL.Streams;
with CCL_Stream_Table;
with CCL_Install_Bindings;
with CuBit.Installer_Protocol;
with Install_Record;

procedure Main is
   use type CuBit.Installer_Protocol.Disk_Ordinal;
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
      end if;
      Ada.Text_IO.Put_Line ((if Condition then "PASS: " else "FAIL: ") & Label);
   end Check;

   Table : CCL_Stream_Table.Table;
   type No_Context is null record;
   procedure Invoke
     (Context : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      CCL_Install_Bindings.Invoke (Binding, Argument, Table, Reply);
   end Invoke;
   procedure Read
     (Context : in out No_Context; Request : CCL.Streams.View_Request; Reply : in out CCL.Streams.View_Reply)
   is
      pragma Unreferenced (Context);
   begin
      CCL_Stream_Table.Read (Table, Request, Reply);
   end Read;
   package Replay is new CCL.Host_Replay (No_Context, Invoke, Read);
   procedure Submit is new CCL.Sessions.Submit_With_Values
     (Replay.Context, Replay.Invoke_Logged, Read_Stream => Replay.Read_Logged);
   procedure Resume is new CCL.Sessions.Resume_With_Values
     (Replay.Context, Replay.Invoke_Logged, Read_Stream => Replay.Read_Logged);

   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   Context : Replay.Context;
   Installed : Boolean;
   Outcome : CCL.Language.Interpretation_Result;
   use type CCL.Language.Interpretation_Status;

   function Run (Source : String) return String is
   begin
      Replay.Clear (Context);
      Submit (Session, Source, CCL.Sessions.Default_Fuel, Grants, Context, Outcome);
      return CCL.Sessions.Result_Image (Outcome);
   end Run;

   --  The entry Source stopped at its wait: pump until the task completes,
   --  then resume it, as the console does.
   Pumps : Natural := 0;
   function Finish return String is
      Arrived, Resumed : Boolean;
      Maximum_Pumps : constant := 8;
   begin
      Pumps := 0;
      while Outcome.Status = CCL.Language.Waiting_On_Task and then Pumps < Maximum_Pumps loop
         CCL_Install_Bindings.Pump (Table, 0, False, Arrived);
         Pumps := Pumps + 1;
         Replay.Rewind (Context.Calls);
         Resume (Session, CCL.Sessions.Length (Session), CCL.Sessions.Default_Fuel, Grants,
                 Context, Outcome, Resumed);
      end loop;
      return CCL.Sessions.Result_Image (Outcome);
   end Finish;

   --  What the console does after each entry: streams and tasks no name
   --  holds are closed, unless pinned.
   function Held (Handle : CCL.Streams.Handle) return Boolean is (False);
   procedure Retain is new CCL_Stream_Table.Retain (Held);

   function Waited (Source : String) return String is
      Started : constant String := Run ("(wait " & Source & ")");
   begin
      return (if Outcome.Status = CCL.Language.Waiting_On_Task then Finish else Started);
   end Waited;

   function Request
     (Model, Serial : String; Disk : String := "0"; Blocks : String := "262144";
      Ratio : String := "262144") return String is
     ("(system.install (Install_Request disk => " & Disk & " confirm => (Disk_Identity """ & Model &
      """ """ & Serial & """ " & Blocks & " 4096) inode_ratio => " & Ratio &
      " esp_mib => 260 source => ""@nvme:0/install""))");
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);
   CCL_Install_Bindings.Install (Catalog, Grants, Installed);
   Check (Installed, "system.* is published and granted");
   CCL.Sessions.Initialize (Session, Catalog);
   declare
      Listed : constant String := Run ("(system.disks)");
   begin
      Check (Listed = "List<Disk>: [(Disk 0 (Disk_Identity ""QEMU UFS"" ""cubitufs"" 262144 4096)) (Disk 1 (Disk_Identity ""NO SERIAL"" """" 31246336 4096))]",
             "system.disks lists the disk: " & Listed);
   end;
   declare
      Refused : constant String := Waited (Request ("QEMU UFS", "another-disk"));
   begin
      Check (Refused = "Install_Report: (Install_Report Install_Outcome.Wrong_Disk Identity_Proof.Serial_Number 0 0)",
             "a confirmation naming another disk is refused: " & Refused);
   end;
   declare
      Started : constant String := Run ("(define job " & Request ("QEMU UFS", "cubitufs") & ")");
      Again : constant String := Run (Request ("QEMU UFS", "cubitufs"));
      Running : constant String := Run ("(system.progress)");
   begin
      Check (Started'Length > 0, "system.install returns at once with a task: " & Started);
      --  Regression: the console sleeps until its next deadline; a followed
      --  install is one, or its task never completes while the console idles.
      Check (CCL_Install_Bindings.Next_Query_Delay (1_000_000) /= Interfaces.Unsigned_64'Last,
             "a followed install gives the front end a deadline to wake for");
      Check (Again'Length > 0 and then Again (Again'First .. Again'First + 4) /= "Task<",
             "a second install while one runs is refused: " & Again);
      Check (Running = "Install_Progress: (Install_Progress Install_State.Running 3 7 6172 12345)",
             "system.progress reports the running install: " & Running);
   end;
   declare
      Waiting : constant String := Run ("(wait job)");
      Done : constant String := (if Outcome.Status = CCL.Language.Waiting_On_Task then Finish else Waiting);
   begin
      Check (Done = "Install_Report: (Install_Report Install_Outcome.Installed Identity_Proof.Serial_Number 7 12345)",
             "the confirmed install is answered: " & Done);
      Check (Install_Record.Seen and then Install_Record.Last.Disk = 0
             and then Install_Record.Last.Ratio = 262_144 and then Install_Record.Last.ESP_Size = 260
             and then Install_Record.Last.Source (1 .. Install_Record.Last.Length) = "@nvme:0/install",
             "the request decodes into the installer protocol");
   end;
   declare
      Geometry : constant String := Waited (Request ("QEMU UFS", "cubitufs", Blocks => "262143"));
   begin
      Check (Geometry = "Install_Report: (Install_Report Install_Outcome.Wrong_Disk Identity_Proof.Serial_Number 0 0)",
             "a confirmation with the wrong block count is refused: " & Geometry);
   end;
   declare
      Unserial : constant String := Waited (Request ("NO SERIAL", "", Disk => "1", Blocks => "31246336"));
   begin
      Check (Unserial = "Install_Report: (Install_Report Install_Outcome.Installed Identity_Proof.Model_Geometry 7 12345)",
             "a disk without a serial installs by model and geometry, and says so: " & Unserial);
   end;
   declare
      Low : constant String := Run (Request ("QEMU UFS", "cubitufs", Ratio => "4096"));
   begin
      Check (Low'Length > 0 and then Low (Low'First .. Low'First + 14) /= "Install_Report:",
             "an inode ratio below the range is refused before the installer: " & Low);
   end;
   declare
      Missing : constant String := Run ("(system.install (Install_Request disk => 0 inode_ratio => 262144 esp_mib => 260 source => ""x""))");
   begin
      Check (Missing'Length > 0 and then Missing (Missing'First .. Missing'First + 14) /= "Install_Report:",
             "no confirmation, no install (nothing is defaulted): " & Missing);
   end;
   Check (CCL_Install_Bindings.Next_Query_Delay (2_000_000) = Interfaces.Unsigned_64'Last,
          "no install followed: no deadline");
   --  Regression: an entry waiting on an install, then another entry and
   --  the console's Retain before the install's task completes; the
   --  waiting entry must still resume with the report (the task was
   --  unpinned on completion and closed, and the entry never finished).
   declare
      Waiting : constant String := Run ("(wait " & Request ("QEMU UFS", "cubitufs") & ")");
      Waiting_Calls : constant Replay.Log := Context.Calls;
      Between : constant String := Run ("(system.progress)");
      Done : Natural;
      Arrived : Boolean;
   begin
      Retain (Table);
      CCL_Install_Bindings.Pump (Table, 0, False, Arrived);
      Retain (Table);
      Run_Again : declare
         Resumed : Boolean;
      begin
         Context.Calls := Waiting_Calls;
         Replay.Rewind (Context.Calls);
         Resume (Session, CCL.Sessions.Length (Session) - 1, CCL.Sessions.Default_Fuel, Grants,
                 Context, Outcome, Resumed);
         Done := (if Resumed and then Outcome.Status = CCL.Language.Succeeded then 1 else 0);
      end Run_Again;
      Check (Done = 1 and then CCL.Sessions.Result_Image (Outcome) =
               "Install_Report: (Install_Report Install_Outcome.Installed Identity_Proof.Serial_Number 7 12345)",
             "an install's waiting entry resumes after later entries: " & Waiting & " / " & Between &
             " / " & CCL.Sessions.Result_Image (Outcome));
   end;
   declare
      Last : constant String := Run ("(system.progress)");
   begin
      Check (Last = "Install_Progress: (Install_Progress Install_State.Finished 7 7 12345 12345)",
             "system.progress reports the last install once finished: " & Last);
   end;
   --  Install_Status's words round-trip, and malformed ones are refused.
   declare
      package IP renames CuBit.Installer_Protocol;
      use type IP.Install_Progress;
      Item : constant IP.Install_Progress :=
        (State => IP.Finished, Outcome => IP.Files_Do_Not_Fit, Proof => IP.Model_And_Geometry,
         Files_Done => 3, Files => 4_000, Bytes_Done => 2 ** 33, Bytes => 2 ** 34);
      Back : IP.Install_Progress;
      Valid : Boolean;
      Words : IP.Status_Words := IP.Encode_Status (Item);
   begin
      IP.Decode_Status (Words, Back, Valid);
      Check (Valid and then Back = Item, "install status words round-trip");
      Words (1) := Interfaces.Shift_Left (Interfaces.Unsigned_64 (2), 32) or 3;
      IP.Decode_Status (Words, Back, Valid);
      Check (not Valid, "a status with more files done than files is refused");
      Words := IP.Encode_Status (Item);
      Words (0) := Words (0) or Interfaces.Shift_Left (1, 24);
      IP.Decode_Status (Words, Back, Valid);
      Check (not Valid, "a status with stray bits is refused");
   end;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("CCL-INSTALL-CHECK: PASS");
   else
      Ada.Text_IO.Put_Line ("CCL-INSTALL-CHECK: FAIL");
   end if;
end Main;
