------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Block_Client;
with CuBit.Clocks;
with CuBit.Disk_Install;
with CuBit.Disk_Install.Client;
with CuBit.Hardware_Random;
with Disk_Layout; use Disk_Layout;
with Install_Sources;
with Install_Steps;
with Install_Tree;
with Layout_Writer;

package body System_Install is
   package BC renames CuBit.Block_Client;
   package DIC renames CuBit.Disk_Install.Client;
   package DI renames CuBit.Disk_Install;
   use type Install_Tree.Entry_Kind;
   use type IP.Install_State, IP.Install_Outcome, IP.Identity_Proof;

   --  Sessions move up to a writer batch at a time.
   Session_Pages : constant BC.Page_Count := 16;
   Identify_Pages : constant BC.Page_Count := 1;

   Through_Endpoints : Endpoints;
   Disk : BC.Session;
   Disk_Block : Unsigned_64 := 1;
   Buffer_Zero : Boolean := False;
   --  The last device request ran past its deadline.
   Device_Timed_Out : Boolean := False;

   type Byte_View is array (Natural range <>) of Unsigned_8;

   Current : IP.Install_Progress;
   Installing : Boolean := False;

   function Running return Boolean is (Installing);

   function To_Wire (Item : Install_Steps.Step) return IP.Install_Step is
     (case Item is
        when Install_Steps.Checking => IP.Checking,
        when Install_Steps.Partitioning => IP.Partitioning,
        when Install_Steps.Formatting_Root => IP.Formatting_Root,
        when Install_Steps.Filling_Storehouse => IP.Filling_Storehouse,
        when Install_Steps.Writing_Generation => IP.Writing_Generation,
        when Install_Steps.Verifying_Root => IP.Verifying_Root,
        when Install_Steps.Writing_ESP => IP.Writing_ESP,
        when Install_Steps.Verifying_ESP => IP.Verifying_ESP,
        when Install_Steps.Making_Bootable => IP.Making_Bootable,
        when Install_Steps.Flushing => IP.Flushing,
        when Install_Steps.Done => IP.Done);

   function Progress return IP.Install_Progress is
      Now : IP.Install_Progress := Current;
   begin
      if Installing then
         Now.Files_Done := Unsigned_64'Min (Install_Sources.Files_Read, Now.Files);
         Now.Bytes_Done := Unsigned_64'Min (Install_Sources.Bytes_Read, Now.Bytes);
      end if;
      return Now;
   end Progress;

   procedure Initialize (Through : Endpoints) is
   begin
      Through_Endpoints := Through;
   end Initialize;

   procedure Note_Transfer (Result : BC.Transfer_Result; OK : out Boolean) is
      use type BC.Transfer_Result;
   begin
      OK := Result = BC.Transferred;
      if Result = BC.Timed_Out then
         Device_Timed_Out := True;
         Log ("the disk's driver did not answer a request within" &
              Unsigned_64'Image (Device_Request_Deadline_Ms) & " ms");
      end if;
   end Note_Transfer;

   procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean) is
      View : Byte_View (0 .. Data'Length - 1) with Import, Address => BC.Buffer (Disk);
      Moved : Unsigned_64;
      Result : BC.Transfer_Result;
   begin
      Serve;
      OK := Offset mod Disk_Block = 0 and then Unsigned_64 (Data'Length) mod Disk_Block = 0
        and then Unsigned_64 (Data'Length) / Disk_Block in 1 .. BC.Transfer_Limit (Disk);
      if not OK then
         return;
      end if;
      for I in View'Range loop
         View (I) := Data (Data'First + I);
      end loop;
      Buffer_Zero := False;
      BC.Write (Disk, Offset / Disk_Block, Unsigned_64 (Data'Length) / Disk_Block, False, Moved, Result);
      Note_Transfer (Result, OK);
   end Put;

   procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean) is
      Next : Unsigned_64 := Offset / Disk_Block;
      Left : Unsigned_64 := Length / Disk_Block;
      Step, Moved : Unsigned_64;
      Result : BC.Transfer_Result;
   begin
      OK := Offset mod Disk_Block = 0 and then Length mod Disk_Block = 0;
      if not Buffer_Zero then
         declare
            View : Byte_View (0 .. Session_Pages * BC.Page_Bytes - 1)
              with Import, Address => BC.Buffer (Disk);
         begin
            View := [others => 0];
            Buffer_Zero := True;
         end;
      end if;
      while OK and then Left > 0 loop
         Serve;
         Step := Unsigned_64'Min (Left, BC.Transfer_Limit (Disk));
         BC.Write (Disk, Next, Step, False, Moved, Result);
         Note_Transfer (Result, OK);
         Next := Next + Step;
         Left := Left - Step;
      end loop;
   end Zero;

   procedure Get (Offset : Byte_Offset; Data : out Bytes; OK : out Boolean) is
      Moved : Unsigned_64;
      Result : BC.Transfer_Result;
   begin
      Data := [others => 0];
      Serve;
      OK := Offset mod Disk_Block = 0 and then Unsigned_64 (Data'Length) mod Disk_Block = 0
        and then Unsigned_64 (Data'Length) / Disk_Block in 1 .. BC.Transfer_Limit (Disk);
      if not OK then
         return;
      end if;
      Buffer_Zero := False;
      BC.Read (Disk, Offset / Disk_Block, Unsigned_64 (Data'Length) / Disk_Block, Moved, Result);
      Note_Transfer (Result, OK);
      if OK then
         declare
            View : constant Byte_View (0 .. Data'Length - 1) with Import, Address => BC.Buffer (Disk);
         begin
            for I in View'Range loop
               Data (Data'First + I) := View (I);
            end loop;
         end;
      end if;
   end Get;

   procedure Flush (OK : out Boolean) is
      Timed_Out : Boolean;
   begin
      Serve;
      BC.Flush (Disk, OK, Timed_Out);
      if Timed_Out then
         Device_Timed_Out := True;
         Log ("the disk's driver did not answer a flush within" &
              Unsigned_64'Image (Device_Request_Deadline_Ms) & " ms");
      end if;
   end Flush;

   procedure Enter (Step : Install_Steps.Step) is
   begin
      Current.Step := To_Wire (Step);
      Log ("step: " & Install_Steps.Name (Step));
      Serve;
   end Enter;

   package Writer is new Layout_Writer
     (Put, Zero, Get, Install_Sources.Read, Install_Sources.Check, Flush, Enter);
   pragma Compile_Time_Error
     (Writer.Batch_Bytes > Session_Pages * BC.Page_Bytes, "a writer batch must fit the session");

   --  Random identifiers: RFC 4122 version 4, stored as GPT stores GUIDs
   --  (Mixed: the first three fields little-endian) or big-endian as ext2's
   --  s_uuid is.
   Version_4 : constant := 16#40#;
   Variant_RFC_4122 : constant := 16#80#;
   procedure Random_GUID (Mixed : Boolean; Result : out GUID; OK : out Boolean) is
      Raw : CuBit.Hardware_Random.Byte_Array (1 .. GUID'Length);
      Version_Byte : constant Positive := (if Mixed then 8 else 7);
   begin
      CuBit.Hardware_Random.Fill (Raw, OK);
      for I in Result'Range loop
         Result (I) := Raw (I);
      end loop;
      Result (Version_Byte) := (Result (Version_Byte) and 16#0F#) or Version_4;
      Result (9) := (Result (9) and 16#3F#) or Variant_RFC_4122;
   end Random_GUID;

   procedure Look
     (Number : IP.Disk_Ordinal; Found : out Boolean;
      Description : out CuBit.Block_Devices.Device_Description;
      Identity : out CuBit.Device_Identities.Identity; Known : out Boolean)
   is
      Opened : constant DIC.Opened_Disk :=
        DIC.Open (Through_Endpoints.Disk_Install, (Disk => Number, Mode => DI.Read_Only), Service_Deadline_Ms);
      Session : BC.Session;
      Result : BC.Open_Result;
      use type BC.Open_Result;
   begin
      Found := Opened.Opened;
      Known := False;
      Description := (others => <>);
      Identity := (others => <>);
      if not Found then
         return;
      end if;
      BC.Open (Opened.Device, Identify_Pages, Device_Request_Deadline_Ms, Session, Result);
      Found := Result = BC.Opened;
      if Found then
         Description := BC.Description (Session);
         BC.Identify (Session, Identity, Known);
         BC.Close (Session);
      end if;
   end Look;

   function Show (Item : CuBit.Device_Identities.Identity_Text) return String is
     (Item.Value (1 .. Item.Length));

   procedure Count (Tree : Install_Tree.Tree; Files, Bytes_Written : in out Unsigned_64) is
   begin
      for E in 1 .. Tree.Count loop
         if Tree.Entries (E).Kind = Install_Tree.Regular_File then
            Files := Files + 1;
            Bytes_Written := Bytes_Written + Tree.Entries (E).Size;
         end if;
      end loop;
   end Count;

   function Source_Outcome return IP.Install_Outcome is
     (case Install_Sources.Last_Failure is
        when Install_Sources.Source_Timed_Out => IP.Source_Timed_Out,
        when Install_Sources.Source_Changed => IP.Source_Changed,
        when others => IP.Source_Unreadable);

   procedure Install
     (Item : IP.Install_Request; Outcome : out IP.Install_Outcome; Proof : out IP.Identity_Proof)
   is
      Found, Known, Ready : Boolean;
      Description : CuBit.Block_Devices.Device_Description;
      Identity : CuBit.Device_Identities.Identity;
      Walked : Install_Sources.Walk_Result;
      Files, Bytes_Written : Unsigned_64 := 0;
      ESP_Files : Install_Tree.Tree renames Install_Sources.Trees (Disk_Layout.ESP_Volume);
      Root_Files : Install_Tree.Tree renames Install_Sources.Trees (Disk_Layout.Root_Volume);
      use type Install_Sources.Walk_Result;
   begin
      Proof := IP.Serial_Number;
      Look (Item.Disk, Found, Description, Identity, Known);
      if not Found then
         Outcome := IP.No_Such_Disk;
         return;
      elsif not Known or else Identity.Model.Length = 0 then
         Outcome := IP.Disk_Unidentified;
         return;
      elsif not CuBit.Device_Identities.Same (Identity.Model, Item.Confirmation.Model)
        or else not CuBit.Device_Identities.Same (Identity.Serial, Item.Confirmation.Serial)
        or else Item.Confirm_Blocks /= Description.blockCount
        or else Item.Confirm_Block_Size /= Description.logicalBlockSize
      then
         Log ("install refused: confirmation names """ & Show (Item.Confirmation.Model) & """/""" &
              Show (Item.Confirmation.Serial) & """" & Item.Confirm_Blocks'Image & " x" &
              Item.Confirm_Block_Size'Image & ", disk" & Item.Disk'Image & " is """ &
              Show (Identity.Model) & """/""" & Show (Identity.Serial) & """" &
              Description.blockCount'Image & " x" & Description.logicalBlockSize'Image);
         Outcome := IP.Confirmation_Mismatch;
         return;
      end if;
      --  A disk without a serial is identified by model and geometry: the
      --  reply says which proof matched, never silently.
      Proof := (if Identity.Serial.Length = 0 then IP.Model_And_Geometry else IP.Serial_Number);
      if Proof = IP.Model_And_Geometry then
         Log ("disk" & Item.Disk'Image & " reports no serial: identified by model and geometry");
      end if;
      if Description.logicalBlockSize not in 512 | 4096 then
         Outcome := IP.Unsupported_Block_Size;
         return;
      end if;
      Install_Sources.Initialize (Through_Endpoints.Filesystem, Ready);
      if not Ready then
         Outcome := IP.Source_Unreadable;
         return;
      end if;
      Install_Sources.Walk (Item.Source (1 .. Item.Length), Walked);
      if Walked /= Install_Sources.Walked then
         if Install_Sources.Last_Failed_Path'Length > 0 then
            Log ("payload: " & Install_Sources.Failure'Image (Install_Sources.Last_Failure) & " at " &
                 Install_Sources.Last_Failed_Path);
         end if;
         Outcome := (case Walked is
                       when Install_Sources.Too_Large => IP.Payload_Too_Large,
                       when Install_Sources.Timed_Out => IP.Source_Timed_Out,
                       when others => IP.Source_Unreadable);
         return;
      end if;
      Count (ESP_Files, Files, Bytes_Written);
      Count (Root_Files, Files, Bytes_Written);
      Current.Files := Unsigned_64'Min (Files, IP.File_Count'Last);
      Current.Bytes := Bytes_Written;
      declare
         Shape : constant Disk_Shape :=
           (Block_Size => Device_Block_Bytes (Description.logicalBlockSize),
            Blocks => Unsigned_64'Min (Description.blockCount, Maximum_Disk_Blocks));
         Layout : Writer.Install_Layout;
         Random_OK : Boolean := True;
         One_OK : Boolean;
         Serial : CuBit.Hardware_Random.Byte_Array (1 .. 4);
         Stamp : CuBit.Clocks.Snapshot;
         Clock_OK : Boolean;
         Opened : DIC.Opened_Disk;
         Result : BC.Open_Result;
         Written : Writer.Write_Result;
         Reached : Install_Steps.Step;
         use type BC.Open_Result;
         use type Writer.Write_Result;
      begin
         Layout.Disk.ESP_Size := Disk_Layout.ESP_MiB (Item.ESP_Size);
         Random_GUID (True, Layout.Disk.Disk_Id, One_OK);  Random_OK := Random_OK and One_OK;
         Random_GUID (True, Layout.Disk.ESP_Id, One_OK);   Random_OK := Random_OK and One_OK;
         Random_GUID (True, Layout.Disk.Root_Id, One_OK);  Random_OK := Random_OK and One_OK;
         Random_GUID (False, Layout.Root.Id, One_OK);      Random_OK := Random_OK and One_OK;
         CuBit.Hardware_Random.Fill (Serial, One_OK);      Random_OK := Random_OK and One_OK;
         if not Random_OK then
            Outcome := IP.Random_Unavailable;
            return;
         end if;
         Layout.ESP_Serial := Unsigned_32 (Serial (1)) or Shift_Left (Unsigned_32 (Serial (2)), 8)
           or Shift_Left (Unsigned_32 (Serial (3)), 16) or Shift_Left (Unsigned_32 (Serial (4)), 24);
         Layout.ESP_Label := "CUBIT ESP  ";
         Layout.Root.Inode_Ratio := Item.Ratio;
         Layout.Root.Name := "cubit-root" & [1 .. 6 => ASCII.NUL];
         Layout.Generation_First := Install_Sources.Generation_First;
         CuBit.Clocks.Read (Through_Endpoints.Clock, Service_Deadline_Ms, Stamp, Clock_OK);
         Layout.Root.Created :=
           (if Clock_OK then Unsigned_32 (Unsigned_64'Min (Stamp.UTC_Seconds, Unsigned_64 (Unsigned_32'Last)))
            else 0);
         if not Fits (Shape, Layout.Disk) then
            Outcome := IP.Disk_Too_Small;
            return;
         end if;
         Opened := DIC.Open (Through_Endpoints.Disk_Install, (Disk => Item.Disk, Mode => DI.Read_Write),
                             Service_Deadline_Ms);
         if not Opened.Opened then
            Outcome := IP.No_Such_Disk;
            return;
         end if;
         BC.Open (Opened.Device, Session_Pages, Device_Request_Deadline_Ms, Disk, Result);
         if Result /= BC.Opened then
            Outcome := (if Result = BC.Describe_Timed_Out then IP.Device_Timed_Out else IP.Device_Failed);
            return;
         end if;
         Disk_Block := Unsigned_64 (Description.logicalBlockSize);
         Buffer_Zero := False;
         Device_Timed_Out := False;
         Log ("installing to disk" & Item.Disk'Image & " """ & Show (Identity.Model) & """/""" &
              Show (Identity.Serial) & """:" & Files'Image & " files (" &
              Natural'Image (Install_Sources.Components) & " components into the storehouse)," &
              Bytes_Written'Image & " bytes, inode ratio" & Item.Ratio'Image);
         Writer.Write_Disk (Shape, Layout, ESP_Files, Root_Files, Written, Reached);
         Install_Sources.Finish;
         BC.Close (Disk);
         Current.Step := To_Wire (Reached);
         Outcome :=
           (case Written is
              when Writer.Written => IP.Installed,
              when Writer.Device_Failed | Writer.Layout_Invalid =>
                (if Device_Timed_Out then IP.Device_Timed_Out else IP.Device_Failed),
              when Writer.Source_Failed => Source_Outcome,
              when Writer.Verify_Failed => IP.Verify_Failed,
              when Writer.ESP_Files_Do_Not_Fit | Writer.Root_Files_Do_Not_Fit => IP.Files_Do_Not_Fit);
         if Written = Writer.Source_Failed and then Install_Sources.Last_Failed_Path'Length > 0 then
            Log ("payload: " & Install_Sources.Failure'Image (Install_Sources.Last_Failure) & " at " &
                 Install_Sources.Last_Failed_Path & " (filesystem.svc, deadline" &
                 Unsigned_64'Image (Install_Sources.Request_Deadline_Ms) & " ms)");
         end if;
         if Written /= Writer.Written then
            Log ("stopped at step: " & Install_Steps.Name (Reached) &
                 "; the disk is not bootable (restart the install)");
         end if;
      end;
   end Install;

   procedure Run (Item : IP.Install_Request; Outcome : out IP.Install_Outcome; Proof : out IP.Identity_Proof) is
   begin
      Current := (State => IP.Running, Step => IP.Checking, others => <>);
      Installing := True;
      Install (Item, Outcome, Proof);
      Installing := False;
      Current :=
        (State => IP.Finished, Step => Current.Step, Outcome => Outcome, Proof => Proof,
         Files => Current.Files, Bytes => Current.Bytes,
         Files_Done => (if Outcome = IP.Installed then Current.Files
                        else Unsigned_64'Min (Install_Sources.Files_Read, Current.Files)),
         Bytes_Done => (if Outcome = IP.Installed then Current.Bytes
                        else Unsigned_64'Min (Install_Sources.Bytes_Read, Current.Bytes)));
   end Run;

   function Outcome_Name (Item : IP.Install_Outcome) return String is
     (case Item is
        when IP.Installed => "installed",
        when IP.No_Such_Disk => "no such disk",
        when IP.Disk_Unidentified => "disk unidentified",
        when IP.Confirmation_Mismatch => "confirmation names another disk",
        when IP.Disk_Too_Small => "disk too small",
        when IP.Unsupported_Block_Size => "unsupported block size",
        when IP.Source_Unreadable => "payload unreadable",
        when IP.Payload_Too_Large => "payload too large",
        when IP.Files_Do_Not_Fit => "files do not fit",
        when IP.Device_Failed => "device failed",
        when IP.Random_Unavailable => "no random source",
        when IP.Bad_Request => "bad request",
        when IP.Verify_Failed => "what was read back differs from what was written",
        when IP.Device_Timed_Out => "the disk's driver did not answer in time",
        when IP.Source_Timed_Out => "the filesystem did not answer a payload read in time",
        when IP.Source_Changed => "a component's bytes are not its recorded SHA-256");
end System_Install;
