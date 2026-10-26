------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  installer.svc (SYS-001): lists disks and installs CuBit to one. It
--  alone holds disk-install (startup-approved authority); its clients ask
--  through CuBit.Installer_Protocol. An install happens only when the
--  request's confirmation equals the target disk's identity, model and
--  serial both, read from the disk itself just before. Then:
--    GPT, ESP (FAT32 with the payload's esp/ tree: the boot files at
--    \EFI\BOOT\BOOTX64.EFI, the UEFI fallback path) and root (ext2 with
--    the payload's root/ tree: the image closure), written in that order
--    with the GPT last, then a device flush. GUIDs, the filesystem UUID
--    and the ESP serial come from the CPU's random source.
--  Every step and refusal is logged. Install answers at once and the
--  install runs in this loop's place; between the writer's device writes
--  the service keeps answering (Install_Status: how far it is; another
--  Install: Busy; Disk_Info: Reply_Busy while the disk is held).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Block_Devices;
with CuBit.Block_Client;
with CuBit.Clocks;
with CuBit.Device_Identities;
with CuBit.Disk_Install;
with CuBit.Disk_Install.Client;
with CuBit.Grant_References;
with CuBit.Hardware_Random;
with CuBit.Installer_Protocol; use CuBit.Installer_Protocol;
with CuBit.Logging;
with CuBit.Log_Records;
with Disk_Layout; use Disk_Layout;
with Install_Tree;
with Layout_Writer;
with Installer_Payload;
with Startup;

procedure Main is
   package BC renames CuBit.Block_Client;
   package DIC renames CuBit.Disk_Install.Client;
   package DI renames CuBit.Disk_Install;
   use type Install_Tree.Entry_Kind;

   Logger : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);

   procedure Log (Text : String) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make ("installer: " & Text);
      Submitted : Boolean;
   begin
      debugPrint ("installer: " & Text & ASCII.LF);
      if Made.Success then
         CuBit.Logging.Emit (Logger, Made.Value, Submitted);
      end if;
   end Log;

   --  Sessions move up to a writer batch at a time.
   Session_Pages : constant BC.Page_Count := 16;
   Identify_Pages : constant BC.Page_Count := 1;

   Disk : BC.Session;
   Disk_Block : Unsigned_64 := 1;
   Buffer_Zero : Boolean := False;

   type Byte_View is array (Natural range <>) of Unsigned_8;

   --  The current or last install; Installing while one runs. Serve
   --  answers requests between the writer's device writes, so the
   --  install's client and others hear back while it runs.
   Current : Install_Progress;
   Installing : Boolean := False;
   --  The first status query an install answers while it runs is logged:
   --  the service answering during an install is visible.
   Answered_While_Installing : Boolean := False;
   procedure Serve;

   procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean) is
      View : Byte_View (0 .. Data'Length - 1) with Import, Address => BC.Buffer (Disk);
      Moved : Unsigned_64;
      Result : BC.Transfer_Result;
      use type BC.Transfer_Result;
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
      BC.Write (Disk, Offset / Disk_Block, Unsigned_64 (Data'Length) / Disk_Block, False,
                Moved, Result);
      OK := Result = BC.Transferred;
   end Put;

   procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean) is
      Next : Unsigned_64 := Offset / Disk_Block;
      Left : Unsigned_64 := Length / Disk_Block;
      Step, Moved : Unsigned_64;
      Result : BC.Transfer_Result;
      use type BC.Transfer_Result;
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
         OK := Result = BC.Transferred;
         Next := Next + Step;
         Left := Left - Step;
      end loop;
   end Zero;

   package Writer is new Layout_Writer (Put, Zero, Installer_Payload.Read);
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

   --  Opens Number read-only and describes and identifies it.
   procedure Look
     (Number : DI.Disk_Ordinal; Found : out Boolean; Description : out CuBit.Block_Devices.Device_Description;
      Identity : out CuBit.Device_Identities.Identity; Known : out Boolean)
   is
      Opened : constant DIC.Opened_Disk :=
        DIC.Open (Startup.Disk_Install, (Disk => Number, Mode => DI.Read_Only));
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
      BC.Open (Opened.Device, Identify_Pages, Session, Result);
      Found := Result = BC.Opened;
      if Found then
         Description := BC.Description (Session);
         BC.Identify (Session, Identity, Known);
         BC.Close (Session);
      end if;
   end Look;

   function Show (Item : CuBit.Device_Identities.Identity_Text) return String is
     (Item.Value (1 .. Item.Length));

   procedure Answer (To : Process_ID; Label : Unsigned_32; Words : MessageWords; Length : Unsigned_8) is
      Ignore : Unsigned_64;
   begin
      Ignore := reply (To, (tag => (label => Label, length => Length, protocol => 0, ipc => <>,
                                    reserved => 0),
                            authorityTag => 0, words => Words));
   end Answer;

   --  The client's page, acquired for the request. Valid False: refused.
   procedure Borrow
     (From : Process_ID; Slot, Generation : Unsigned_64; Writable : Boolean;
      Where : out System.Address; Valid : out Boolean;
      Reference : out CuBit.Memory_Grants.Grant_Reference) is
   begin
      Where := System.Null_Address;
      Reference := (slot => 0, generation => 1);
      Valid := Slot <= CuBit.Grant_References.Maximum_Slot and then Generation /= 0
        and then Generation <= CuBit.Grant_References.Maximum_Generation;
      if not Valid then
         return;
      end if;
      Reference := (slot => Slot, generation => Generation);
      CuBit.Memory_Grants.Acquire
        (Reference, From, 0, Page_Bytes,
         (if Writable then CuBit.Memory_Grants.Write_Access else CuBit.Memory_Grants.Read_Access),
         Where, Valid);
   end Borrow;

   procedure Give_Back (Reference : CuBit.Memory_Grants.Grant_Reference) is
      Ignore : Boolean;
   begin
      CuBit.Memory_Grants.Return_Acquisition (Reference, Ignore);
   end Give_Back;

   procedure Handle_Disk_Info (From : Process_ID; Request : Message) is
      Number : constant Unsigned_64 := Request.words (0);
      Where : System.Address;
      Valid, Found, Known : Boolean;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Description : CuBit.Block_Devices.Device_Description;
      Identity : CuBit.Device_Identities.Identity;
   begin
      if Number > Unsigned_64 (DI.Disk_Ordinal'Last) then
         Answer (From, Reply_No_Device, [0, 0, 0, 0], 1);
         return;
      end if;
      Look (DI.Disk_Ordinal (Number), Found, Description, Identity, Known);
      if not Found then
         Answer (From, Reply_No_Device, [0, 0, 0, 0], 1);
         return;
      end if;
      Borrow (From, Request.words (1), Request.words (2), True, Where, Valid, Reference);
      if not Valid then
         Answer (From, Reply_Error, [0, 0, 0, 0], 1);
         return;
      end if;
      declare
         Target : Page with Import, Address => Where;
      begin
         Target := Encode_Disk
           ((Blocks => Description.blockCount,
             Block_Size => Description.logicalBlockSize,
             Identity => Identity));
      end;
      Give_Back (Reference);
      Answer (From, Reply_OK, [Number, 0, 0, 0], 1);
   end Handle_Disk_Info;

   ESP_Files : Install_Tree.Tree renames Installer_Payload.Trees (Disk_Layout.ESP_Volume);
   Root_Files : Install_Tree.Tree renames Installer_Payload.Trees (Disk_Layout.Root_Volume);

   procedure Count (Tree : Install_Tree.Tree; Files, Bytes_Written : in out Unsigned_64) is
   begin
      for E in 1 .. Tree.Count loop
         if Tree.Entries (E).Kind = Install_Tree.Regular_File then
            Files := Files + 1;
            Bytes_Written := Bytes_Written + Tree.Entries (E).Size;
         end if;
      end loop;
   end Count;

   procedure Run_Install
     (Item : Install_Request; Outcome : out Install_Outcome; Files, Bytes_Written : out Unsigned_64;
      Proof : out Identity_Proof)
   is
      Found, Known, Ready : Boolean;
      Description : CuBit.Block_Devices.Device_Description;
      Identity : CuBit.Device_Identities.Identity;
      Walked : Installer_Payload.Walk_Result;
      use type Installer_Payload.Walk_Result;
   begin
      Files := 0;
      Bytes_Written := 0;
      Proof := Serial_Number;
      Look (Item.Disk, Found, Description, Identity, Known);
      if not Found then
         Outcome := No_Such_Disk;
         return;
      elsif not Known or else Identity.Model.Length = 0 then
         Outcome := Disk_Unidentified;
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
         Outcome := Confirmation_Mismatch;
         return;
      end if;
      --  A disk without a serial is identified by model and geometry: the
      --  reply says which proof matched, never silently.
      Proof := (if Identity.Serial.Length = 0 then Model_And_Geometry else Serial_Number);
      if Proof = Model_And_Geometry then
         Log ("disk" & Item.Disk'Image & " reports no serial: identified by model and geometry");
      end if;
      if Description.logicalBlockSize not in 512 | 4096 then
         Outcome := Unsupported_Block_Size;
         return;
      end if;
      Installer_Payload.Initialize (Ready);
      if not Ready then
         Outcome := Source_Unreadable;
         return;
      end if;
      Installer_Payload.Walk (Item.Source (1 .. Item.Length), Walked);
      if Walked /= Installer_Payload.Walked then
         Outcome := (if Walked = Installer_Payload.Too_Large then Payload_Too_Large
                     else Source_Unreadable);
         return;
      end if;
      Count (ESP_Files, Files, Bytes_Written);
      Count (Root_Files, Files, Bytes_Written);
      Current.Files := Unsigned_64'Min (Files, File_Count'Last);
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
         Durable : Boolean;
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
            Outcome := Random_Unavailable;
            return;
         end if;
         Layout.ESP_Serial := Unsigned_32 (Serial (1)) or Shift_Left (Unsigned_32 (Serial (2)), 8)
           or Shift_Left (Unsigned_32 (Serial (3)), 16) or Shift_Left (Unsigned_32 (Serial (4)), 24);
         Layout.ESP_Label := "CUBIT ESP  ";
         Layout.Root.Inode_Ratio := Item.Ratio;
         Layout.Root.Name := "cubit-root" & [1 .. 6 => ASCII.NUL];
         CuBit.Clocks.Read (Startup.Clock, Stamp, Clock_OK);
         Layout.Root.Created :=
           (if Clock_OK then Unsigned_32 (Unsigned_64'Min (Stamp.UTC_Seconds, Unsigned_64 (Unsigned_32'Last)))
            else 0);
         if not Fits (Shape, Layout.Disk) then
            Outcome := Disk_Too_Small;
            return;
         end if;
         Opened := DIC.Open (Startup.Disk_Install, (Disk => Item.Disk, Mode => DI.Read_Write));
         if not Opened.Opened then
            Outcome := No_Such_Disk;
            return;
         end if;
         BC.Open (Opened.Device, Session_Pages, Disk, Result);
         if Result /= BC.Opened then
            Outcome := Device_Failed;
            return;
         end if;
         Disk_Block := Unsigned_64 (Description.logicalBlockSize);
         Buffer_Zero := False;
         Log ("installing to disk" & Item.Disk'Image & " """ & Show (Identity.Model) & """/""" &
              Show (Identity.Serial) & """:" & Files'Image & " files," & Bytes_Written'Image &
              " bytes, inode ratio" & Item.Ratio'Image);
         Writer.Write_Disk (Shape, Layout, ESP_Files, Root_Files, Written);
         Installer_Payload.Finish;
         BC.Flush (Disk, Durable);
         BC.Close (Disk);
         Outcome :=
           (case Written is
              when Writer.Written => (if Durable then Installed else Device_Failed),
              when Writer.Device_Failed | Writer.Layout_Invalid => Device_Failed,
              when Writer.Source_Failed => Source_Unreadable,
              when Writer.ESP_Files_Do_Not_Fit | Writer.Root_Files_Do_Not_Fit => Files_Do_Not_Fit);
      end;
   end Run_Install;

   --  The runtime keeps no enumeration images: the names, spelled out.
   function Outcome_Name (Item : Install_Outcome) return String is
     (case Item is
        when Installed => "installed",
        when No_Such_Disk => "no such disk",
        when Disk_Unidentified => "disk unidentified",
        when Confirmation_Mismatch => "confirmation names another disk",
        when Disk_Too_Small => "disk too small",
        when Unsupported_Block_Size => "unsupported block size",
        when Source_Unreadable => "payload unreadable",
        when Payload_Too_Large => "payload too large",
        when Files_Do_Not_Fit => "files do not fit",
        when Device_Failed => "device failed",
        when Random_Unavailable => "no random source",
        when Bad_Request => "bad request");

   --  Install answers at once (Start_Result), then installs; the outcome
   --  is Install_Status's to tell.
   procedure Handle_Install (From : Process_ID; Request : Message) is
      Where : System.Address;
      Valid : Boolean;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Item : Install_Request;
      Outcome : Install_Outcome := Bad_Request;
      Files, Bytes_Written : Unsigned_64 := 0;
      Proof : Identity_Proof := Serial_Number;
   begin
      Borrow (From, Request.words (0), Request.words (1), False, Where, Valid, Reference);
      if Valid then
         declare
            Source : constant Page with Import, Address => Where;
            Copy : constant Page := Source;   --  copied, then checked
         begin
            Give_Back (Reference);
            Decode_Install (Copy, Item, Valid);
         end;
      end if;
      if not Valid then
         Log ("install refused or failed: " & Outcome_Name (Bad_Request));
         Answer (From, Reply_OK, [Unsigned_64 (Start_Result'Enum_Rep (Rejected)), 0, 0, 0], 1);
         return;
      end if;
      Answer (From, Reply_OK, [Unsigned_64 (Start_Result'Enum_Rep (Started)), 0, 0, 0], 1);
      Current := (State => Running, others => <>);
      Answered_While_Installing := False;
      Installing := True;
      Run_Install (Item, Outcome, Files, Bytes_Written, Proof);
      Installing := False;
      Current :=
        (State => Finished, Outcome => Outcome, Proof => Proof,
         Files => Current.Files, Bytes => Current.Bytes,
         Files_Done => (if Outcome = Installed then Current.Files
                        else Unsigned_64'Min (Installer_Payload.Files_Read, Current.Files)),
         Bytes_Done => (if Outcome = Installed then Current.Bytes
                        else Unsigned_64'Min (Installer_Payload.Bytes_Read, Current.Bytes)));
      Log ("install " & (case Outcome is
                           when Installed => "done",
                           when others => "refused or failed") &
           ": " & Outcome_Name (Outcome));
   end Handle_Install;

   procedure Handle_Status (From : Process_ID) is
      Now : Install_Progress := Current;
   begin
      if Installing then
         Now.Files_Done := Unsigned_64'Min (Installer_Payload.Files_Read, Now.Files);
         Now.Bytes_Done := Unsigned_64'Min (Installer_Payload.Bytes_Read, Now.Bytes);
         if not Answered_While_Installing then
            Answered_While_Installing := True;
            Log ("status answered during the install:" & Now.Files_Done'Image & " of" &
                 Now.Files'Image & " files");
         end if;
      end if;
      Answer (From, Reply_OK, MessageWords (Encode_Status (Now)), Status_Words'Length);
   end Handle_Status;

   procedure Dispatch (From : Process_ID; Request : Message) is
   begin
      if Request.tag.length /= Request_Words then
         Answer (From, Reply_Error, [0, 0, 0, 0], 1);
      elsif Request.tag.label = Install_Status then
         Handle_Status (From);
      elsif Installing and then Request.tag.label = Disk_Info then
         Answer (From, Reply_Busy, [0, 0, 0, 0], 1);
      elsif Installing and then Request.tag.label = Install then
         Answer (From, Reply_OK, [Unsigned_64 (Start_Result'Enum_Rep (Busy)), 0, 0, 0], 1);
      elsif Request.tag.label = Disk_Info then
         Handle_Disk_Info (From, Request);
      elsif Request.tag.label = Install then
         Handle_Install (From, Request);
      else
         Answer (From, Reply_Error, [0, 0, 0, 0], 1);
      end if;
   end Dispatch;

   procedure Serve is
      From : Process_ID;
      Request : Message;
      Found : Boolean;
   begin
      loop
         Poll_Any_Ipc (From, Request, Found);
         exit when not Found;
         Dispatch (From, Request);
      end loop;
   end Serve;

   From : Process_ID;
   Request : Message;
   Found : Boolean;
   Ignore : Unsigned_64;
   Ignored_Activity : Activity_Result;
begin
   Ignore := registerDriver (Service_Role, Startup.Registration);
   Log ("registered");
   loop
      Poll_Any_Ipc (From, Request, Found);
      if not Found then
         Ignored_Activity := Wait_For_Activity_Until (Wait_Forever);
      else
         Dispatch (From, Request);
      end if;
   end loop;
end Main;
