------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  installer.svc (SYS-001): lists disks and installs CuBit to one. It
--  alone holds disk-install (startup-approved authority); its clients ask
--  through CuBit.Installer_Protocol. The install itself is the System_Install
--  library (userspace/lib/disk-install): partition, format, fill the
--  storehouse, generation 1, verify, and the ESP's boot sector last
--  (SYS-002). This service only answers the protocol: Install answers at
--  once and the install runs in this loop's place; between the library's
--  device requests the service keeps answering (Install_Status: how far it
--  is, with its step; another Install: Busy; Disk_Info: Reply_Busy while
--  the disk is held). Every step and refusal is logged.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Block_Devices;
with CuBit.Device_Identities;
with CuBit.Grant_References;
with CuBit.Installer_Protocol; use CuBit.Installer_Protocol;
with CuBit.Logging;
with CuBit.Log_Records;
with System_Install;
with Startup;

procedure Main is
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

   --  The first status query an install answers while it runs is logged:
   --  the service answering during an install is visible.
   Answered_While_Installing : Boolean := False;
   procedure Serve;

   package Installs is new System_Install (Serve, Log);

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
      if Number > Unsigned_64 (Disk_Ordinal'Last) then
         Answer (From, Reply_No_Device, [0, 0, 0, 0], 1);
         return;
      end if;
      Installs.Look (Disk_Ordinal (Number), Found, Description, Identity, Known);
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

   --  Install answers at once (Start_Result), then installs; the outcome
   --  is Install_Status's to tell.
   procedure Handle_Install (From : Process_ID; Request : Message) is
      Where : System.Address;
      Valid : Boolean;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Item : Install_Request;
      Outcome : Install_Outcome := Bad_Request;
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
         Log ("install refused or failed: " & Installs.Outcome_Name (Bad_Request));
         Answer (From, Reply_OK, [Unsigned_64 (Start_Result'Enum_Rep (Rejected)), 0, 0, 0], 1);
         return;
      end if;
      Answer (From, Reply_OK, [Unsigned_64 (Start_Result'Enum_Rep (Started)), 0, 0, 0], 1);
      Answered_While_Installing := False;
      Installs.Run (Item, Outcome, Proof);
      Log ("install " & (case Outcome is
                           when Installed => "done",
                           when others => "refused or failed") &
           ": " & Installs.Outcome_Name (Outcome));
   end Handle_Install;

   procedure Handle_Status (From : Process_ID) is
      Now : constant Install_Progress := Installs.Progress;
   begin
      if Installs.Running and then not Answered_While_Installing then
         Answered_While_Installing := True;
         Log ("status answered during the install:" & Now.Files_Done'Image & " of" &
              Now.Files'Image & " files");
      end if;
      Answer (From, Reply_OK, MessageWords (Encode_Status (Now)), Status_Words'Length);
   end Handle_Status;

   procedure Dispatch (From : Process_ID; Request : Message) is
   begin
      if Request.tag.length /= Request_Words then
         Answer (From, Reply_Error, [0, 0, 0, 0], 1);
      elsif Request.tag.label = Install_Status then
         Handle_Status (From);
      elsif Installs.Running and then Request.tag.label = Disk_Info then
         Answer (From, Reply_Busy, [0, 0, 0, 0], 1);
      elsif Installs.Running and then Request.tag.label = Install then
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
   Installs.Initialize
     ((Disk_Install => Startup.Disk_Install, Filesystem => Startup.Filesystem, Clock => Startup.Clock));
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
