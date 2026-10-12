------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace filesystem server: the IPC adapter.
--
--  Serves files of the bootstrap archive, the optical disc and ext2/ext3
--  volumes. This procedure starts the service and runs its loop: request
--  queues first, then messages, each label handed to its handler in
--  Fs_Server (state and handlers, by concern, in its children).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Filesystem_Queues;
with CuBit.Channel_Protocol;
with CuBit.Control_Events;
with CuBit.Busy_Poll;
with Fs_Server;

procedure main is
   use ASCII;

   sender : Process_ID;
   msg    : Message;
   rdAddr : Unsigned_64;
   rdSize : Unsigned_64;
   nextCommit : Unsigned_64 := 0;
   received   : Boolean := False;
   volumesOk  : Boolean;
begin
   debugPrint ("FS Server: Starting..." & LF);

   --  The boot image devmgr mapped for us, from its launch configuration.
   declare
      Configuration : Launch_Configuration.Boot_Image_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get (Configuration, Configured);
      rdSize := (if Configured then Configuration.Boot_Image else 0);
      rdAddr := (if rdSize = 0 then 0 else Launch_Configuration.Boot_Image_Address);
   end;
   Fs_Server.Mount_Boot_Image (rdAddr, rdSize);

   --  Bootstrap bindings only. File operations never test these driver roles.
   Fs_Server.Register_Volumes (volumesOk);
   if not volumesOk then
      return;
   end if;

   --  Register as DRIVER_FS so other services can discover us
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_FS);
   end;

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      ignore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, reserved => 0),
          authorityTag => 0,
          words    => [others => 0]), CuBit.Messages.Wait_Forever);
   end;

   CuBit.Busy_Poll.Calibrate;   --  queue polling windows are timed by the TSC
   debugPrint ("FS Server: Entering message loop." & LF);

   --  Main IPC message loop.
   --  Uses blocking receive for lowest latency.  Future async work
   --  can switch to Poll_Service_Request + Poll_Completion when capSubmit-based
   --  driver I/O is implemented.
   nextCommit := syscall (SYSCALL_GETTIME) + Fs_Server.Commit_Interval_Ms;
   loop
      --  Queued requests first; block only when none wait, and at most
      --  until the next journal commit.
      Fs_Server.Service_Client_Queues;
      Fs_Server.Settle_Watches;
      Fs_Server.Run_Copies;
      if not Fs_Server.Arm_Client_Queues then
      --  Copies in progress: look for messages without waiting.
      receiveUntil ((if Fs_Server.Copies_Active then 0 else nextCommit), sender, msg, received);
      if received then
      case msg.tag.label is
         when CuBit.Channel_Protocol.OP_OPEN_PRODUCING
            | CuBit.Channel_Protocol.OP_OPEN_CONSUMING =>   --  consuming: the event ring
            Fs_Server.Open_Channel (sender, msg);
         when CuBit.Channel_Protocol.OP_KICK =>
            null;   --  one-way: the queue is serviced at the loop's top
         when CuBit.Channel_Protocol.OP_CLOSE =>
            --  One-way: a client let go of its queue.
            Fs_Server.Close_Client_Queue (sender);
         when CuBit.Control_Events.Grant_Revoked_Label
            | CuBit.Control_Events.Grant_Returned_Label =>
            --  Only the kernel posts these (it refuses them from a process).
            if sender = No_Process then
               Fs_Server.Grant_Event (msg.tag.label, msg);
            end if;
         when CuBit.Filesystem_Queues.OP_FS_WAKE =>
            Fs_Server.Wake (sender);
         when OP_OPEN =>
            Fs_Server.Open_File (sender, msg);
         when OP_READ =>
            Fs_Server.Read_File (sender, msg);
         when OP_WRITE =>
            Fs_Server.Write_File (sender, msg);
         when OP_READ_AT | OP_WRITE_AT =>
            Fs_Server.Positioned_Transfer (sender, msg);
         when OP_SEEK =>
            Fs_Server.Seek_File (sender, msg);
         when OP_CLOSE =>
            Fs_Server.Close_File (sender, msg);
         when OP_FLUSH_FILE =>
            Fs_Server.Flush_File (sender, msg);
         when OP_RESIZE_FILE =>
            Fs_Server.Resize_File (sender, msg);
         when OP_OPEN_DIRECTORY =>
            Fs_Server.Open_Directory (sender, msg);
         when OP_READ_DIRECTORY_PAGE =>
            Fs_Server.Read_Directory_Page (sender, msg);
         when OP_CLOSE_DIRECTORY =>
            Fs_Server.Close_Directory (sender, msg);
         when OP_OPEN_CHILD_DIRECTORY =>
            Fs_Server.Open_Child_Directory (sender, msg);
         when OP_REWIND_DIRECTORY =>
            Fs_Server.Rewind_Directory (sender, msg);
         when OP_SET_ACL =>
            Fs_Server.Set_Access_Profile (sender, msg);
         when OP_REVOKE_ACL =>
            Fs_Server.Revoke_Access_Profile (sender, msg);
         when OP_RELEASE_OWNER =>
            Fs_Server.Release_Owner (sender, msg);
         when OP_RENAME =>
            Fs_Server.Rename (sender, msg);
         when OP_UNLINK =>
            Fs_Server.Unlink (sender, msg);
         when OP_MKDIR =>
            Fs_Server.Make_Directory (sender, msg);
         when OP_RMDIR =>
            Fs_Server.Remove_Directory (sender, msg);
         when others =>
            Fs_Server.Refuse (sender);
      end case;
      end if;
      end if;
      if syscall (SYSCALL_GETTIME) >= nextCommit then
         Fs_Server.Commit_Tick;
         nextCommit := syscall (SYSCALL_GETTIME) + Fs_Server.Commit_Interval_Ms;
      end if;
   end loop;
end main;
