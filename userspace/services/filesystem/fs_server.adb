with CuBit.Control_Events;
with CuBit.Filesystems;
with Fs_Server.Access_Control;
with Fs_Server.Client_Queues;
with Fs_Server.Copies;
with Fs_Server.Directory_Requests;
with Fs_Server.File_Requests;
with Fs_Server.Namespace_Requests;
with Fs_Server.Replies;
with Fs_Server.Storage;
with Fs_Server.Watches;

package body Fs_Server is

   procedure Mount_Boot_Image (Address, Size : Unsigned_64)
     renames Storage.mountBootImage;
   procedure Register_Volumes (OK : out Boolean) renames Storage.registerVolumes;
   procedure Attach_Storage (Sender : Process_ID; Msg : Message) renames Storage.handleAttach;
   procedure Service_Client_Queues renames Client_Queues.serviceClientQueues;
   procedure Settle_Watches renames Watches.settleWatches;
   procedure Run_Copies renames Copies.runCopies;
   function Arm_Client_Queues return Boolean renames Client_Queues.armClientQueues;
   function Copies_Active return Boolean renames Copies.copiesActive;
   procedure Commit_Tick renames Storage.commitTick;

   procedure Open_Channel (Sender : Process_ID; Msg : Message)
     renames Client_Queues.handleChannelOpen;
   procedure Close_Client_Queue (Sender : Process_ID)
     renames Client_Queues.releaseClientQueue;

   procedure Grant_Event (Label : Unsigned_32; Msg : Message) is
   begin
      Client_Queues.clientChannelEnded
        (CuBit.Control_Events.Decode
           (Label, Msg.tag.length, Msg.words (0), Msg.words (1), Msg.words (2)));
   end Grant_Event;

   procedure Wake (Sender : Process_ID) renames Client_Queues.handleWake;
   procedure Open_File (Sender : Process_ID; Msg : Message)
     renames File_Requests.handleOpen;

   procedure Read_File (Sender : Process_ID; Msg : Message) is
   begin
      File_Requests.handleRead (Sender, Msg);
   end Read_File;

   procedure Write_File (Sender : Process_ID; Msg : Message) is
   begin
      File_Requests.handleWrite (Sender, Msg);
   end Write_File;

   procedure Positioned_Transfer (Sender : Process_ID; Msg : Message)
     renames File_Requests.handlePositioned;
   procedure Seek_File (Sender : Process_ID; Msg : Message)
     renames File_Requests.handleSeek;
   procedure Close_File (Sender : Process_ID; Msg : Message)
     renames File_Requests.handleClose;
   procedure Flush_File (Sender : Process_ID; Msg : Message)
     renames File_Requests.handleFlush;
   procedure Resize_File (Sender : Process_ID; Msg : Message)
     renames File_Requests.handleResize;
   procedure Open_Directory (Sender : Process_ID; Msg : Message)
     renames Directory_Requests.handleOpenDirectory;
   procedure Read_Directory_Page (Sender : Process_ID; Msg : Message)
     renames Directory_Requests.handleReadDirectoryPage;
   procedure Close_Directory (Sender : Process_ID; Msg : Message)
     renames Directory_Requests.handleCloseDirectory;
   procedure Open_Child_Directory (Sender : Process_ID; Msg : Message)
     renames Directory_Requests.handleOpenChildDirectory;
   procedure Rewind_Directory (Sender : Process_ID; Msg : Message)
     renames Directory_Requests.handleRewindDirectory;
   procedure Set_Access_Profile (Sender : Process_ID; Msg : Message)
     renames Access_Control.handleSetACL;
   procedure Revoke_Access_Profile (Sender : Process_ID; Msg : Message)
     renames Access_Control.handleRevokeACL;
   procedure Release_Owner (Sender : Process_ID; Msg : Message)
     renames Access_Control.handleReleaseOwner;
   procedure Rename (Sender : Process_ID; Msg : Message)
     renames Namespace_Requests.handleRename;
   procedure Unlink (Sender : Process_ID; Msg : Message)
     renames Namespace_Requests.handleUnlink;
   procedure Make_Directory (Sender : Process_ID; Msg : Message)
     renames Namespace_Requests.handleMkdir;
   procedure Remove_Directory (Sender : Process_ID; Msg : Message)
     renames Namespace_Requests.handleRmdir;

   procedure Refuse (Sender : Process_ID) is
   begin
      Replies.sendReply (Sender, CuBit.Filesystems.REPLY_ERR, 0);
   end Refuse;

end Fs_Server;
