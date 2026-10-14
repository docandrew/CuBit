------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The filesystem service's state, shared by its request handlers (the
--  children of this package); main.adb is the IPC adapter in front of them.
--
--  Children, by concern:
--  - Handles (private): the handle table, unlinked-but-held files, versions.
--  - Replies (private): answering a message or a queue entry, client memory.
--  - Delegations (private): read/write delegations, harvesting dirty pages,
--    kept write-back errors.
--  - Watches (private): change notifications and the paths writers report.
--  - Access_Control: per-process file policies and their administration.
--  - Storage: volume admission, startup volumes, volume descriptions, the
--    periodic commit.
--  - File_Requests, Directory_Requests, Namespace_Requests: the requests.
--  - Copies: server-side copies.
--  - Client_Queues: queue channels, wakes, and the request-queue dispatch.
--  The pure parts are proved separately: Fs_Handle_Codes (handle and tag
--  codes, hashes, tokens), Fs_Path_Checks, Fs_Reply_Labels.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Filesystem_Queues;
with CuBit.Channels;
with CuBit.Directory_Paths;
with Cpio;
with Ext2;
with ISO_Records;
with Shared_Objects;
with Volume_List; use Volume_List;
with Volume_Admission; use Volume_Admission;
with Dirty_Runs;
with Queue_Wakes;
with Watch_Reserve;
with Fs_Handle_Codes;

with CuBit.Handles;
package Fs_Server is

   --  What main.adb, the IPC adapter, calls: startup, the service loop's
   --  steps, and one handler per message label.

   --  Startup: the boot image devmgr mapped (0: none) and the volumes.
   procedure Mount_Boot_Image (Address, Size : Unsigned_64);
   procedure Register_Volumes (OK : out Boolean);
   --  devmgr attaches a block driver's endpoint (Fs_Attachments).
   procedure Attach_Storage (Sender : Process_ID; Msg : Message);

   --  The loop's steps: queued requests first, then watches and copies;
   --  block only when nothing waits (Arm_Client_Queues False), without
   --  waiting while copies run, and at most until the next commit.
   procedure Service_Client_Queues;
   procedure Settle_Watches;
   procedure Run_Copies;
   function Arm_Client_Queues return Boolean;
   function Copies_Active return Boolean;
   Commit_Interval_Ms : constant := 5_000;
   procedure Commit_Tick;

   --  Messages (CuBit.Filesystems, CuBit.Channel_Protocol,
   --  CuBit.Filesystem_Queues).
   procedure Open_Channel (Sender : Process_ID; Msg : Message);
   procedure Close_Client_Queue (Sender : Process_ID);
   procedure Grant_Event (Label : Unsigned_32; Msg : Message);
   procedure Wake (Sender : Process_ID);
   procedure Open_File (Sender : Process_ID; Msg : Message);
   procedure Read_File (Sender : Process_ID; Msg : Message);
   procedure Write_File (Sender : Process_ID; Msg : Message);
   procedure Positioned_Transfer (Sender : Process_ID; Msg : Message);
   procedure Seek_File (Sender : Process_ID; Msg : Message);
   procedure Close_File (Sender : Process_ID; Msg : Message);
   procedure Flush_File (Sender : Process_ID; Msg : Message);
   procedure Resize_File (Sender : Process_ID; Msg : Message);
   procedure Open_Directory (Sender : Process_ID; Msg : Message);
   procedure Read_Directory_Page (Sender : Process_ID; Msg : Message);
   procedure Close_Directory (Sender : Process_ID; Msg : Message);
   procedure Open_Child_Directory (Sender : Process_ID; Msg : Message);
   procedure Rewind_Directory (Sender : Process_ID; Msg : Message);
   procedure Set_Access_Profile (Sender : Process_ID; Msg : Message);
   procedure Revoke_Access_Profile (Sender : Process_ID; Msg : Message);
   procedure Release_Owner (Sender : Process_ID; Msg : Message);
   procedure Rename (Sender : Process_ID; Msg : Message);
   procedure Unlink (Sender : Process_ID; Msg : Message);
   procedure Make_Directory (Sender : Process_ID; Msg : Message);
   procedure Remove_Directory (Sender : Process_ID; Msg : Message);
   --  A message the service does not take: REPLY_ERR.
   procedure Refuse (Sender : Process_ID);

private
   package Codes renames Fs_Handle_Codes;
   package FQ renames CuBit.Filesystem_Queues;
   package FQueues renames CuBit.Filesystem_Queues.Queues;

   --  Maximum open files. Handle slots below EXTENDED_HANDLES also carry
   --  directory and optical-file state (extras); regular files use the
   --  rest, so a large table costs a few words per slot.
   MAX_OPEN_FILES : constant := Codes.Maximum_Handles;
   --  Handles one client may hold on one file at once (of the file's
   --  Open_Inodes.Max_Holders).
   MAX_HANDLES_PER_OWNER : constant := 8;
   EXTENDED_HANDLES : constant := Codes.Extended_Handles;
   subtype Handle_Slot is Natural range 0 .. MAX_OPEN_FILES - 1;

   --  Filesystem format is independent of the block provider.
   type Filesystem_Kind is (CPIO_ARCHIVE, ISO_FILESYSTEM, EXT2_FILESYSTEM);

   type Inode_Identity is record
      volume : Volume_Index;
      number : Unsigned_32;
   end record;
   function identityHash (Key : Inode_Identity) return Unsigned_32 is
     (Codes.Identity_Hash (Key.volume, Key.number));
   package Open_Inodes is new Shared_Objects
     (Capacity => MAX_OPEN_FILES, Object_Key => Inode_Identity,
      Empty_Key => (Volume_Index'First, 0), Object_Value => Ext2.Inode,
      Empty_Value => Ext2.NULL_INODE, Hash => identityHash);
   inodeObjects : Open_Inodes.State;

   --  The volume-and-inode identity of an open object (Fs_Handle_Codes).
   function objectOf (key : Inode_Identity) return Unsigned_64 is
     (Codes.Object_Id (key.volume, key.number));

   type Open_Object_Kind is (FILE_OBJECT, DIRECTORY_OBJECT);

   --  File handles retain format, volume identity, ownership and rights.
   type FileEntry is record
      active      : Boolean      := False;
      retired     : Boolean      := False;
      generation  : Codes.Generation := Codes.First_Generation;
      filesystemKind     : Filesystem_Kind  := CPIO_ARCHIVE;
      volume      : Volume_Index := Volume_Index'First;
      inodeNum    : Unsigned_32  := 0;      --  ext2 only
      cpioFileIdx : Natural      := 0;      --  cpio only
      offset      : Unsigned_64  := 0;
      ownerPID    : Process_ID    := No_Process;
      openRights  : Unsigned_8   := 0;
      objectKind  : Open_Object_Kind := FILE_OBJECT;
   end record;

   type FileTable is array (Handle_Slot) of FileEntry;
   files : FileTable;

   --  Directory and optical-file handles (slots below EXTENDED_HANDLES).
   type Extra_Entry is record
      directoryInode : Ext2.Inode; -- Directory enumeration snapshot only.
      opticalFile : ISO_Records.File_Record;
      directoryPath : CuBit.Directory_Paths.Path;
   end record;
   extras : array (0 .. EXTENDED_HANDLES - 1) of Extra_Entry;

   --  CPIO ramdisk archive
   cpioArchive : Cpio.Archive;
   cpioOk      : Boolean := False;

   --  Append-only identities and per-volume storage sessions. No slot reuse
   --  while handles or cached metadata can refer to an entry.
   Volumes : Volume_List.State;
   type Volume_Context is record
      Status : Admission_Result := Provider_Not_Ready;
      Fs : Ext2.Filesystem;
      Buffer : System.Address := System.Null_Address;
      Grant : CuBit.Memory_Grants.Grant_Reference;
   end record;
   Contexts : array (Volume_Index) of Volume_Context;
   Default_Write_Volume : Volume_Reference := No_Volume;

   --  Access rights (CuBit.File_Access wire rights).
   ACL_READ   : constant Unsigned_8 := 1;
   ACL_WRITE  : constant Unsigned_8 := 2;
   ACL_CREATE : constant Unsigned_8 := 8;

   function toAddr is new Ada.Unchecked_Conversion (Unsigned_64, System.Address);

   ---------------------------------------------------------------------------
   --  Files unlinked while handles held them (handleUnlink): freed when the
   --  last handle goes (POSIX). Each needs a live handle, so MAX_OPEN_FILES
   --  entries always suffice.
   ---------------------------------------------------------------------------
   type Inode_Key_Array is array (Handle_Slot) of Inode_Identity;
   NO_ORPHAN : constant Inode_Identity := (Volume_Index'First, 0);
   orphans : Inode_Key_Array := [others => NO_ORPHAN];
   orphanCount : Natural range 0 .. MAX_OPEN_FILES := 0;

   ---------------------------------------------------------------------------
   --  Client request queues (CuBit.Filesystem_Queues,
   --  docs/filesystem-data-plane.md): a client lends a queue pair and a
   --  transfer arena once; each request is then an entry, handled by the
   --  same handler as its message twin, with its data in the arena and its
   --  answer on the queue.
   ---------------------------------------------------------------------------
   MAX_CLIENT_QUEUES : constant := 16;
   subtype Client_Queue_Count is Natural range 0 .. MAX_CLIENT_QUEUES;
   subtype Client_Queue_Index is Client_Queue_Count range 1 .. MAX_CLIENT_QUEUES;
   No_Client_Queue : constant Client_Queue_Count := 0;
   --  Saved replies for held wake requests (FQ.OP_FS_WAKE), one per queue.
   wakeReplies : array (Client_Queue_Index) of CuBit.Handles.Reply :=
     [others => CuBit.Handles.No_Reply];

   --  A client's queue: three channels it opened (FQ: the queue pair, the
   --  transfer arena, the dirty arena), docs/data-plane.md.
   type Client_Queue is record
      owner      : Process_ID := No_Process;   --  set once the queue pair is open
      queueLink  : CuBit.Channels.Channel;
      transferLink : CuBit.Channels.Channel;
      dirtyLink  : CuBit.Channels.Channel;
      clientBase : System.Address := System.Null_Address;   --  its region, read-only
      serverBase : System.Address := System.Null_Address;   --  ours
      arena      : System.Address := System.Null_Address;
      arenaBytes : Unsigned_64 := 0;
      server     : FQueues.Server;
      wake       : Queue_Wakes.State := Queue_Wakes.Idle;   --  a held wake request
      --  The event ring (FQ.Event_Connector), if the client opened one, and
      --  how many of its watches are Watching (Watch_Reserve).
      eventLink  : CuBit.Channels.Channel;
      watching   : Watch_Reserve.Normal_Count := 0;
      --  The dirty arena (FQ.Dirty_*), if the client lent one.
      dirty      : System.Address := System.Null_Address;
   end record;
   clientQueues : array (Client_Queue_Index) of Client_Queue;

   --  While a queue entry is handled: where its answer goes, and the arena
   --  range it names (checked against the arena before it is set).
   type Reply_Route is record
      queue : Client_Queue_Count := No_Client_Queue;
      token : FQueues.Token := 0;
   end record;
   curRoute : Reply_Route;
   curArena : System.Address := System.Null_Address;
   curArenaBytes : Unsigned_64 := 0;

   --  A word of the client's region (it writes them) or of ours.
   function clientWord (q : Client_Queue_Index; offset : Natural) return System.Address is
     (clientQueues (q).clientBase + Storage_Offset (offset));
   function serverWord (q : Client_Queue_Index; offset : Natural) return System.Address is
     (clientQueues (q).serverBase + Storage_Offset (offset));

   --  The namespace generation (FQ.Server_Namespace_At), published in
   --  every client's queue page. It moves on before any change that could
   --  alter what a name resolves to, or whether a client may open it, so a
   --  client never reuses a parked handle across such a change.
   namespaceGeneration : Unsigned_32 := 1;

   ---------------------------------------------------------------------------
   --  Read delegations (FQ.Delegations_At): a handle whose client may answer
   --  reads from its own cache while no other handle can write the file.
   --  Every change to a file revokes the other handles' delegations first
   --  and moves the file's version on.
   ---------------------------------------------------------------------------
   pragma Compile_Time_Error
     (MAX_OPEN_FILES > FQ.Maximum_Delegations,
      "every handle slot needs a delegation entry");
   delegated : array (Handle_Slot) of Boolean := [others => False];
   --  The opener's policy lets it read the file, whatever the handle's own
   --  rights: the handle may be parked for a later read-only open of the
   --  same name (FQ.Queue_Park), which then gives it read rights alone.
   parkable : array (Handle_Slot) of Boolean := [others => False];
   --  The delegation lets the client buffer writes (FQ.Write_Delegation).
   writeDelegated : array (Handle_Slot) of Boolean := [others => False];
   --  A write-back of the handle's dirty pages failed: reported by its
   --  next flush or close (as Linux reports write-back errors on fsync).
   writebackFailed : array (Handle_Slot) of Boolean := [others => False];

   --  Bytes one harvest run, one write slice and one copy slice move
   --  through service memory (planned by Dirty_Runs, proved).
   HARVEST_BUFFER_BYTES : constant := Dirty_Runs.Buffer_Bytes;
   type Byte_Buffer is array (0 .. HARVEST_BUFFER_BYTES - 1) of Unsigned_8;
   harvestBuffer : Byte_Buffer with Alignment => FQ.Dirty_Page_Bytes;

   pragma Compile_Time_Error
     (MAX_OPEN_FILES > 2 ** FQ.Tag_Slot_Bits, "a dirty-entry tag must hold every slot");
   pragma Compile_Time_Error
     (Codes.Tag_Slot_Bits /= FQ.Tag_Slot_Bits or else
      Codes.Tag_Generation_Bits /= FQ.Tag_Generation_Bits,
      "Fs_Handle_Codes tags must match the dirty arena's");

end Fs_Server;
