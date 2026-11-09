------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What an install writes and where its bytes come from (SYS-001 installer
--  v2, SYS-003), read through filesystem.svc with an explicit deadline on
--  every request.
--
--  @description
--  The payload is a directory holding esp/ (the boot files), root/ (the
--  image closure) and INDEX, one line per file:
--
--    <volume>/<path> [TAB <where> [TAB <field>]...]
--
--  <volume> is esp or root. <where> names the bytes: empty, they are in the
--  payload at <volume>/<path>; a path below the medium's root (the live
--  image's own files are not stored twice); or a whole CuBit name
--  ("@mem:0/..."). Fields: "executable" (mode 0755) and "sha256=<hex>", the
--  file's SHA-256 as the image build computed it: such a file is a
--  component, and the install also stores it by content in the root's
--  storehouse (root files under storehouse/executables/<sha256>, boot files
--  under storehouse/files/<sha256>), the way ccl-build's Storehouse names
--  entries. "root/<path>/" alone is a directory (an empty one).
--
--  Walk builds the two trees from INDEX, then adds the storehouse entries
--  and, last of the root tree, the first system generation:
--  storehouse/profiles/system/1 (System_Generation number 1, previous 0,
--  no pins: the installed image's programs run) and .../current naming it.
--  The live system's own generations (INDEX lines under
--  storehouse/profiles/system/) do not carry over: an installed system
--  starts at generation 1.
--
--  Read gives the layout writer each file's bytes and hashes them as they
--  go; a component whose bytes are not its INDEX SHA-256 fails the read
--  (Source_Changed). Check hashes the bytes read back from the disk and
--  compares them with what Read gave. Every filesystem request has
--  Request_Deadline_Ms; one past it fails with Source_Timed_Out, naming
--  the request (Last_Failure).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles;
with Disk_Layout;
with Install_Tree;

package Install_Sources is

   --  One filesystem request (open, a 64 KiB read, a seek, a close):
   --  Linux's SCSI command timeout, then some.
   Request_Deadline_Ms : constant := 40_000;

   type Walk_Result is (Walked, Unreadable, Too_Large, Malformed, Timed_Out);

   --  Why the last request or read failed, for the log.
   type Failure is (None, Refused, Short, Source_Timed_Out, Source_Changed, Not_Prepared);

   --  Opens the loan through which every request moves its bytes.
   procedure Initialize (Filesystem : CuBit.Handles.Endpoint; Ready : out Boolean);

   --  The trees, as the last Walk listed them (one copy only: the writer
   --  reads them in place).
   type Payload_Trees is array (Disk_Layout.Install_Volume) of Install_Tree.Tree;
   Trees : Payload_Trees;

   --  Reads Source/INDEX into Trees and adds the storehouse and generation
   --  entries.
   procedure Walk (Source : String; Result : out Walk_Result);

   --  The root tree's first generation record (Layout_Writer's
   --  Generation_First) after a Walk.
   function Generation_First return Install_Tree.Entry_Index;

   --  Components stored by content, after a Walk.
   function Components return Natural;

   --  Data'Length bytes of File from Offset (the writer's Read).
   procedure Read
     (Volume : Disk_Layout.Install_Volume; File : Install_Tree.Entry_Index;
      Offset : Unsigned_64; Data : out Disk_Layout.Bytes; OK : out Boolean);

   --  The writer's Check: File's bytes as read back, in order.
   procedure Check
     (Volume : Disk_Layout.Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : Disk_Layout.Bytes; Final : Boolean; Matches : out Boolean);

   --  Closes the file Read keeps open between calls.
   procedure Finish;

   --  Progress since the last Walk: files Read has opened, bytes it gave.
   function Files_Read return Unsigned_64;
   function Bytes_Read return Unsigned_64;

   function Last_Failure return Failure;
   --  The path of the request that failed last (empty: none).
   function Last_Failed_Path return String;

end Install_Sources;
