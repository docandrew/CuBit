------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  FAT32 volumes over authorized block-device sessions (FS-030): the
--  filesystem service's second writable format, beside ext2/ext3. USB
--  sticks and SD cards move files to and from macOS, Windows and Linux.
--
--  @description
--  The on-disk codecs, names, chain walks and free-cluster ledger are the
--  proved SPARK core (Fat_Format, Fat_Names, Fat_Chains); this package is
--  the transport and the operations over it, regression-tested (not
--  proved), as Ext2 is.
--
--  Objects. FAT has no inodes; an object is named by a number this volume
--  gives the directory entry when it first meets it (2 is the root), kept
--  while the volume is mounted: a rename moves the entry, not the number,
--  and a removed entry's number is never given again. The service's
--  metadata record is an ext2-shaped inode (Ext2_Inodes.Inode) synthesized
--  from the entry: mode 040755 (directory), 100644 or 100444 (read-only
--  attribute), size, the written time as modified and changed time, the
--  access date, one link, and the first cluster in directBlocks (0).
--
--  Mount policy. Read-write only if the device is writable and the volume
--  was cleanly unmounted: FAT[1]'s clean-shutdown and no-hard-error bits
--  set (FATGEN section 4) and the boot sector's dirty bit (BS_Reserved1
--  bit 0, as Windows NT and Linux keep it) clear. Otherwise read-only, as
--  a dirty volume needs a check first. The first write after mount or a
--  flush clears the clean bit in every FAT (made durable before anything
--  else is written); a flush sets it again once everything before it is
--  durable, so an idle volume is clean and a stick pulled between writes
--  needs no repair.
--
--  Write order. Every change is ordered so that a cut leaves only what a
--  FAT checker (fsck.fat, chkdsk) repairs without losing committed data,
--  and never a name for unwritten or freed clusters:
--  1. Growth: the new clusters' data and their chain in every FAT (the
--     active FAT first) are written while nothing references them; a
--     device barrier; then the link from the old last cluster (or the
--     entry's first-cluster field) and the entry's size and time.
--     A cut before the barrier: lost clusters (reclaimed). After it: a
--     chain longer than the size (truncated by the checker), or FATs that
--     differ (the first is current); the old contents stay intact.
--  2. Shrink and removal: the entry first (its new size and end of chain,
--     or the deleted mark, short entry before its long-name parts), a
--     barrier, then the released clusters are freed in every FAT. A cut
--     before the barrier: the old file; after: lost clusters or orphaned
--     long-name parts. A freed cluster is never referenced by a name.
--  3. Creation: the long-name parts, then the short entry (in one sector
--     when they share it). A new directory's cluster ("." and "..",
--     zeros) and its end of chain first, a barrier, then the parent's
--     entry. A directory that must grow gets a zeroed cluster, a barrier,
--     then the link, before any entry is written in it.
--  4. Rename: the new name first (or, replacing a file, the target's short
--     entry rewritten to the source's clusters), a moved directory's ".."
--     next, a barrier, then the old name deleted, a barrier, then a
--     replaced file's clusters freed. A cut between leaves both names on
--     one chain (shared clusters, which the checker separates); the data
--     is never lost.
--  5. The FSInfo free count and next-free hint are written at a flush.
--     They are hints: the free count is counted from the FAT at mount,
--     and an allocation takes only a cluster whose entry reads free.
--  A sector write is assumed atomic (as for ext2); barriers are the
--  device's flush where it has a volatile cache.
--
--  Names: Fat_Names. Times: UTC (FAT has no zone). Limits: files below
--  4 GiB (DIR_FileSize), directories of 65536 entries; numbers are kept
--  for at most Maximum_Objects entries per mount (a named ceiling).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with CuBit.Handles;
with CuBit.Memory_Grants;
with CuBit.Block_Devices;
with CuBit.Directory_Pages;
with Volume_Admission;
with Ext2_Inodes;
with Fat_Format;
with Fat_Chains;

package Fat32 is

   Root_Number : constant Unsigned_32 := 2;

   --  The objects one mount can name (each costs about 32 bytes); past it
   --  a lookup of a new entry fails with Capacity.
   Maximum_Objects : constant := 2 ** 21;

   type Status is
     (Done,
      Listing_End,         --  a directory page that ends the listing
      Not_Found,
      Exists,
      Malformed,           --  the volume's metadata is not well-formed here
      Device_Error,
      Out_Of_Range,
      Range_Unsupported,   --  past 4 GiB, or a directory past 65536 entries
      Capacity,            --  Maximum_Objects
      Read_Only,
      No_Space,
      Is_Directory,
      Not_Directory,
      Not_Empty,
      Invalid_Name,        --  Fat_Names refuses it
      Invalid_Move,        --  a directory into its own subtree
      Wrong_Object,        --  no such object, or not the kind the operation needs
      Recovery_Required);  --  an earlier write failed: the volume needs a check

   type Volume is private;

   --  A volume's window on its device (Ext2.Volume_Window's shape).
   type Volume_Window is record
      First  : Unsigned_64 := 0;
      Blocks : Unsigned_64 := 0;   --  0: the whole device
   end record;

   --  Unsupported_Filesystem when the boot sector is not FAT32's (so
   --  another format may be tried); Invalid_Filesystem when it is but its
   --  geometry or root chain is not well-formed.
   procedure Mount
     (V          : out Volume;
      Endpoint   : CuBit.Handles.Endpoint;
      Key        : Unsigned_64;
      Grant      : CuBit.Memory_Grants.Grant_Reference;
      Buffer     : System.Address;
      Buffer_Bytes : Unsigned_32;
      Window     : Volume_Window;
      Result     : out Volume_Admission.Admission_Result);

   --  What the volume description reports.
   function Read_Only (V : Volume) return Boolean;
   function Quarantined (V : Volume) return Boolean;
   function Session (V : Volume) return CuBit.Block_Devices.Device_Session;
   function Cluster_Bytes (V : Volume) return Unsigned_32;
   function Total_Clusters (V : Volume) return Unsigned_64;
   function Free_Clusters (V : Volume) return Unsigned_64;
   --  Writes not yet made clean and durable by Flush.
   function Unflushed (V : Volume) return Boolean;

   subtype Inode is Ext2_Inodes.Inode;

   procedure Read_Object (V : in out Volume; Number : Unsigned_32; Item : out Inode; S : out Status);
   procedure Resolve (V : in out Volume; Path : String; Number : out Unsigned_32; S : out Status);
   procedure Lookup (V : in out Volume; Directory : Inode; Name : String; Number : out Unsigned_32;
                     S : out Status);

   type Listed_Record is record
      Number : Unsigned_32 := 0;
      Kind   : Unsigned_8 := CuBit.Directory_Pages.Kind_Unknown;
      Length : CuBit.Directory_Pages.Name_Length := 0;
      Name   : CuBit.Directory_Pages.Name_Bytes := [others => 0];
   end record;
   subtype Listed_Index is Natural range 0 .. CuBit.Directory_Pages.Maximum_Entries - 1;
   type Listed_Records is array (Listed_Index) of Listed_Record;

   --  The records one Directory.Page.V2 holds, from Cursor (0 starts; a
   --  byte position, as the previous call's Next): Done when the page is
   --  full, Listing_End when the directory ended.
   procedure Read_Directory_Page
     (V : in out Volume; Directory : Inode; Cursor : Unsigned_64;
      Entries : out Listed_Records; Count : out Natural; Next : out Unsigned_64; S : out Status);

   procedure Read_Data
     (V : in out Volume; Item : Inode; Offset : Unsigned_64; Buffer : System.Address;
      Count : Unsigned_64; Done_Bytes : out Unsigned_64; S : out Status);

   --  A failure may follow a written prefix (Done_Bytes); Item is then the
   --  object as it stands.
   procedure Write_Data
     (V : in out Volume; Number : Unsigned_32; Item : in out Inode; Offset : Unsigned_64;
      Buffer : System.Address; Count : Unsigned_64; Done_Bytes : out Unsigned_64; S : out Status);

   procedure Create_File
     (V : in out Volume; Directory : Unsigned_32; Name : String; Number : out Unsigned_32; S : out Status);

   --  Parent_Status is the parent's resolution; S matters when it is Done.
   procedure Make_Directory
     (V : in out Volume; Path : String; Number : out Unsigned_32; Parent_Status : out Status;
      S : out Status);

   --  Growth exposes zeroes.
   procedure Resize
     (V : in out Volume; Number : Unsigned_32; Size : Unsigned_64; Item : out Inode; S : out Status);

   --  Removes a file's name. With Keep (open handles hold it) its clusters
   --  stay for Reclaim at last close; otherwise they are freed now. Number
   --  and Removed (no links) are set once the name is gone.
   procedure Unlink
     (V : in out Volume; Path : String; Keep : Boolean; Number : out Unsigned_32; Removed : out Inode;
      S : out Status);

   procedure Reclaim (V : in out Volume; Number : Unsigned_32; S : out Status);

   procedure Remove_Directory (V : in out Volume; Path : String; Number : out Unsigned_32; S : out Status);

   --  POSIX rename within the volume: a file over a file, a directory over
   --  an empty directory. A replaced file with Keep stays for Reclaim
   --  (Replaced_Number, Replaced with no links); Replaced_Number is 0 when
   --  nothing was replaced or its clusters were freed.
   procedure Rename
     (V : in out Volume; Old_Path, New_Path : String; Keep : Boolean;
      Replaced_Number : out Unsigned_32; Replaced : out Inode; S : out Status);

   --  Every completed write durable, the FSInfo hints written and the
   --  volume marked clean (see the description).
   procedure Flush (V : in out Volume; S : out Status);

   --  No more writes (a detached volume at the service's stop).
   procedure Retire (V : in out Volume);

   --  Device requests and barriers since startup (profiling, as Ext2's).
   procedure Device_Requests (Requests, Barriers : out Unsigned_64);

private
   type Cache_Store;
   type Cache_Access is access Cache_Store;
   type Object_Store;
   type Object_Access is access Object_Store;

   type Volume is record
      Device       : CuBit.Block_Devices.Device_Session;
      G            : Fat_Format.Geometry;
      Space        : Fat_Chains.Ledger;
      Mounted      : Boolean := False;
      Writable     : Boolean := False;
      Marked_Dirty : Boolean := False;
      Quarantine   : Boolean := False;
      Device_Blocks_Per_Sector : Positive := 1;
      Cache        : Cache_Access := null;
      Objects      : Object_Access := null;
   end record;
end Fat32;
