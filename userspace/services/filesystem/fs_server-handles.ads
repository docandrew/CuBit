------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The filesystem service's handle table, held unlinked files, file versions.
------------------------------------------------------------------------------
private package Fs_Server.Handles is

   --  File handles are opaque service objects (Fs_Handle_Codes): the
   --  one-based table slot in the low word, its generation in the high word.
   --  A closed handle therefore cannot become valid merely because its slot
   --  is reused for a different file. Extended handles (directories,
   --  optical files) take the slots below EXTENDED_HANDLES.
   procedure allocHandle
     (handle  : out Unsigned_64;
      slot    : out Integer;
      success : out Boolean;
      extended : Boolean := False);

   function resolveHandle
     (handle : Unsigned_64;
      sender : Process_ID;
      expectedKind : Open_Object_Kind) return Integer;

   --  Release a handle: its delegation, kept write error, notice path and
   --  copies go; an unlinked file it held last is reclaimed.
   procedure releaseHandle (slot : Integer);

   procedure releaseHandlesForOwner (owner : Process_ID);

   --  The version of a file seen during this service's life (each change
   --  moves it on), and moving it on.
   function versionOf (key : Inode_Identity) return Unsigned_64;
   procedure bumpVersion (key : Inode_Identity);

   --  A file's last name is gone while handles hold it: they keep a
   --  zero-link inode (their writes must not restore the link), freed at
   --  the last close (reclaimOrphan).
   procedure keepOrphan (volume : Volume_Index; target : Unsigned_32; holder : Natural);

   --  A handle on (volume, target) other than a dropped parked one, or -1.
   function holderOf (volume : Volume_Index; target : Unsigned_32) return Integer;

end Fs_Server.Handles;
