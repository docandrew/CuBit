------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Name lookup and directory listing: path resolution through the name
--  cache, cached-block directory scans, and Directory.Page.V2 pages. Every
--  record is validated (Ext2_Format.Fits) before its name is viewed.
------------------------------------------------------------------------------
pragma Ada_2022;

private package Ext2.Lookup is

   --  The fast path of a lookup: a plain directory's blocks scanned in the
   --  block cache, comparing lengths before bytes. With clean, the answer
   --  (and, if found, the index of the directory block holding the name);
   --  without, a record did not validate or a block was not cached, and
   --  the general, fully checked path must answer.
   procedure scanCachedDirectory
     (fs : Filesystem; dirIno : Inode; name : String;
      inodeNum : out Unsigned_32; blockIndex : out Natural;
      status : out Directory_Lookup_Status; clean : out Boolean);

   procedure lookupInDir
     (fs      : Filesystem;
      dirIno  : Inode;
      name : String;
      inodeNum : out Unsigned_32;
      status : out Directory_Lookup_Status)
     with Post => (if status = Lookup_Found then inodeNum /= 0 else inodeNum = 0);

   --  Resolve a volume-relative path from the root, a component at a time.
   procedure resolvePath
     (fs : Filesystem; path : String; inodeNum : out Unsigned_32;
      status : out Directory_Lookup_Status)
     with Post => (if status = Lookup_Found then inodeNum /= 0 else inodeNum = 0);

   procedure readDirectoryPage
     (fs          : Filesystem;
      dirIno      : Inode;
      cursor      : Unsigned_64;
      entries     : out Listed_Records;
      entryCount  : out Natural;
      nextCursor  : out Unsigned_64;
      status      : out Directory_Read_Status)
     --  A scan never moves backwards (lookupInDir relies on progress).
     with Post => nextCursor >= cursor;

end Ext2.Lookup;
