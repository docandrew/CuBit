------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Typed application protocol for filesystem services.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Memory_Grants;
with CuBit.Messages;

package CuBit.Filesystems with
   SPARK_Mode => On
is
   PROTOCOL_VERSION : constant Unsigned_16 := 1;

   type Filesystem_Operation is
     (Open_File, Close_File, Read_File, Write_File, Open_Directory,
      Seek_File, Read_Directory_Page, Rename_File, Close_Directory,
      Set_Access_Profile,
      Revoke_Access_Profile);
   for Filesystem_Operation use
     (Open_File             => 16#0001#,
      Close_File            => 16#0002#,
      Read_File             => 16#0003#,
      Write_File            => 16#0004#,
      Open_Directory        => 16#0005#,
      Seek_File             => 16#0006#,
      Read_Directory_Page   => 16#0007#,
      Rename_File           => 16#0008#,
      Close_Directory       => 16#0009#,
      Set_Access_Profile    => 16#0080#,
      Revoke_Access_Profile => 16#0081#);

   --  Ada case choices require static expressions. These constants are the
   --  sole numeric definition site for the userspace filesystem protocol.
   OP_OPEN       : constant Unsigned_32 := 16#0001#;
   OP_CLOSE      : constant Unsigned_32 := 16#0002#;
   OP_READ       : constant Unsigned_32 := 16#0003#;
   OP_WRITE      : constant Unsigned_32 := 16#0004#;
   OP_OPEN_DIRECTORY : constant Unsigned_32 := 16#0005#;
   OP_SEEK       : constant Unsigned_32 := 16#0006#;
   OP_READ_DIRECTORY_PAGE : constant Unsigned_32 := 16#0007#;
   OP_RENAME     : constant Unsigned_32 := 16#0008#;
   OP_CLOSE_DIRECTORY : constant Unsigned_32 := 16#0009#;
   OP_SET_ACL    : constant Unsigned_32 := 16#0080#;
   OP_REVOKE_ACL : constant Unsigned_32 := 16#0081#;

   REPLY_OK            : constant Unsigned_32 := 16#F000#;
   REPLY_ERR           : constant Unsigned_32 := 16#F001#;
   REPLY_ERROR         : constant Unsigned_32 := REPLY_ERR;
   REPLY_NO_SPACE      : constant Unsigned_32 := 16#F002#;
   REPLY_READ_ONLY     : constant Unsigned_32 := 16#F003#;
   REPLY_OUT_OF_RANGE  : constant Unsigned_32 := 16#F004#;
   REPLY_FILE_RANGE_UNSUPPORTED : constant Unsigned_32 := 16#F005#;
   REPLY_IO_ERROR      : constant Unsigned_32 := 16#F006#;
   REPLY_ACCESS_DENIED : constant Unsigned_32 := 16#F007#;
   REPLY_MALFORMED_FILESYSTEM : constant Unsigned_32 := 16#F008#;
   REPLY_WRONG_OBJECT_TYPE    : constant Unsigned_32 := 16#F009#;

   MAXIMUM_PATH_BYTES : constant := 256;
   subtype Path_Byte_Count is Natural range 0 .. MAXIMUM_PATH_BYTES;
   subtype Nonempty_Path_Byte_Count is
     Path_Byte_Count range 1 .. MAXIMUM_PATH_BYTES;

   --  Handles are service-issued, generation-tagged object identities. Their
   --  representation is intentionally opaque to clients.
   type File_Handle is new Unsigned_64;
   INVALID_FILE_HANDLE : constant File_Handle := 0;

   type Directory_Handle is new Unsigned_64;
   INVALID_DIRECTORY_HANDLE : constant Directory_Handle := 0;

   --  Directory.Page.V1 is deliberately fixed-size.  The service acquires
   --  exactly one page from the caller and never trusts a caller-supplied
   --  capacity.  Inode/object hints are descriptive identities only: they
   --  cannot be used in place of a handle and confer no authority.
   DIRECTORY_PAGE_BYTES : constant := 4096;
   MAXIMUM_DIRECTORY_NAME_BYTES : constant := 255;
   MAXIMUM_DIRECTORY_PAGE_ENTRIES : constant := 14;
   DIRECTORY_PAGE_HEADER_BYTES : constant := 32;
   DIRECTORY_ENTRY_BYTES : constant := 280;

   DIRECTORY_PAGE_END : constant Unsigned_32 := 1;
   DIRECTORY_ENTRY_SIZE_VALID : constant Unsigned_8 := 1;

   DIRECTORY_KIND_UNKNOWN   : constant Unsigned_8 := 0;
   DIRECTORY_KIND_FILE      : constant Unsigned_8 := 1;
   DIRECTORY_KIND_DIRECTORY : constant Unsigned_8 := 2;
   DIRECTORY_KIND_SYMLINK   : constant Unsigned_8 := 3;

   subtype Directory_Name_Index is
     Positive range 1 .. MAXIMUM_DIRECTORY_NAME_BYTES;
   type Directory_Name is array (Directory_Name_Index) of Unsigned_8
     with Component_Size => 8;

   type Directory_Page_Header is record
      version       : Unsigned_16;
      headerBytes   : Unsigned_16;
      entryBytes    : Unsigned_16;
      entryCount    : Unsigned_16;
      flags         : Unsigned_32;
      reserved      : Unsigned_32;
      nextCursor    : Unsigned_64;
      snapshot      : Unsigned_64;
   end record with Convention => C, Size => DIRECTORY_PAGE_HEADER_BYTES * 8;

   for Directory_Page_Header use record
      version     at 0  range 0 .. 15;
      headerBytes at 2  range 0 .. 15;
      entryBytes  at 4  range 0 .. 15;
      entryCount  at 6  range 0 .. 15;
      flags       at 8  range 0 .. 31;
      reserved    at 12 range 0 .. 31;
      nextCursor  at 16 range 0 .. 63;
      snapshot    at 24 range 0 .. 63;
   end record;

   type Directory_Entry is record
      objectHint : Unsigned_64;
      sizeBytes  : Unsigned_64;
      nameLength : Unsigned_16;
      kind       : Unsigned_8;
      flags      : Unsigned_8;
      reserved   : Unsigned_32;
      name       : Directory_Name;
   end record with Convention => C, Size => DIRECTORY_ENTRY_BYTES * 8;

   for Directory_Entry use record
      objectHint at 0  range 0 .. 63;
      sizeBytes  at 8  range 0 .. 63;
      nameLength at 16 range 0 .. 15;
      kind       at 18 range 0 .. 7;
      flags      at 19 range 0 .. 7;
      reserved   at 20 range 0 .. 31;
      name       at 24 range 0 .. MAXIMUM_DIRECTORY_NAME_BYTES * 8 - 1;
   end record;

   subtype Directory_Page_Entry_Index is Natural range
     0 .. MAXIMUM_DIRECTORY_PAGE_ENTRIES - 1;
   type Directory_Entries is
     array (Directory_Page_Entry_Index) of Directory_Entry
       with Component_Size => DIRECTORY_ENTRY_BYTES * 8;

   type Open_Options is mod 2 ** 64;
   OPEN_READ_ONLY  : constant Open_Options := 0;
   OPEN_WRITE_ONLY : constant Open_Options := 1;
   OPEN_READ_WRITE : constant Open_Options := 2;
   OPEN_CREATE     : constant Open_Options := 64;
   OPEN_TRUNCATE   : constant Open_Options := 512;

   function Valid_Open_Options (options : Open_Options) return Boolean;
   function Requests_Read (options : Open_Options) return Boolean;
   function Requests_Write (options : Open_Options) return Boolean;

   type Seek_Origin is (From_Start, From_Current, From_End);
   for Seek_Origin use
     (From_Start => 0, From_Current => 1, From_End => 2);

   function Open_Request
     (loan       : CuBit.Memory_Grants.Grant_Reference;
      pathLength : Nonempty_Path_Byte_Count;
      options    : Open_Options := OPEN_READ_ONLY)
      return CuBit.Messages.Message;

   function Close_Request
     (handle : File_Handle) return CuBit.Messages.Message;

   function Read_Request
     (handle : File_Handle;
      loan   : CuBit.Memory_Grants.Grant_Reference;
      count  : Unsigned_64) return CuBit.Messages.Message;

   function Write_Request
     (handle : File_Handle;
      loan   : CuBit.Memory_Grants.Grant_Reference;
      count  : Unsigned_64) return CuBit.Messages.Message;

   function Seek_Request
     (handle : File_Handle;
      offset : Unsigned_64;
      origin : Seek_Origin) return CuBit.Messages.Message;

   function Open_Directory_Request
     (loan       : CuBit.Memory_Grants.Grant_Reference;
      pathLength : Path_Byte_Count) return CuBit.Messages.Message;

   function Read_Directory_Page_Request
     (handle : Directory_Handle;
      loan   : CuBit.Memory_Grants.Grant_Reference)
      return CuBit.Messages.Message;

   function Close_Directory_Request
     (handle : Directory_Handle) return CuBit.Messages.Message;

   function Rename_Request
     (loan          : CuBit.Memory_Grants.Grant_Reference;
      oldPathLength : Nonempty_Path_Byte_Count;
      newPathLength : Nonempty_Path_Byte_Count)
      return CuBit.Messages.Message;
end CuBit.Filesystems;
