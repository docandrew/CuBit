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
     (Open_File, Close_File, Read_File, Write_File, Seek_File,
      Read_Directory, Rename_File, Set_Access_Profile,
      Revoke_Access_Profile);
   for Filesystem_Operation use
     (Open_File             => 16#0001#,
      Close_File            => 16#0002#,
      Read_File             => 16#0003#,
      Write_File            => 16#0004#,
      Seek_File             => 16#0006#,
      Read_Directory        => 16#0007#,
      Rename_File           => 16#0008#,
      Set_Access_Profile    => 16#0080#,
      Revoke_Access_Profile => 16#0081#);

   --  Ada case choices require static expressions. These constants are the
   --  sole numeric definition site for the userspace filesystem protocol.
   OP_OPEN       : constant Unsigned_32 := 16#0001#;
   OP_CLOSE      : constant Unsigned_32 := 16#0002#;
   OP_READ       : constant Unsigned_32 := 16#0003#;
   OP_WRITE      : constant Unsigned_32 := 16#0004#;
   OP_SEEK       : constant Unsigned_32 := 16#0006#;
   OP_READDIR    : constant Unsigned_32 := 16#0007#;
   OP_RENAME     : constant Unsigned_32 := 16#0008#;
   OP_SET_ACL    : constant Unsigned_32 := 16#0080#;
   OP_REVOKE_ACL : constant Unsigned_32 := 16#0081#;

   REPLY_OK            : constant Unsigned_32 := 16#F000#;
   REPLY_ERR           : constant Unsigned_32 := 16#F001#;
   REPLY_ERROR         : constant Unsigned_32 := REPLY_ERR;
   REPLY_ACCESS_DENIED : constant Unsigned_32 := 16#F007#;

   MAXIMUM_PATH_BYTES : constant := 256;
   subtype Path_Byte_Count is Natural range 0 .. MAXIMUM_PATH_BYTES;
   subtype Nonempty_Path_Byte_Count is
     Path_Byte_Count range 1 .. MAXIMUM_PATH_BYTES;

   --  Handles are service-issued, generation-tagged object identities. Their
   --  representation is intentionally opaque to clients.
   type File_Handle is new Unsigned_64;
   INVALID_FILE_HANDLE : constant File_Handle := 0;

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

   function Read_Directory_Request
     (loan       : CuBit.Memory_Grants.Grant_Reference;
      pathLength : Path_Byte_Count;
      capacity   : Unsigned_64) return CuBit.Messages.Message;

   function Rename_Request
     (loan          : CuBit.Memory_Grants.Grant_Reference;
      oldPathLength : Nonempty_Path_Byte_Count;
      newPathLength : Nonempty_Path_Byte_Count)
      return CuBit.Messages.Message;
end CuBit.Filesystems;
