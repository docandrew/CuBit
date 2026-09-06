------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body CuBit.Filesystems with
   SPARK_Mode => On
is
   OPEN_ACCESS_MASK : constant Open_Options := 3;
   SUPPORTED_OPEN_OPTIONS : constant Open_Options :=
     OPEN_ACCESS_MASK or OPEN_CREATE or OPEN_TRUNCATE;

   function Grant_Message
     (label      : Unsigned_32;
      word0      : Unsigned_64;
      loan       : CuBit.Memory_Grants.Grant_Reference;
      byteCount  : Unsigned_64) return CuBit.Messages.Message;

   function Grant_Message
     (label      : Unsigned_32;
      word0      : Unsigned_64;
      loan       : CuBit.Memory_Grants.Grant_Reference;
      byteCount  : Unsigned_64) return CuBit.Messages.Message
   is
   begin
      return
        (tag      => (label => label, length => 4, flags => 0, badge => 0),
         capBadge => 0,
         words    => (0 => word0,
                      1 => loan.slot,
                      2 => byteCount,
                      3 => loan.generation));
   end Grant_Message;

   function Valid_Open_Options (options : Open_Options) return Boolean is
      accessMode : constant Open_Options := options and OPEN_ACCESS_MASK;
   begin
      return (options and not SUPPORTED_OPEN_OPTIONS) = 0 and then
        accessMode /= OPEN_ACCESS_MASK and then
        ((options and OPEN_TRUNCATE) = 0 or else
         accessMode in OPEN_WRITE_ONLY | OPEN_READ_WRITE);
   end Valid_Open_Options;

   function Requests_Write (options : Open_Options) return Boolean is
      accessMode : constant Open_Options := options and OPEN_ACCESS_MASK;
   begin
      return accessMode in OPEN_WRITE_ONLY | OPEN_READ_WRITE;
   end Requests_Write;

   function Requests_Read (options : Open_Options) return Boolean is
      accessMode : constant Open_Options := options and OPEN_ACCESS_MASK;
   begin
      return accessMode = OPEN_READ_ONLY or else
        accessMode = OPEN_READ_WRITE;
   end Requests_Read;

   function Open_Request
     (loan       : CuBit.Memory_Grants.Grant_Reference;
      pathLength : Nonempty_Path_Byte_Count;
      options    : Open_Options := OPEN_READ_ONLY)
      return CuBit.Messages.Message
   is
   begin
      return
        (tag      => (label => OP_OPEN, length => 4, flags => 0, badge => 0),
         capBadge => 0,
         words    => (0 => loan.slot,
                      1 => Unsigned_64 (pathLength),
                      2 => Unsigned_64 (options),
                      3 => loan.generation));
   end Open_Request;

   function Close_Request
     (handle : File_Handle) return CuBit.Messages.Message
   is
   begin
      return
        (tag      => (label => OP_CLOSE, length => 1, flags => 0, badge => 0),
         capBadge => 0,
         words    => (0 => Unsigned_64 (handle), others => 0));
   end Close_Request;

   function Read_Request
     (handle : File_Handle;
      loan   : CuBit.Memory_Grants.Grant_Reference;
      count  : Unsigned_64) return CuBit.Messages.Message
   is
   begin
      return Grant_Message (OP_READ, Unsigned_64 (handle), loan, count);
   end Read_Request;

   function Write_Request
     (handle : File_Handle;
      loan   : CuBit.Memory_Grants.Grant_Reference;
      count  : Unsigned_64) return CuBit.Messages.Message
   is
   begin
      return Grant_Message (OP_WRITE, Unsigned_64 (handle), loan, count);
   end Write_Request;

   function Seek_Request
     (handle : File_Handle;
      offset : Unsigned_64;
      origin : Seek_Origin) return CuBit.Messages.Message
   is
   begin
      return
        (tag      => (label => OP_SEEK, length => 3, flags => 0, badge => 0),
         capBadge => 0,
         words    => (0 => Unsigned_64 (handle),
                      1 => offset,
                      2 => Unsigned_64 (Seek_Origin'Enum_Rep (origin)),
                      3 => 0));
   end Seek_Request;

   function Open_Directory_Request
     (loan       : CuBit.Memory_Grants.Grant_Reference;
      pathLength : Path_Byte_Count) return CuBit.Messages.Message
   is
   begin
      return
        (tag      => (label => OP_OPEN_DIRECTORY, length => 3,
                       flags => 0, badge => 0),
         capBadge => 0,
         words    => (0 => loan.slot,
                      1 => Unsigned_64 (pathLength),
                      2 => loan.generation,
                      3 => 0));
   end Open_Directory_Request;

   function Read_Directory_Page_Request
     (handle : Directory_Handle;
      loan   : CuBit.Memory_Grants.Grant_Reference)
      return CuBit.Messages.Message
   is
   begin
      return
        (tag      => (label => OP_READ_DIRECTORY_PAGE, length => 4,
                       flags => 0, badge => 0),
         capBadge => 0,
         words    => (0 => Unsigned_64 (handle),
                      1 => loan.slot,
                      2 => Unsigned_64 (PROTOCOL_VERSION),
                      3 => loan.generation));
   end Read_Directory_Page_Request;

   function Close_Directory_Request
     (handle : Directory_Handle) return CuBit.Messages.Message
   is
   begin
      return
        (tag      => (label => OP_CLOSE_DIRECTORY, length => 1,
                       flags => 0, badge => 0),
         capBadge => 0,
         words    => (0 => Unsigned_64 (handle), others => 0));
   end Close_Directory_Request;

   function Rename_Request
     (loan          : CuBit.Memory_Grants.Grant_Reference;
      oldPathLength : Nonempty_Path_Byte_Count;
      newPathLength : Nonempty_Path_Byte_Count)
      return CuBit.Messages.Message
   is
   begin
      return
        (tag      => (label => OP_RENAME, length => 4,
                       flags => 0, badge => 0),
         capBadge => 0,
         words    => (0 => loan.slot,
                      1 => Unsigned_64 (oldPathLength),
                      2 => Unsigned_64 (newPathLength),
                      3 => loan.generation));
   end Rename_Request;
end CuBit.Filesystems;
