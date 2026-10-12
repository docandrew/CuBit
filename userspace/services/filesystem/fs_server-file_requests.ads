------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  File requests of the filesystem service: open, read, write, seek, close, park, flush, resize, describe.
------------------------------------------------------------------------------
private package Fs_Server.File_Requests is

   --  OP_OPEN: path grant, path length, open options, grant generation.
   procedure handleOpen (sender : Process_ID; msg : Message);

   --  Positioned requests do not temporarily seek.
   type File_Position_Mode is (Advance_Cursor, Explicit_Offset);

   --  OP_READ / OP_WRITE: words (0) = file handle, (1) = grant slot (the
   --  buffer), (2) = byte count, (3) = grant generation.
   procedure handleRead
     (sender : Process_ID; msg : Message;
      mode : File_Position_Mode := Advance_Cursor;
      explicitOffset : Unsigned_64 := 0);

   procedure handleWrite
     (sender : Process_ID; msg : Message;
      mode : File_Position_Mode := Advance_Cursor;
      explicitOffset : Unsigned_64 := 0);

   --  OP_READ_AT / OP_WRITE_AT: transport metadata decoded once, then the
   --  same authorization, grant acquisition, I/O and return path as the
   --  cursor-based requests.
   procedure handlePositioned (sender : Process_ID; msg : Message);

   --  OP_SEEK: words (0) = file handle, (1) = offset, (2) = whence (0 =
   --  SET, 1 = CUR, 2 = END).
   procedure handleSeek (sender : Process_ID; msg : Message);

   --  OP_CLOSE: words (0) = file handle.
   procedure handleClose (sender : Process_ID; msg : Message);

   --  PARK (FQ.Queue_Park): the client keeps a closed handle to reuse for a
   --  later read-only open of the same name. Its rights drop to reading; it
   --  keeps its delegation (or is delegated as a new reader would be). A
   --  handle that cannot read, or is not an ext2 file, is closed instead.
   procedure handlePark (sender : Process_ID; handleWord : Unsigned_64);

   procedure handleFlush (sender : Process_ID; msg : Message);

   procedure handleResize (sender : Process_ID; msg : Message);

   --  Queue_Describe: the open handle's object as one Directory.Inspection.V1
   --  record at the request's arena range. A write-delegated handle's
   --  buffered pages are written first, so size and times include them.
   procedure handleDescribe (owner : Process_ID; handleWord : Unsigned_64);

end Fs_Server.File_Requests;
