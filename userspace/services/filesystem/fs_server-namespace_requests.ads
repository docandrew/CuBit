------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Namespace requests of the filesystem service: rename, unlink, mkdir, rmdir.
------------------------------------------------------------------------------
private package Fs_Server.Namespace_Requests is

   --  OP_RENAME: words (0) = grant slot (both paths), (1) = old path
   --  length, (2) = new path length, (3) = grant generation. Grant buffer
   --  layout: [old path][new path].
   procedure handleRename (sender : Process_ID; msg : Message);

   --  Namespace operations (unlink, mkdir, rmdir). Request as OP_OPEN:
   --  words (0) = path grant slot, (1) = path length, (3) = grant
   --  generation; an unlink's (2) = its parked handle (0: none). Authority:
   --  ACL_WRITE and ACL_CREATE on the path, as for OPEN_CREATE.
   procedure handleUnlink (sender : Process_ID; msg : Message);

   procedure handleMkdir (sender : Process_ID; msg : Message);

   procedure handleRmdir (sender : Process_ID; msg : Message);

end Fs_Server.Namespace_Requests;
