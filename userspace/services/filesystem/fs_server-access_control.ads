------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Per-process file access policies of the filesystem service, and their administration.
------------------------------------------------------------------------------
with CuBit.File_Access;

private package Fs_Server.Access_Control is

   --  Administrative identity comes only from the kernel's authenticated
   --  service registry. Query it for each rare policy operation so a cached
   --  raw PID cannot become authority after process death and PID reuse.
   function isAdmin (sender : Process_ID) return Boolean;

   --  Whether sender has the access rights for path. A scope prefix must end
   --  at a path-component boundary: authority for "apps/foo" must not also
   --  authorize "apps/foobar".
   function checkAccess
     (sender : Process_ID;
      path : String;
      rights : Unsigned_8) return Boolean;

   --  OP_SET_ACL: words (0) = target PID, (1) = entry count (0 = wildcard
   --  full access), (2) = grant slot and (3) = grant generation (when the
   --  count is not 0).
   procedure handleSetACL (sender : Process_ID; msg : Message);

   --  OP_REVOKE_ACL: words (0) = target PID.
   procedure handleRevokeACL (sender : Process_ID; msg : Message);

   --  OP_RELEASE_OWNER (procmgr, once a process has exited): nothing it
   --  held survives to be found through a reused PID. Its handles are
   --  released (a write delegation's dirty pages are harvested first: the
   --  service's acquisition keeps the arena's frames), then its queue and
   --  its access profile go.
   procedure handleReleaseOwner (sender : Process_ID; msg : Message);

   --  Queue_List_Scopes (docs/filesystem-protocol-v2.md step 6): the
   --  caller's own access profile, as CuBit.File_Access wire entries. A
   --  process learns only its own grants. Without a profile: no entries.
   procedure handleListScopes (owner : Process_ID; rangeBytes : Unsigned_64);

   --  The caller's own access profile as those wire entries (Count of
   --  them from Wire'First; none without a profile), for Queue_List_Places.
   procedure scopesOf
     (owner : Process_ID;
      wire  : out CuBit.File_Access.Wire_Bytes;
      count : out Natural)
     with Pre => wire'First = 1 and then
                 wire'Length = CuBit.File_Access.Maximum_Entries *
                               CuBit.File_Access.Wire_Entry_Bytes;

end Fs_Server.Access_Control;
