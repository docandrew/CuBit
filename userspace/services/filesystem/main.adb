------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace Ext2 filesystem server.
--
--  Serves file operations from a ramdisk Ext2 image via IPC.
--  Receives OP_OPEN/OP_READ/OP_SEEK/OP_CLOSE messages from clients
--  and replies with file data.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Filesystems; use CuBit.Filesystems;
with Cpio;
with Ext2;

procedure main is
   use ASCII;

   --  Sysinfo query for ramdisk address
   --  (uses SYSINFO_RAMDISK_ADDRESS from CuBit.Messages)

   --  Maximum open files and path length
   MAX_OPEN_FILES : constant := 32;

   --  Backend kind for file handles
   type BackendKind is
     (CPIO_RAMDISK, EXT2_MEMORY, EXT2_ATA, EXT2_NVME);

   type SchemeKind is
     (AUTOMATIC_SCHEME, MEMORY_SCHEME, ATA_SCHEME, NVME_SCHEME);

   type Open_Object_Kind is (FILE_OBJECT, DIRECTORY_OBJECT);

   --  File handle entry (tracks which backend each file uses)
   type FileEntry is record
      active      : Boolean      := False;
      retired     : Boolean      := False;
      generation  : Unsigned_32  := 1;
      backend     : BackendKind  := CPIO_RAMDISK;
      inodeNum    : Unsigned_32  := 0;      --  ext2 only
      ino         : Ext2.Inode;             --  ext2 only
      cpioFileIdx : Natural      := 0;      --  cpio only
      offset      : Unsigned_64  := 0;
      ownerPID    : ProcessID    := NO_PROCESS;
      openRights  : Unsigned_8   := 0;
      objectKind  : Open_Object_Kind := FILE_OBJECT;
   end record;

   type FileTable is array (0 .. MAX_OPEN_FILES - 1) of FileEntry;
   files : FileTable;

   --  CPIO ramdisk archive
   cpioArchive : Cpio.Archive;
   cpioOk      : Boolean := False;

   --  Optional writable Ext2 image carried in the trusted bootstrap archive.
   memoryFs          : Ext2.Filesystem;
   memoryInitialized : Boolean := False;

   --  ATA-backed filesystem context (lazy initialized)
   ataFs          : Ext2.Filesystem;
   ataInitialized : Boolean := False;
   ataInitFailed  : Boolean := False;  --  true after a real init attempt fails

   --  ATA grant buffer (8 pages = 32KB for multi-sector bulk reads)
   ATA_GRANT_PAGES : constant := 8;
   ataGrantBuf     : System.Address := System.Null_Address;
   ataGrant        : CuBit.Memory_Grants.Grant_Reference;

   --  NVMe-backed filesystem context (lazy initialized)
   nvmeFs          : Ext2.Filesystem;
   nvmeInitialized : Boolean := False;

   --  NVMe grant buffer (128 pages = 512KB for large PRP transfers)
   NVME_GRANT_PAGES : constant := 128;
   nvmeGrantBuf     : System.Address := System.Null_Address;
   nvmeGrant        : CuBit.Memory_Grants.Grant_Reference;

   ---------------------------------------------------------------------------
   --  Per-process file ACL infrastructure
   ---------------------------------------------------------------------------

   ACL_READ   : constant Unsigned_8 := 1;
   ACL_WRITE  : constant Unsigned_8 := 2;
   ACL_CREATE : constant Unsigned_8 := 8;

   MAX_ACL_PREFIX : constant := 64;
   MAX_ACL_ENTRIES : constant := 16;
   MAX_ACL_PROFILES : constant := 32;

   type ACLEntry is record
      prefix    : String (1 .. MAX_ACL_PREFIX);
      prefixLen : Natural    := 0;  --  0 = wildcard (matches everything)
      rights    : Unsigned_8 := 0;
   end record;

   type ACLEntryArray is
     array (0 .. MAX_ACL_ENTRIES - 1) of ACLEntry;

   type ACLProfile is record
      pid    : ProcessID := NO_PROCESS;
      active : Boolean   := False;
      count  : Natural   := 0;
      entries : ACLEntryArray;
   end record;

   aclProfiles : array (0 .. MAX_ACL_PROFILES - 1) of ACLProfile;

   --  Administrative identity comes only from the kernel's authenticated
   --  service registry. Query it for each rare policy operation so a cached
   --  raw PID cannot become authority after process death and PID reuse.
   function isAdmin (sender : ProcessID) return Boolean is
      devmgrAdmin : constant Unsigned_64 :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR);
      procmgrAdmin : constant Unsigned_64 :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_PROCMGR);
   begin
      return
        (devmgrAdmin /= 0 and then devmgrAdmin /= Unsigned_64'Last and then
         sender = devmgrAdmin) or else
        (procmgrAdmin /= 0 and then procmgrAdmin /= Unsigned_64'Last and then
         sender = procmgrAdmin);
   end isAdmin;

   --  Check if sender has access rights for the given path.  A scope prefix
   --  must end at a path-component boundary: authority for "apps/foo" must
   --  not also authorize "apps/foobar".
   function checkAccess
     (sender : ProcessID;
      path   : String;
      rights : Unsigned_8) return Boolean
   is
   begin
      for i in aclProfiles'Range loop
         if aclProfiles (i).active and then
            aclProfiles (i).pid = sender
         then
            --  Profile found; scan entries for prefix match
            for j in 0 .. aclProfiles (i).count - 1 loop
               if (aclProfiles (i).entries (j).rights and rights) = rights
               then
                  --  Wildcard entry matches everything
                  if aclProfiles (i).entries (j).prefixLen = 0 then
                     return True;
                  end if;

                  --  Prefix match
                  if path'Length >=
                     aclProfiles (i).entries (j).prefixLen
                  then
                     declare
                        pLen : constant Natural :=
                          aclProfiles (i).entries (j).prefixLen;
                        match : Boolean := True;
                     begin
                        for k in 0 .. pLen - 1 loop
                           if path (path'First + k) /=
                              aclProfiles (i).entries (j).prefix (1 + k)
                           then
                              match := False;
                              exit;
                           end if;
                        end loop;
                        if match and then
                          (path'Length = pLen or else
                           aclProfiles (i).entries (j).prefix (pLen) = '/' or else
                           path (path'First + pLen) = '/')
                        then
                           return True;
                        end if;
                     end;
                  end if;
               end if;
            end loop;

            --  Profile found but no matching entry
            return False;
         end if;
      end loop;

      --  No profile found: default deny
      return False;
   end checkAccess;

   --  Conversion helpers
   function toAddr is new Ada.Unchecked_Conversion
     (Unsigned_64, System.Address);

   --  File handles are opaque service objects.  The low word contains the
   --  one-based table slot and the high word contains its generation.  A
   --  closed handle therefore cannot become valid merely because its slot is
   --  reused for a different file.
   procedure allocHandle
     (handle  : out Unsigned_64;
      slot    : out Integer;
      success : out Boolean)
   is
   begin
      for i in files'Range loop
         if not files (i).active and then not files (i).retired then
            slot := i;
            handle := Shift_Left (Unsigned_64 (files (i).generation), 32) or
              Unsigned_64 (i + 1);
            success := True;
            return;
         end if;
      end loop;

      handle := 0;
      slot := -1;
      success := False;
   end allocHandle;

   function resolveHandle
     (handle : Unsigned_64;
      sender : ProcessID;
      expectedKind : Open_Object_Kind) return Integer
   is
      slotCode : constant Unsigned_64 := handle and 16#FFFF_FFFF#;
      generation : constant Unsigned_32 :=
        Unsigned_32 (Shift_Right (handle, 32));
      slot : Integer;
   begin
      if slotCode = 0 or else slotCode > Unsigned_64 (MAX_OPEN_FILES) then
         return -1;
      end if;

      slot := Integer (slotCode - 1);
      if not files (slot).active or else
         files (slot).retired or else
         files (slot).generation /= generation or else
         files (slot).ownerPID /= sender or else
         files (slot).objectKind /= expectedKind
      then
         return -1;
      end if;

      return slot;
   end resolveHandle;

   procedure releaseHandle (slot : Integer) is
   begin
      files (slot).active := False;
      files (slot).ownerPID := NO_PROCESS;
      files (slot).openRights := 0;
      files (slot).offset := 0;
      files (slot).objectKind := FILE_OBJECT;

      --  Never wrap a generation: retire the slot instead.  This makes stale
      --  handle rejection unconditional rather than merely probabilistic.
      if files (slot).generation = Unsigned_32'Last then
         files (slot).retired := True;
      else
         files (slot).generation := files (slot).generation + 1;
      end if;
   end releaseHandle;

   procedure releaseHandlesForOwner (owner : ProcessID) is
   begin
      for slot in files'Range loop
         if files (slot).active and then files (slot).ownerPID = owner then
            releaseHandle (slot);
         end if;
      end loop;
   end releaseHandlesForOwner;

   --  Decode an untrusted wire reference only after checking that both fields
   --  fit the strongly typed userspace representation.  The kernel then
   --  authenticates the owner, current grantee, generation, access, and full
   --  byte range before returning an address.
   procedure acquireClientMemory
     (sender        : ProcessID;
      rawSlot       : Unsigned_64;
      rawGeneration : Unsigned_64;
      byteLength    : Unsigned_64;
      requiredAccess : CuBit.Memory_Grants.Required_Access;
      address       : out System.Address;
      success       : out Boolean)
   is
   begin
      address := System.Null_Address;
      success := False;
      if rawSlot > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
         rawGeneration = 0 or else
         rawGeneration > CuBit.Memory_Grants.MAXIMUM_GENERATION or else
         byteLength = 0
      then
         return;
      end if;

      CuBit.Memory_Grants.Acquire
        (reference      =>
           (slot => CuBit.Memory_Grants.Global_Grant_Slot (rawSlot),
            generation =>
              CuBit.Memory_Grants.Grant_Generation (rawGeneration)),
         expectedOwner => sender,
         byteOffset     => 0,
         byteLength     => byteLength,
         requiredAccess => requiredAccess,
         mappedAddress  => address,
         success        => success);
   end acquireClientMemory;

   procedure returnClientMemory
     (rawSlot       : Unsigned_64;
      rawGeneration : Unsigned_64;
      success       : out Boolean)
   is
   begin
      CuBit.Memory_Grants.Return_Acquisition
        ((slot => CuBit.Memory_Grants.Global_Grant_Slot (rawSlot),
          generation =>
            CuBit.Memory_Grants.Grant_Generation (rawGeneration)),
         success);
   end returnClientMemory;

   --  Send a reply with the given label and word0 value
   procedure sendReply
     (dest   : ProcessID;
      label  : Unsigned_32;
      word0  : Unsigned_64)
   is
      replyMsg : Message;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 1,
                       flags  => 0,
                       badge  => 0);
      replyMsg.words := (0 => word0, others => 0);
      ignore := reply (dest, replyMsg);
   end sendReply;

   --  Handle OP_SET_ACL
   --  words(0) = target PID
   --  words(1) = entry count (0 = wildcard full access)
   --  words(2) = grant slot (when count > 0)
   --  words(3) = grant generation (when count > 0)
   procedure handleSetACL (sender : ProcessID; msg : Message) is
      targetPID     : constant ProcessID := msg.words (0);
      entryCountRaw : constant Unsigned_64 := msg.words (1);
      entryCount    : Natural := 0;
      slotIdx       : Integer := -1;
      grantAddr     : System.Address := System.Null_Address;
      grantOk       : Boolean := False;
      returned      : Boolean := False;
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      if entryCountRaw > Unsigned_64 (MAX_ACL_ENTRIES) then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      entryCount := Natural (entryCountRaw);

      --  Find existing profile or allocate a free slot
      for i in aclProfiles'Range loop
         if aclProfiles (i).active and then
            aclProfiles (i).pid = targetPID
         then
            slotIdx := i;
            exit;
         end if;
      end loop;

      if slotIdx < 0 then
         for i in aclProfiles'Range loop
            if not aclProfiles (i).active then
               slotIdx := i;
               exit;
            end if;
         end loop;
      end if;

      if slotIdx < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if entryCount > 0 then
         if msg.tag.length /= 4 then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;

         acquireClientMemory
           (sender, msg.words (2), msg.words (3),
            Unsigned_64 (entryCount * 72),
            CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
         if not grantOk then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;
      end if;

      aclProfiles (slotIdx).pid    := targetPID;
      aclProfiles (slotIdx).active := True;
      releaseHandlesForOwner (targetPID);

      if entryCount = 0 then
         --  Wildcard: single entry with prefixLen=0, rights=all
         aclProfiles (slotIdx).count := 1;
         aclProfiles (slotIdx).entries (0).prefixLen := 0;
         aclProfiles (slotIdx).entries (0).rights := 16#FF#;
      else
         --  Read entries from grant buffer
         --  Each entry: 1 byte rights, 1 byte prefixLen, 6 reserved,
         --  64 bytes prefix = 72 bytes total
         declare
            buf : array (0 .. entryCount * 72 - 1) of Unsigned_8
              with Import, Address => grantAddr;
            base : Natural;
            pLen : Natural;
         begin
            aclProfiles (slotIdx).count := entryCount;
            for e in 0 .. entryCount - 1 loop
               base := e * 72;
               aclProfiles (slotIdx).entries (e).rights := buf (base);
               pLen := Natural (buf (base + 1));
               if pLen > MAX_ACL_PREFIX then
                  pLen := MAX_ACL_PREFIX;
               end if;
               aclProfiles (slotIdx).entries (e).prefixLen := pLen;
               for c in 0 .. pLen - 1 loop
                  aclProfiles (slotIdx).entries (e).prefix (1 + c) :=
                    Character'Val (buf (base + 8 + c));
               end loop;
            end loop;
         end;
         returnClientMemory
           (msg.words (2), msg.words (3), returned);
         if not returned then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;
      end if;

      debugPrint ("FS Server: ACL set for PID" & LF);
      sendReply (sender, REPLY_OK, 0);
   end handleSetACL;

   --  Handle OP_REVOKE_ACL
   --  words(0) = target PID
   procedure handleRevokeACL (sender : ProcessID; msg : Message) is
      targetPID : constant ProcessID := msg.words (0);
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      for i in aclProfiles'Range loop
         if aclProfiles (i).active and then
            aclProfiles (i).pid = targetPID
         then
            aclProfiles (i).active := False;
            aclProfiles (i).pid    := NO_PROCESS;
            aclProfiles (i).count  := 0;
         end if;
      end loop;

      releaseHandlesForOwner (targetPID);

      sendReply (sender, REPLY_OK, 0);
   end handleRevokeACL;

   --  ATA driver PID (discovered at runtime via sysinfo)
   ataDriverPID  : Unsigned_64 := 0;

   --  NVMe driver PID (discovered at runtime via sysinfo)
   nvmeDriverPID : Unsigned_64 := 0;

   --  Page size for grant buffer allocation
   FS_PAGE_SIZE : constant := 4096;

   --  Parse an explicit backend selector and return the first path byte after
   --  it.  An unqualified path uses the unified bootstrap/overlay namespace.
   procedure parseScheme
     (pathStr  : String;
      scheme   : out SchemeKind;
      relStart : out Natural)
   is
   begin
      scheme   := AUTOMATIC_SCHEME;
      relStart := pathStr'First;

      if pathStr'Length >= 6 and then
         pathStr (pathStr'First)     = '@' and then
         pathStr (pathStr'First + 1) = 'n' and then
         pathStr (pathStr'First + 2) = 'v' and then
         pathStr (pathStr'First + 3) = 'm' and then
         pathStr (pathStr'First + 4) = 'e' and then
         pathStr (pathStr'First + 5) = ':'
      then
         scheme   := NVME_SCHEME;
         relStart := pathStr'First + 6;
      elsif pathStr'Length >= 5 and then
         pathStr (pathStr'First)     = '@' and then
         pathStr (pathStr'First + 1) = 'a' and then
         pathStr (pathStr'First + 2) = 't' and then
         pathStr (pathStr'First + 3) = 'a' and then
         pathStr (pathStr'First + 4) = ':'
      then
         scheme   := ATA_SCHEME;
         relStart := pathStr'First + 5;
      elsif pathStr'Length >= 5 and then
         pathStr (pathStr'First)     = '@' and then
         pathStr (pathStr'First + 1) = 'm' and then
         pathStr (pathStr'First + 2) = 'e' and then
         pathStr (pathStr'First + 3) = 'm' and then
         pathStr (pathStr'First + 4) = ':'
      then
         scheme   := MEMORY_SCHEME;
         relStart := pathStr'First + 5;
      end if;
   end parseScheme;

   --  Lazy-initialize the ATA filesystem on first @ata: open.
   --  Creates a grant buffer and reads the superblock via ATA IPC.
   procedure ensureATA (ok : out Boolean) is
      grantOk : Boolean;
   begin
      if ataInitialized then
         ok := True;
         return;
      end if;

      if ataInitFailed then
         ok := False;
         return;
      end if;

      --  Discover ATA driver PID via sysinfo
      if ataDriverPID = 0 then
         ataDriverPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_ATA);
         if ataDriverPID = 0 or ataDriverPID = Unsigned_64'Last then
            debugPrint ("FS Server: ATA driver not registered." & LF);
            ataDriverPID := 0;
            ok := False;
            return;
         end if;
      end if;

      --  Allocate a page-aligned grant buffer via sbrk
      if ataGrantBuf = System.Null_Address then
         declare
            raw : Unsigned_64;
            aligned : Unsigned_64;
            allocSize : constant Unsigned_64 :=
              Unsigned_64 (ATA_GRANT_PAGES) * FS_PAGE_SIZE + FS_PAGE_SIZE;
         begin
            raw := syscall (SYSCALL_SBRK, allocSize);
            if raw = Unsigned_64'Last then
               debugPrint ("FS Server: sbrk failed for ATA grant buffer." & LF);
               ok := False;
               return;
            end if;
            aligned := (raw + FS_PAGE_SIZE - 1) and not (FS_PAGE_SIZE - 1);
            ataGrantBuf := toAddr (aligned);
         end;
      end if;

      --  Create a grant to the ATA driver for data transfer
      CuBit.Memory_Grants.Create_Via_Capability
        (slot      => CAP_SLOT_ATA,
         localAddr => ataGrantBuf,
         numPages  => ATA_GRANT_PAGES,
         readWrite => True,
         reference => ataGrant,
         success   => grantOk);

      if not grantOk then
         debugPrint ("FS Server: Failed to create ATA grant." & LF);
         ok := False;
         return;
      end if;

      debugPrint ("FS Server: ATA grant reference ready." & LF);
      debugPrint ("FS Server: Initializing ATA ext2 filesystem..." & LF);

      Ext2.initBlockDevice
        (ataFs, CAP_SLOT_ATA, ataGrant, ataGrantBuf,
         ATA_GRANT_PAGES * 4096, ok);

      if ok then
         ataInitialized := True;
         debugPrint ("FS Server: ATA ext2 filesystem initialized." & LF);
      else
         ataInitFailed := True;
         debugPrint ("FS Server: ATA ext2 init failed (no ext2?)." & LF);
         CuBit.Memory_Grants.Revoke (ataGrant, grantOk);
      end if;
   end ensureATA;

   --  Lazy-initialize the second transitional block endpoint on first
   --  @nvme: open. Both disk drivers now expose Block.Device.V1; the hardware
   --  name and fixed endpoint slot remain boot-wiring debt in this unit only.
   procedure ensureNVMe (ok : out Boolean) is
      grantOk : Boolean;
   begin
      if nvmeInitialized then
         ok := True;
         return;
      end if;

      --  Discover NVMe driver PID via sysinfo
      if nvmeDriverPID = 0 then
         nvmeDriverPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_NVME);
         if nvmeDriverPID = 0 or nvmeDriverPID = Unsigned_64'Last then
            nvmeDriverPID := 0;
            ok := False;
            return;
         end if;
      end if;

      --  Allocate a page-aligned grant buffer via sbrk
      if nvmeGrantBuf = System.Null_Address then
         declare
            raw       : Unsigned_64;
            aligned   : Unsigned_64;
            allocSize : constant Unsigned_64 :=
              Unsigned_64 (NVME_GRANT_PAGES) * FS_PAGE_SIZE + FS_PAGE_SIZE;
         begin
            raw := syscall (SYSCALL_SBRK, allocSize);
            if raw = Unsigned_64'Last then
               debugPrint ("FS Server: sbrk failed for NVMe grant buffer." & LF);
               ok := False;
               return;
            end if;
            aligned := (raw + FS_PAGE_SIZE - 1) and not (FS_PAGE_SIZE - 1);
            nvmeGrantBuf := toAddr (aligned);
         end;
      end if;

      --  Create a grant to the NVMe driver for data transfer
      CuBit.Memory_Grants.Create_Via_Capability
        (slot      => CAP_SLOT_NVME,
         localAddr => nvmeGrantBuf,
         numPages  => NVME_GRANT_PAGES,
         readWrite => True,
         reference => nvmeGrant,
         success   => grantOk);

      if not grantOk then
         debugPrint ("FS Server: Failed to create NVMe grant." & LF);
         ok := False;
         return;
      end if;

      debugPrint ("FS Server: NVMe grant OK, initializing ext2..." & LF);

      Ext2.initBlockDevice
        (nvmeFs, CAP_SLOT_NVME, nvmeGrant, nvmeGrantBuf,
         NVME_GRANT_PAGES * 4096, ok);

      if ok then
         nvmeInitialized := True;
         debugPrint ("FS Server: NVMe ext2 filesystem initialized." & LF);
      else
         debugPrint ("FS Server: NVMe ext2 init failed." & LF);
         CuBit.Memory_Grants.Revoke (nvmeGrant, grantOk);
      end if;
   end ensureNVMe;

   --  Reject paths containing ".." traversal components.
   --  Checks for: exact "..", leading "../", trailing "/..", and "/../".
   function hasTraversal (path : String) return Boolean is
   begin
      if path'Length = 2 and then
         path (path'First) = '.' and then path (path'First + 1) = '.'
      then
         return True;
      end if;

      for i in path'First .. path'Last - 1 loop
         if path (i) = '.' and then path (i + 1) = '.' then
            declare
               atStart : constant Boolean :=
                 (i = path'First) or else path (i - 1) = '/';
               atEnd   : constant Boolean :=
                 (i + 1 = path'Last) or else path (i + 2) = '/';
            begin
               if atStart and atEnd then
                  return True;
               end if;
            end;
         end if;
      end loop;

      return False;
   end hasTraversal;

   --  Handle OP_OPEN
   --  words(0) = grant slot (where path string is)
   --  words(1) = path_length
   --  words(2) = flags (unused for now)
   --  words(3) = grant generation
   procedure handleOpen (sender : ProcessID; msg : Message) is
      pathLen   : constant Unsigned_64 := msg.words (1);
      openFlags : constant Open_Options := Open_Options (msg.words (2));
      grantAddr : System.Address := System.Null_Address;
      grantOk   : Boolean := False;
      returned  : Boolean := False;
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));

      handle     : Integer;
      handleId   : Unsigned_64;
      allocated  : Boolean;
      inodeNum   : Unsigned_32 := 0;
      scheme     : SchemeKind;
      relStart   : Natural;
      useBackend : BackendKind := CPIO_RAMDISK;
      cpioIdx    : Natural := 0;
   begin
      if msg.tag.length /= 4 or else
         pathLen = 0 or else pathLen > MAXIMUM_PATH_BYTES or else
         not Valid_Open_Options (openFlags)
      then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (0), msg.words (3), pathLen,
         CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
      if not grantOk then
         sendReply (sender, REPLY_ACCESS_DENIED, Unsigned_64'Last);
         return;
      end if;

      declare
         grantedPath : String (1 .. Natural (pathLen))
           with Import, Address => grantAddr;
      begin
         pathBuffer (1 .. Natural (pathLen)) := grantedPath;
      end;
      returnClientMemory (msg.words (0), msg.words (3), returned);
      if not returned then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      --  Read path from grant buffer and parse scheme prefix
      declare
         pathStr : String renames pathBuffer (1 .. Natural (pathLen));

         --  Helper: skip optional device selector "0/" after scheme prefix
         procedure skipSelector
           (relPath : String;
            skipIdx : out Natural)
         is
         begin
            skipIdx := relPath'First;
            if relPath'Length > 0 and then
               relPath (relPath'First) in '0' .. '9'
            then
               skipIdx := relPath'First + 1;
               if skipIdx <= relPath'Last and then
                  relPath (skipIdx) = '/'
               then
                  skipIdx := skipIdx + 1;
               end if;
            end if;
         end skipSelector;
      begin
         if hasTraversal (pathStr) then
            sendReply (sender, REPLY_ERR, Unsigned_64'Last);
            return;
         end if;

         declare
            requiredRights : Unsigned_8 := 0;
         begin
            if Requests_Read (openFlags) then
               requiredRights := requiredRights or ACL_READ;
            end if;
            if Requests_Write (openFlags) then
               requiredRights := requiredRights or ACL_WRITE;
            end if;

            if (openFlags and OPEN_CREATE) /= 0 then
               requiredRights := requiredRights or ACL_WRITE or ACL_CREATE;
            end if;

            if not checkAccess (sender, pathStr, requiredRights) then
               debugPrint ("FS: access denied for PID" & LF);
               sendReply (sender, REPLY_ACCESS_DENIED, Unsigned_64'Last);
               return;
            end if;
         end;

         parseScheme (pathStr, scheme, relStart);

         if scheme = MEMORY_SCHEME then
            if not memoryInitialized then
               sendReply (sender, REPLY_ERR, Unsigned_64'Last);
               return;
            end if;

            useBackend := EXT2_MEMORY;
            declare
               relPath : String renames
                 pathStr (relStart .. Natural (pathLen));
               skipIdx : Natural;
            begin
               skipSelector (relPath, skipIdx);
               if skipIdx > relPath'Last then
                  inodeNum := Ext2.ROOT_INODE;
               else
                  inodeNum := Ext2.resolvePath
                    (memoryFs, relPath (skipIdx .. relPath'Last));
               end if;
            end;

         elsif scheme = NVME_SCHEME then
            useBackend := EXT2_NVME;

            --  Lazy-init NVMe filesystem
            declare
               ok : Boolean;
            begin
               ensureNVMe (ok);
               if not ok then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;
            end;

            --  Resolve path on NVMe filesystem
            declare
               relPath : String renames
                 pathStr (relStart .. Natural (pathLen));
               skipIdx : Natural;
            begin
               skipSelector (relPath, skipIdx);
               if skipIdx > relPath'Last then
                  inodeNum := Ext2.ROOT_INODE;
               else
                  inodeNum := Ext2.resolvePath
                    (nvmeFs, relPath (skipIdx .. relPath'Last));
               end if;
            end;

         elsif scheme = ATA_SCHEME then
            useBackend := EXT2_ATA;

            --  Lazy-init ATA filesystem
            declare
               ok : Boolean;
            begin
               ensureATA (ok);
               if not ok then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;
            end;

            --  Resolve path on ATA filesystem (skip selector/prefix)
            declare
               relPath : String renames
                 pathStr (relStart .. Natural (pathLen));
               skipIdx : Natural;
            begin
               skipSelector (relPath, skipIdx);
               if skipIdx > relPath'Last then
                  inodeNum := Ext2.ROOT_INODE;
               else
                  inodeNum := Ext2.resolvePath
                    (ataFs, relPath (skipIdx .. relPath'Last));
               end if;
            end;
         else
            --  No scheme prefix: immutable bootstrap first, then the writable
            --  memory overlay, followed by persistent disks.
            if cpioOk then
               cpioIdx := Cpio.findFile (cpioArchive, pathStr);
               if cpioIdx < cpioArchive.count then
                  useBackend := CPIO_RAMDISK;
                  inodeNum := 1;
               end if;
            end if;

            if inodeNum = 0 and then memoryInitialized then
               useBackend := EXT2_MEMORY;
               inodeNum := Ext2.resolvePath (memoryFs, pathStr);
            end if;

            --  Fallback to disk if not found in ramdisk
            if inodeNum = 0 then
               declare
                  ok : Boolean;
               begin
                  ensureATA (ok);
                  if ok then
                     inodeNum := Ext2.resolvePath (ataFs, pathStr);
                     if inodeNum /= 0 then
                        useBackend := EXT2_ATA;
                     end if;
                  end if;
               end;
            end if;

            if inodeNum = 0 then
               declare
                  ok : Boolean;
               begin
                  ensureNVMe (ok);
                  if ok then
                     inodeNum := Ext2.resolvePath (nvmeFs, pathStr);
                     if inodeNum /= 0 then
                        useBackend := EXT2_NVME;
                     end if;
                  end if;
               end;
            end if;
         end if;
      end;

      if inodeNum = 0 then
         --  OPEN_CREATE: create the file if it doesn't exist
         if (openFlags and OPEN_CREATE) /= 0 and
            useBackend in EXT2_MEMORY | EXT2_ATA | EXT2_NVME
         then
            declare
               pathStr : String renames
                 pathBuffer (1 .. Natural (pathLen));

               --  Strip scheme prefix (@ata:, @nvme:) and device selector
               --  (0/) to get the pure filesystem path.
               relPath   : String renames
                 pathStr (relStart .. Natural (pathLen));
               fileStart : Natural := relPath'First;
               dirEnd    : Natural := 0;
               nameFirst : Natural;
            begin
               --  Skip device selector (e.g. "0/")
               if relPath'Length > 0 and then
                  relPath (relPath'First) in '0' .. '9'
               then
                  fileStart := relPath'First + 1;
                  if fileStart <= relPath'Last and then
                     relPath (fileStart) = '/'
                  then
                     fileStart := fileStart + 1;
                  end if;
               end if;

               if fileStart > relPath'Last then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;

               --  Find last '/' in the relative path to split dir/name
               for i in reverse fileStart .. relPath'Last loop
                  if relPath (i) = '/' then
                     dirEnd := i;
                     exit;
                  end if;
               end loop;

               if dirEnd = 0 then
                  nameFirst := fileStart;
               else
                  nameFirst := dirEnd + 1;
               end if;

               if nameFirst > relPath'Last then
                  sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                  return;
               end if;

               declare
                  dirInodeNum : Unsigned_32 := Ext2.ROOT_INODE;
               begin
                  --  Resolve parent directory (relative path only)
                  if dirEnd > 0 then
                     if useBackend = EXT2_MEMORY then
                        dirInodeNum := Ext2.resolvePath
                          (memoryFs, relPath (fileStart .. dirEnd - 1));
                     elsif useBackend = EXT2_ATA then
                        dirInodeNum := Ext2.resolvePath
                          (ataFs, relPath (fileStart .. dirEnd - 1));
                     elsif useBackend = EXT2_NVME then
                        dirInodeNum := Ext2.resolvePath
                          (nvmeFs, relPath (fileStart .. dirEnd - 1));
                     end if;
                  end if;

                  if dirInodeNum = 0 then
                     debugPrint ("FS: parent dir not found" & LF);
                     sendReply (sender, REPLY_ERR, Unsigned_64'Last);
                     return;
                  end if;

                  if useBackend = EXT2_MEMORY then
                     inodeNum := Ext2.createFile
                       (memoryFs, dirInodeNum,
                        relPath (nameFirst .. relPath'Last),
                        Ext2.FILETYPE_REGULAR);
                  elsif useBackend = EXT2_ATA then
                     inodeNum := Ext2.createFile
                       (ataFs, dirInodeNum,
                        relPath (nameFirst .. relPath'Last),
                        Ext2.FILETYPE_REGULAR);
                  elsif useBackend = EXT2_NVME then
                     inodeNum := Ext2.createFile
                       (nvmeFs, dirInodeNum,
                        relPath (nameFirst .. relPath'Last),
                        Ext2.FILETYPE_REGULAR);
                  end if;
               end;
            end;
         end if;

         if inodeNum = 0 then
            debugPrint ("FS: file not found" & LF);
            sendReply (sender, REPLY_ERR, Unsigned_64'Last);
            return;
         end if;
      end if;

      --  A file handle must never be an alternate spelling of directory
      --  authority. Directories are opened only through OP_OPEN_DIRECTORY.
      if useBackend /= CPIO_RAMDISK then
         declare
            objectInode : Ext2.Inode;
         begin
            case useBackend is
               when EXT2_MEMORY =>
                  Ext2.readInode (memoryFs, inodeNum, objectInode);
               when EXT2_ATA =>
                  Ext2.readInode (ataFs, inodeNum, objectInode);
               when EXT2_NVME =>
                  Ext2.readInode (nvmeFs, inodeNum, objectInode);
               when CPIO_RAMDISK => null;
            end case;
            if Ext2.inodeType (objectInode) = Ext2.INODE_DIRECTORY then
               sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
               return;
            end if;
         end;
      end if;

      --  OPEN_TRUNCATE: truncate existing file to zero length
      if (openFlags and OPEN_TRUNCATE) /= 0 and inodeNum /= 0 then
         if useBackend = EXT2_MEMORY then
            Ext2.truncateFile (memoryFs, inodeNum, 0);
         elsif useBackend = EXT2_ATA then
            Ext2.truncateFile (ataFs, inodeNum, 0);
         elsif useBackend = EXT2_NVME then
            Ext2.truncateFile (nvmeFs, inodeNum, 0);
         end if;
      end if;

      --  Allocate file handle
      allocHandle (handleId, handle, allocated);
      if not allocated then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      --  Set up file entry with backend tracking
      files (handle).active      := True;
      files (handle).inodeNum    := inodeNum;
      files (handle).offset      := 0;
      files (handle).ownerPID    := sender;
      files (handle).backend     := useBackend;
      files (handle).cpioFileIdx := cpioIdx;
      files (handle).openRights := 0;
      files (handle).objectKind := FILE_OBJECT;
      if Requests_Read (openFlags) then
         files (handle).openRights :=
           files (handle).openRights or ACL_READ;
      end if;
      if Requests_Write (openFlags) then
         files (handle).openRights :=
           files (handle).openRights or ACL_WRITE;
      end if;

      case useBackend is
         when CPIO_RAMDISK =>
            null;  --  cpio files don't need inode
         when EXT2_MEMORY =>
            Ext2.readInode (memoryFs, inodeNum, files (handle).ino);
         when EXT2_ATA =>
            Ext2.readInode (ataFs, inodeNum, files (handle).ino);
         when EXT2_NVME =>
            Ext2.readInode (nvmeFs, inodeNum, files (handle).ino);
      end case;

      --  Reply with handle in words(0) and file size in words(1)
      declare
         fsize    : Unsigned_64 := 0;
         replyMsg : Message;
         ignore   : Unsigned_64;
      begin
         case useBackend is
            when CPIO_RAMDISK =>
               fsize := cpioArchive.files (cpioIdx).dataSize;
            when EXT2_MEMORY =>
               fsize := Ext2.fileSize (files (handle).ino);
            when EXT2_ATA =>
               fsize := Ext2.fileSize (files (handle).ino);
            when EXT2_NVME =>
               fsize := Ext2.fileSize (files (handle).ino);
         end case;

         replyMsg.tag := (label  => REPLY_OK,
                          length => 2,
                          flags  => 0,
                          badge  => 0);
         replyMsg.words := (0 => handleId,
                            1 => fsize,
                            others => 0);
         ignore := reply (sender, replyMsg);
      end;
   end handleOpen;

   --  Handle OP_READ
   --  words(0) = file_handle
   --  words(1) = grant slot (buffer to write data into)
   --  words(2) = count (bytes to read)
   --  words(3) = grant generation
   procedure handleRead (sender : ProcessID; msg : Message) is
      handle    : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      count     : constant Unsigned_64 := msg.words (2);
      grantAddr : System.Address := System.Null_Address;
      grantOk   : Boolean := False;
      bytesRead : Unsigned_64;
      readStatus : Ext2.Read_Status := Ext2.Read_Complete;
      returned  : Boolean := False;

      function replyForRead
        (status : Ext2.Read_Status) return Unsigned_32
      is
      begin
         case status is
            when Ext2.Read_Complete =>
               return REPLY_OK;
            when Ext2.Read_Out_Of_Range =>
               return REPLY_OUT_OF_RANGE;
            when Ext2.Read_Device_Error =>
               return REPLY_IO_ERROR;
            when Ext2.Read_File_Range_Unsupported =>
               return REPLY_FILE_RANGE_UNSUPPORTED;
         end case;
      end replyForRead;
   begin
      if msg.tag.length /= 4 or else handle < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if (files (handle).openRights and ACL_READ) = 0 then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      if count = 0 then
         sendReply (sender, REPLY_OK, 0);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (1), msg.words (3), count,
         CuBit.Memory_Grants.Write_Access, grantAddr, grantOk);
      if not grantOk then
         debugPrint ("FS: read grant acquisition denied" & LF);
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      case files (handle).backend is
         when CPIO_RAMDISK =>
            bytesRead := Cpio.readData
              (cpioArchive,
               files (handle).cpioFileIdx,
               files (handle).offset,
               grantAddr,
               count);
            readStatus := Ext2.Read_Complete;
         when EXT2_MEMORY =>
            Ext2.readData
              (memoryFs,
               files (handle).ino,
               files (handle).offset,
               grantAddr,
               count,
               bytesRead,
               readStatus);
         when EXT2_ATA =>
            Ext2.readData
              (ataFs,
               files (handle).ino,
               files (handle).offset,
               grantAddr,
               count,
               bytesRead,
               readStatus);
         when EXT2_NVME =>
            Ext2.readData
              (nvmeFs,
               files (handle).ino,
               files (handle).offset,
               grantAddr,
               count,
               bytesRead,
               readStatus);
      end case;

      files (handle).offset := files (handle).offset + bytesRead;

      returnClientMemory (msg.words (1), msg.words (3), returned);
      if not returned then
         debugPrint ("FS: read grant return failed" & LF);
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      sendReply (sender, replyForRead (readStatus), bytesRead);
   end handleRead;

   --  Handle OP_WRITE
   --  words(0) = file_handle
   --  words(1) = grant slot (buffer containing data to write)
   --  words(2) = count (bytes to write)
   --  words(3) = grant generation
   procedure handleWrite (sender : ProcessID; msg : Message) is
      handle       : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      count        : constant Unsigned_64 := msg.words (2);
      grantAddr    : System.Address := System.Null_Address;
      grantOk      : Boolean := False;
      bytesWritten : Unsigned_64;
      writeStatus  : Ext2.Write_Status := Ext2.Write_Complete;
      returned     : Boolean := False;

      function replyForWrite
        (status : Ext2.Write_Status) return Unsigned_32
      is
      begin
         case status is
            when Ext2.Write_Complete =>
               return REPLY_OK;
            when Ext2.Write_Read_Only =>
               return REPLY_READ_ONLY;
            when Ext2.Write_Out_Of_Range =>
               return REPLY_OUT_OF_RANGE;
            when Ext2.Write_Device_Error =>
               return REPLY_IO_ERROR;
            when Ext2.Write_No_Space =>
               return REPLY_NO_SPACE;
            when Ext2.Write_File_Range_Unsupported =>
               return REPLY_FILE_RANGE_UNSUPPORTED;
         end case;
      end replyForWrite;
   begin
      if msg.tag.length /= 4 or else handle < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if (files (handle).openRights and ACL_WRITE) = 0 then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      if count = 0 then
         sendReply (sender, REPLY_OK, 0);
         return;
      end if;

      if files (handle).backend = CPIO_RAMDISK then
         -- CPIO bootstrap storage is immutable; reject before acquiring the
         -- caller's memory so every successful acquisition has one exit.
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (1), msg.words (3), count,
         CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
      if not grantOk then
         debugPrint ("FS: write grant acquisition denied" & LF);
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      case files (handle).backend is
         when CPIO_RAMDISK =>
            bytesWritten := 0; -- Rejected above.
         when EXT2_MEMORY =>
            Ext2.writeData
              (memoryFs,
               files (handle).inodeNum,
               files (handle).ino,
               files (handle).offset,
               grantAddr,
               count,
               bytesWritten,
               writeStatus);
         when EXT2_ATA =>
            Ext2.writeData
              (ataFs,
               files (handle).inodeNum,
               files (handle).ino,
               files (handle).offset,
               grantAddr,
               count,
               bytesWritten,
               writeStatus);
         when EXT2_NVME =>
            Ext2.writeData
              (nvmeFs,
               files (handle).inodeNum,
               files (handle).ino,
               files (handle).offset,
               grantAddr,
               count,
               bytesWritten,
               writeStatus);
      end case;

      --  The completed prefix is part of the file even when a later block
      --  fails.  Keep the handle synchronized with that committed progress.
      files (handle).offset := files (handle).offset + bytesWritten;

      returnClientMemory (msg.words (1), msg.words (3), returned);
      if not returned then
         debugPrint ("FS: write grant return failed" & LF);
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      sendReply (sender, replyForWrite (writeStatus), bytesWritten);
   end handleWrite;

   --  Handle OP_SEEK
   --  words(0) = file_handle
   --  words(1) = offset
   --  words(2) = whence (0=SET, 1=CUR, 2=END)
   procedure handleSeek (sender : ProcessID; msg : Message) is
      handle  : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
      seekOff : constant Unsigned_64 := msg.words (1);
      whence  : constant Unsigned_64 := msg.words (2);
      newOff  : Unsigned_64;
      size    : Unsigned_64;
   begin
      if handle < 0 then
         sendReply (sender, REPLY_ERR, Unsigned_64'Last);
         return;
      end if;

      case files (handle).backend is
         when CPIO_RAMDISK =>
            size := cpioArchive.files (files (handle).cpioFileIdx).dataSize;
         when EXT2_MEMORY | EXT2_ATA | EXT2_NVME =>
            size := Ext2.fileSize (files (handle).ino);
      end case;

      case whence is
         when 0 =>  --  SEEK_SET
            newOff := seekOff;
         when 1 =>  --  SEEK_CUR
            newOff := files (handle).offset + seekOff;
         when 2 =>  --  SEEK_END
            newOff := size + seekOff;
         when others =>
            sendReply (sender, REPLY_ERR, Unsigned_64'Last);
            return;
      end case;

      files (handle).offset := newOff;
      sendReply (sender, REPLY_OK, newOff);
   end handleSeek;

   --  Handle OP_CLOSE
   --  words(0) = file_handle
   procedure handleClose (sender : ProcessID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, FILE_OBJECT);
   begin
      if handle < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      releaseHandle (handle);
      sendReply (sender, REPLY_OK, 0);
   end handleClose;

   --  Open a directory by bootstrap path.  The returned object is a distinct
   --  PID-bound directory handle; subsequent enumeration carries no path.
   procedure handleOpenDirectory (sender : ProcessID; msg : Message) is
      pathLen : constant Unsigned_64 := msg.words (1);
      grantAddr : System.Address := System.Null_Address;
      grantOk : Boolean := False;
      returned : Boolean := False;
      pathBuffer : String (1 .. Natural (MAXIMUM_PATH_BYTES));
      scheme : SchemeKind := AUTOMATIC_SCHEME;
      relStart : Natural := 1;
      backend : BackendKind := CPIO_RAMDISK;
      inodeNum : Unsigned_32 := 0;
      dirIno : Ext2.Inode;
      handleSlot : Integer;
      handleId : Unsigned_64;
      allocated : Boolean;

      procedure resolveExt2Directory
        (theFs : Ext2.Filesystem;
         path : String)
      is
         first : Natural := path'First;
      begin
         if path'Length > 0 and then path (path'First) in '0' .. '9' then
            first := path'First + 1;
            if first <= path'Last and then path (first) = '/' then
               first := first + 1;
            end if;
         end if;

         if first > path'Last then
            inodeNum := Ext2.ROOT_INODE;
         else
            inodeNum := Ext2.resolvePath (theFs, path (first .. path'Last));
         end if;
      end resolveExt2Directory;
   begin
      if msg.tag.length /= 3 or else pathLen > MAXIMUM_PATH_BYTES then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if pathLen > 0 then
         acquireClientMemory
           (sender, msg.words (0), msg.words (2), pathLen,
            CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
         if not grantOk then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;
         declare
            grantedPath : String (1 .. Natural (pathLen))
              with Import, Address => grantAddr;
         begin
            pathBuffer (1 .. Natural (pathLen)) := grantedPath;
         end;
         returnClientMemory (msg.words (0), msg.words (2), returned);
         if not returned then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;
      end if;

      declare
         path : String renames pathBuffer (1 .. Natural (pathLen));
         onlySeparators : Boolean := True;
      begin
         if pathLen > 0 then
            if hasTraversal (path) then
               sendReply (sender, REPLY_ERR, 0);
               return;
            end if;
            if not checkAccess (sender, path, ACL_READ) then
               sendReply (sender, REPLY_ACCESS_DENIED, 0);
               return;
            end if;
            parseScheme (path, scheme, relStart);
            for index in path'Range loop
               if path (index) /= '/' then
                  onlySeparators := False;
                  exit;
               end if;
            end loop;
         elsif not checkAccess (sender, "", ACL_READ) then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;

         if pathLen = 0 or else
           (scheme = AUTOMATIC_SCHEME and then onlySeparators)
         then
            if not cpioOk then
               sendReply (sender, REPLY_ERR, 0);
               return;
            end if;
            backend := CPIO_RAMDISK;
            inodeNum := 1;
         elsif scheme = MEMORY_SCHEME then
            if not memoryInitialized then
               sendReply (sender, REPLY_ERR, 0);
               return;
            end if;
            backend := EXT2_MEMORY;
            resolveExt2Directory
              (memoryFs, path (relStart .. path'Last));
         elsif scheme = ATA_SCHEME then
            declare
               ok : Boolean;
            begin
               ensureATA (ok);
               if not ok then
                  sendReply (sender, REPLY_IO_ERROR, 0);
                  return;
               end if;
            end;
            backend := EXT2_ATA;
            resolveExt2Directory (ataFs, path (relStart .. path'Last));
         elsif scheme = NVME_SCHEME then
            declare
               ok : Boolean;
            begin
               ensureNVMe (ok);
               if not ok then
                  sendReply (sender, REPLY_IO_ERROR, 0);
                  return;
               end if;
            end;
            backend := EXT2_NVME;
            resolveExt2Directory (nvmeFs, path (relStart .. path'Last));
         else
            sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
            return;
         end if;
      end;

      if inodeNum = 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      if backend /= CPIO_RAMDISK then
         case backend is
            when EXT2_MEMORY => Ext2.readInode (memoryFs, inodeNum, dirIno);
            when EXT2_ATA => Ext2.readInode (ataFs, inodeNum, dirIno);
            when EXT2_NVME => Ext2.readInode (nvmeFs, inodeNum, dirIno);
            when CPIO_RAMDISK => null;
         end case;
         if Ext2.inodeType (dirIno) /= Ext2.INODE_DIRECTORY then
            sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
            return;
         end if;
      end if;

      allocHandle (handleId, handleSlot, allocated);
      if not allocated then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      files (handleSlot).active := True;
      files (handleSlot).backend := backend;
      files (handleSlot).inodeNum := inodeNum;
      if backend /= CPIO_RAMDISK then
         files (handleSlot).ino := dirIno;
      end if;
      files (handleSlot).offset := 0;
      files (handleSlot).ownerPID := sender;
      files (handleSlot).openRights := ACL_READ;
      files (handleSlot).objectKind := DIRECTORY_OBJECT;

      declare
         replyMsg : Message := NULL_MESSAGE;
         ignored : Unsigned_64;
      begin
         replyMsg.tag :=
           (label => REPLY_OK, length => 2, flags => 0, badge => 0);
         replyMsg.words (0) := handleId;
         replyMsg.words (1) :=
           (if backend = CPIO_RAMDISK then 0 else
              Unsigned_64 (dirIno.generationNumber));
         ignored := reply (sender, replyMsg);
      end;
   end handleOpenDirectory;

   --  Return one Directory.Page.V1 into a fixed one-page writable grant.
   procedure handleReadDirectoryPage (sender : ProcessID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, DIRECTORY_OBJECT);
      grantAddr : System.Address := System.Null_Address;
      grantOk : Boolean := False;
      returned : Boolean := False;
      entryCount : Natural := 0;
      nextCursor : Unsigned_64 := 0;
      atEnd : Boolean := False;
      readStatus : Ext2.Directory_Read_Status := Ext2.Directory_Malformed;
      replyLabel : Unsigned_32 := REPLY_OK;
   begin
      if msg.tag.length /= 4 or else
         msg.words (2) /= Unsigned_64 (PROTOCOL_VERSION) or else handle < 0
      then
         sendReply
           (sender, (if handle < 0 then REPLY_WRONG_OBJECT_TYPE else
              REPLY_ERR), 0);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (1), msg.words (3), DIRECTORY_PAGE_BYTES,
         CuBit.Memory_Grants.Write_Access, grantAddr, grantOk);
      if not grantOk then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      declare
         rawPage : String (1 .. DIRECTORY_PAGE_BYTES)
           with Import, Address => grantAddr;
         header : Directory_Page_Header
           with Import, Address => grantAddr;
         pageEntries : Directory_Entries
           with Import,
                Address => grantAddr + DIRECTORY_PAGE_HEADER_BYTES;
      begin
         rawPage := (others => Character'Val (0));

         if files (handle).backend = CPIO_RAMDISK then
            if files (handle).offset > Unsigned_64 (cpioArchive.count) then
               replyLabel := REPLY_MALFORMED_FILESYSTEM;
            else
               nextCursor := files (handle).offset;
               while nextCursor < Unsigned_64 (cpioArchive.count) and then
                 entryCount < MAXIMUM_DIRECTORY_PAGE_ENTRIES
               loop
                  declare
                     archiveIndex : constant Natural := Natural (nextCursor);
                     nameLength : constant Natural :=
                       cpioArchive.files (archiveIndex).nameLen;
                  begin
                     if nameLength = 0 or else
                       nameLength > MAXIMUM_DIRECTORY_NAME_BYTES
                     then
                        replyLabel := REPLY_MALFORMED_FILESYSTEM;
                        exit;
                     end if;
                     declare
                        archiveName : String (1 .. nameLength)
                          with Import,
                               Address => cpioArchive.base + Storage_Offset
                                 (cpioArchive.files (archiveIndex).nameOff);
                     begin
                        pageEntries (entryCount).objectHint := nextCursor + 1;
                        pageEntries (entryCount).sizeBytes :=
                          cpioArchive.files (archiveIndex).dataSize;
                        pageEntries (entryCount).nameLength :=
                          Unsigned_16 (nameLength);
                        pageEntries (entryCount).kind := DIRECTORY_KIND_FILE;
                        pageEntries (entryCount).flags :=
                          DIRECTORY_ENTRY_SIZE_VALID;
                        for index in 1 .. nameLength loop
                           pageEntries (entryCount).name (index) :=
                             Unsigned_8 (Character'Pos (archiveName (index)));
                        end loop;
                     end;
                     entryCount := entryCount + 1;
                     nextCursor := nextCursor + 1;
                  end;
               end loop;
               atEnd := nextCursor = Unsigned_64 (cpioArchive.count);
            end if;
         else
            case files (handle).backend is
               when EXT2_MEMORY =>
                  Ext2.readDirectoryPage
                    (memoryFs, files (handle).ino, files (handle).offset,
                     pageEntries, entryCount, nextCursor, readStatus);
               when EXT2_ATA =>
                  Ext2.readDirectoryPage
                    (ataFs, files (handle).ino, files (handle).offset,
                     pageEntries, entryCount, nextCursor, readStatus);
               when EXT2_NVME =>
                  Ext2.readDirectoryPage
                    (nvmeFs, files (handle).ino, files (handle).offset,
                     pageEntries, entryCount, nextCursor, readStatus);
               when CPIO_RAMDISK => null;
            end case;

            case readStatus is
               when Ext2.Directory_Page_Complete => null;
               when Ext2.Directory_End => atEnd := True;
               when Ext2.Directory_Malformed =>
                  replyLabel := REPLY_MALFORMED_FILESYSTEM;
               when Ext2.Directory_Device_Error =>
                  replyLabel := REPLY_IO_ERROR;
               when Ext2.Directory_Out_Of_Range =>
                  replyLabel := REPLY_OUT_OF_RANGE;
               when Ext2.Directory_Range_Unsupported =>
                  replyLabel := REPLY_FILE_RANGE_UNSUPPORTED;
            end case;
         end if;

         header.version := PROTOCOL_VERSION;
         header.headerBytes := DIRECTORY_PAGE_HEADER_BYTES;
         header.entryBytes := DIRECTORY_ENTRY_BYTES;
         header.entryCount := Unsigned_16 (entryCount);
         header.flags := (if atEnd then DIRECTORY_PAGE_END else 0);
         header.reserved := 0;
         header.nextCursor := nextCursor;
         header.snapshot :=
           (if files (handle).backend = CPIO_RAMDISK then 0 else
              Unsigned_64 (files (handle).ino.generationNumber));
      end;

      returnClientMemory (msg.words (1), msg.words (3), returned);
      if not returned then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      if replyLabel = REPLY_OK then
         files (handle).offset := nextCursor;
      end if;
      sendReply (sender, replyLabel, Unsigned_64 (entryCount));
   end handleReadDirectoryPage;

   procedure handleCloseDirectory (sender : ProcessID; msg : Message) is
      handle : constant Integer :=
        resolveHandle (msg.words (0), sender, DIRECTORY_OBJECT);
   begin
      if msg.tag.length /= 1 or else handle < 0 then
         sendReply (sender, REPLY_WRONG_OBJECT_TYPE, 0);
         return;
      end if;
      releaseHandle (handle);
      sendReply (sender, REPLY_OK, 0);
   end handleCloseDirectory;

   --  Deferred reply table for async I/O (Phase 3 foundation)
   MAX_PENDING_CLIENTS : constant := 8;
   type PendingClient is record
      clientPID : ProcessID    := NO_PROCESS;
      handle    : Integer      := -1;
      destAddr  : System.Address := System.Null_Address;
      remaining : Unsigned_64  := 0;
      token     : Unsigned_64  := 0;
      active    : Boolean      := False;
   end record;
   pendingClients : array (0 .. MAX_PENDING_CLIENTS - 1) of PendingClient;

   --  Handle OP_RENAME
   --  words(0) = grant slot (buffer with both paths)
   --  words(1) = old_path_length
   --  words(2) = new_path_length
   --  words(3) = grant generation
   --  Grant buffer layout: [old_path][new_path]
   procedure handleRename (sender : ProcessID; msg : Message) is
      oldPathLen : constant Unsigned_64 := msg.words (1);
      newPathLen : constant Unsigned_64 := msg.words (2);
      grantAddr  : System.Address := System.Null_Address;
      grantOk    : Boolean := False;
      returned   : Boolean := False;
      pathBuffer : String
        (1 .. 2 * Natural (MAXIMUM_PATH_BYTES));
   begin
      if msg.tag.length /= 4 or else
         oldPathLen = 0 or else newPathLen = 0 or else
         oldPathLen > MAXIMUM_PATH_BYTES or else
         newPathLen > MAXIMUM_PATH_BYTES
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      acquireClientMemory
        (sender, msg.words (0), msg.words (3), oldPathLen + newPathLen,
         CuBit.Memory_Grants.Read_Access, grantAddr, grantOk);
      if not grantOk then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      declare
         totalLen : constant Natural := Natural (oldPathLen + newPathLen);
         grantedPaths : String (1 .. totalLen)
           with Import, Address => grantAddr;
      begin
         pathBuffer (1 .. totalLen) := grantedPaths;
      end;
      returnClientMemory (msg.words (0), msg.words (3), returned);
      if not returned then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      declare
         totalLen : constant Natural :=
           Natural (oldPathLen + newPathLen);
         bothPaths : String renames pathBuffer (1 .. totalLen);
         oldPath : String renames
           bothPaths (1 .. Natural (oldPathLen));
         newPath : String renames
           bothPaths (Natural (oldPathLen) + 1 .. totalLen);

         oldScheme, newScheme : SchemeKind;
         oldRelStart, newRelStart : Natural;
      begin
         if hasTraversal (oldPath) or else hasTraversal (newPath) then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;

         if not checkAccess (sender, oldPath, ACL_WRITE) or else
            not checkAccess (sender, newPath, ACL_WRITE)
         then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;

         parseScheme (oldPath, oldScheme, oldRelStart);
         parseScheme (newPath, newScheme, newRelStart);

         if oldScheme /= newScheme then
            sendReply (sender, REPLY_ERR, 0);
            return;
         end if;

         declare
            procedure skipSel (path : String; idx : out Natural) is
            begin
               idx := path'First;
               if path'Length > 0 and then
                  path (path'First) in '0' .. '9'
               then
                  idx := path'First + 1;
                  if idx <= path'Last and then
                     path (idx) = '/'
                  then
                     idx := idx + 1;
                  end if;
               end if;
            end skipSel;

            oldSkip, newSkip : Natural;
            ok : Boolean;
         begin
            if oldScheme in MEMORY_SCHEME | AUTOMATIC_SCHEME and then
               memoryInitialized
            then
               if not memoryInitialized then
                  sendReply (sender, REPLY_ERR, 0);
                  return;
               end if;

               skipSel (oldPath (oldRelStart .. oldPath'Last), oldSkip);
               skipSel (newPath (newRelStart .. newPath'Last), newSkip);

               declare
                  oldRel : String renames
                    oldPath (oldSkip .. oldPath'Last);
                  newRel : String renames
                    newPath (newSkip .. newPath'Last);
               begin
                  ok := Ext2.renameEntry
                    (memoryFs, Ext2.ROOT_INODE, oldRel, newRel);
               end;
            elsif oldScheme = NVME_SCHEME then
               declare
                  okInit : Boolean;
               begin
                  ensureNVMe (okInit);
                  if not okInit then
                     sendReply (sender, REPLY_ERR, 0);
                     return;
                  end if;
               end;

               skipSel (oldPath (oldRelStart .. oldPath'Last),
                        oldSkip);
               skipSel (newPath (newRelStart .. newPath'Last),
                        newSkip);

               declare
                  oldRel : String renames
                    oldPath (oldSkip .. oldPath'Last);
                  newRel : String renames
                    newPath (newSkip .. newPath'Last);
               begin
                  ok := Ext2.renameEntry
                    (nvmeFs, Ext2.ROOT_INODE, oldRel, newRel);
               end;
            elsif oldScheme = ATA_SCHEME then
               declare
                  okInit : Boolean;
               begin
                  ensureATA (okInit);
                  if not okInit then
                     sendReply (sender, REPLY_ERR, 0);
                     return;
                  end if;
               end;

               skipSel (oldPath (oldRelStart .. oldPath'Last),
                        oldSkip);
               skipSel (newPath (newRelStart .. newPath'Last),
                        newSkip);

               declare
                  oldRel : String renames
                    oldPath (oldSkip .. oldPath'Last);
                  newRel : String renames
                    newPath (newSkip .. newPath'Last);
               begin
                  ok := Ext2.renameEntry
                    (ataFs, Ext2.ROOT_INODE, oldRel, newRel);
               end;
            else
               ok := False;
            end if;

            if ok then
               sendReply (sender, REPLY_OK, 0);
            else
               sendReply (sender, REPLY_ERR, 0);
            end if;
         end;
      end;
   end handleRename;

   --  Main message loop variables
   sender : ProcessID;
   msg    : Message;
   rdAddr : Unsigned_64;
   rdSize : Unsigned_64;
begin
   debugPrint ("FS Server: Starting..." & LF);

   --  Get ramdisk address and size from kernel
   rdAddr := getInfo (SYSINFO_RAMDISK_ADDRESS);
   rdSize := getInfo (SYSINFO_RAMDISK_SIZE);
   if rdAddr = 0 or rdAddr = Unsigned_64'Last then
      debugPrint ("FS Server: No ramdisk found, disk-only mode." & LF);
      cpioOk := False;
   else
      debugPrint ("FS Server: Got ramdisk, initializing CPIO..." & LF);

      Cpio.init (cpioArchive, toAddr (rdAddr), rdSize, cpioOk);
      if not cpioOk then
         debugPrint ("FS Server: Invalid CPIO archive on ramdisk." & LF);
      else
         declare
            imageIndex : constant Natural :=
              Cpio.findFile (cpioArchive, "live-rw.ext2");
            imageBase : System.Address;
            imageSize : Unsigned_64;
            viewOk : Boolean;
            writableBase : Unsigned_64;
         begin
            if imageIndex < cpioArchive.count then
               Cpio.fileView
                 (cpioArchive, imageIndex, imageBase, imageSize, viewOk);
               if viewOk then
                  writableBase := syscall (SYSCALL_SBRK, imageSize);
                  if writableBase /= Unsigned_64'Last then
                     declare
                        source : String (1 .. Natural (imageSize))
                          with Import, Address => imageBase;
                        destination : String (1 .. Natural (imageSize))
                          with Import, Address => toAddr (writableBase);
                     begin
                        destination := source;
                     end;

                     Ext2.initMemory
                       (memoryFs, toAddr (writableBase), imageSize,
                        memoryInitialized);
                  end if;
               end if;

               if memoryInitialized then
                  debugPrint
                    ("FS Server: Writable memory filesystem ready." & LF);
               else
                  debugPrint
                    ("FS Server: Invalid writable memory filesystem." & LF);
               end if;
            end if;
         end;
      end if;
   end if;

   --  Register as DRIVER_FS so other services can discover us
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_FS);
   end;

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      ignore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, badge => 0),
          capBadge => 0,
          words    => (others => 0)));
   end;

   debugPrint ("FS Server: Entering message loop." & LF);

   --  Main IPC message loop.
   --  Uses blocking receive for lowest latency.  Future async work
   --  can switch to Poll_Service_Request + Poll_Completion when capSubmit-based
   --  driver I/O is implemented.
   loop
      receive (sender, msg);

      case msg.tag.label is
         when OP_OPEN =>
            handleOpen (sender, msg);
         when OP_READ =>
            handleRead (sender, msg);
         when OP_WRITE =>
            handleWrite (sender, msg);
         when OP_SEEK =>
            handleSeek (sender, msg);
         when OP_CLOSE =>
            handleClose (sender, msg);
         when OP_OPEN_DIRECTORY =>
            handleOpenDirectory (sender, msg);
         when OP_READ_DIRECTORY_PAGE =>
            handleReadDirectoryPage (sender, msg);
         when OP_CLOSE_DIRECTORY =>
            handleCloseDirectory (sender, msg);
         when OP_SET_ACL =>
            handleSetACL (sender, msg);
         when OP_REVOKE_ACL =>
            handleRevokeACL (sender, msg);
         when OP_RENAME =>
            handleRename (sender, msg);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end loop;
end main;
