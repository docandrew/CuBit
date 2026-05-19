------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace Config Store Service
--
--  Hierarchical key-value store with capability-gated, per-app isolated
--  access. Follows the same IPC loop + ACL pattern as the FS server.
--
--  Capability slots:
--    1 = CAP_ENDPOINT to filesystem server (granted by devmgr)
--    7 = CAP_NOTIFICATION for DRIVER_CONFIG registration
--   15 = CAP_ENDPOINT to devmgr (for OP_READY signal)
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

procedure main is
   use ASCII;

   --  IPC operation labels
   OP_CONFIG_GET    : constant Unsigned_32 := 16#0600#;
   OP_CONFIG_SET    : constant Unsigned_32 := 16#0601#;
   OP_CONFIG_DELETE : constant Unsigned_32 := 16#0602#;
   OP_CONFIG_LIST   : constant Unsigned_32 := 16#0603#;
   OP_CONFIG_LOAD   : constant Unsigned_32 := 16#0604#;
   OP_CONFIG_SAVE   : constant Unsigned_32 := 16#0605#;
   OP_SET_ACL       : constant Unsigned_32 := 16#0080#;
   OP_REVOKE_ACL    : constant Unsigned_32 := 16#0081#;
   REPLY_OK            : constant Unsigned_32 := 16#F000#;
   REPLY_ERR           : constant Unsigned_32 := 16#F001#;
   REPLY_ACCESS_DENIED : constant Unsigned_32 := 16#F007#;

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096; -- 16 MiB

   --  FS IPC labels (for persistence)
   OP_OPEN  : constant Unsigned_32 := 16#0001#;
   OP_CLOSE : constant Unsigned_32 := 16#0002#;
   OP_READ  : constant Unsigned_32 := 16#0003#;
   OP_WRITE : constant Unsigned_32 := 16#0004#;
   OP_SEEK  : constant Unsigned_32 := 16#0006#;

   --  FS endpoint is at slot 1 (granted by devmgr)
   CAP_SLOT_FS_LOCAL : constant Unsigned_64 := 1;

   PAGE_SIZE : constant := 4096;

   --  FS grant state for persistence
   fsGrantBuf  : System.Address := System.Null_Address;
   fsGrantId   : Unsigned_64 := 0;
   fsReady     : Boolean := False;

   --  Persistence backing store path (read from "config.store" key)
   storePath    : String (1 .. 128);
   storePathLen : Natural := 0;

   --  KV store limits
   MAX_KEY_LEN   : constant := 128;
   MAX_VALUE_LEN : constant := 4096;
   MAX_ENTRIES   : constant := 256;

   type ValueArray is array (0 .. MAX_VALUE_LEN - 1) of Unsigned_8;

   type ConfigEntry is record
      active   : Boolean   := False;
      key      : String (1 .. MAX_KEY_LEN);
      keyLen   : Natural   := 0;
      value    : ValueArray := (others => 0);
      valueLen : Natural   := 0;
   end record;

   store : array (0 .. MAX_ENTRIES - 1) of ConfigEntry;

   ---------------------------------------------------------------------------
   --  Per-process ACL infrastructure (same pattern as FS server)
   ---------------------------------------------------------------------------

   ACL_READ  : constant Unsigned_8 := 1;
   ACL_WRITE : constant Unsigned_8 := 2;

   MAX_ACL_PREFIX   : constant := 64;
   MAX_ACL_ENTRIES  : constant := 16;
   MAX_ACL_PROFILES : constant := 32;

   type ACLEntry is record
      prefix    : String (1 .. MAX_ACL_PREFIX);
      prefixLen : Natural    := 0;  --  0 = wildcard (matches everything)
      rights    : Unsigned_8 := 0;
   end record;

   type ACLEntryArray is
     array (0 .. MAX_ACL_ENTRIES - 1) of ACLEntry;

   type ACLProfile is record
      pid     : ProcessID := NO_PROCESS;
      active  : Boolean   := False;
      count   : Natural   := 0;
      entries : ACLEntryArray;
   end record;

   aclProfiles : array (0 .. MAX_ACL_PROFILES - 1) of ACLProfile;

   --  Admin tracking: first OP_SET_ACL sender auto-becomes primary admin
   primaryAdmin : ProcessID   := NO_PROCESS;
   procmgrAdmin : Unsigned_64 := 0;

   --  Check if sender is an authorized admin (devmgr or procmgr)
   function isAdmin (sender : ProcessID) return Boolean is
   begin
      if primaryAdmin /= NO_PROCESS and then sender = primaryAdmin then
         return True;
      end if;

      --  Lazy-discover procmgr PID via sysinfo
      if procmgrAdmin = 0 then
         procmgrAdmin := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_PROCMGR);
         if procmgrAdmin = 0 or procmgrAdmin = Unsigned_64'Last then
            procmgrAdmin := 0;
         end if;
      end if;

      if procmgrAdmin /= 0 and then sender = procmgrAdmin then
         return True;
      end if;

      return False;
   end isAdmin;

   --  Check if sender has access rights for the given key
   function checkAccess
     (sender : ProcessID;
      key    : String;
      rights : Unsigned_8) return Boolean
   is
   begin
      if isAdmin (sender) then
         return True;
      end if;

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
                  if key'Length >=
                     aclProfiles (i).entries (j).prefixLen
                  then
                     declare
                        pLen : constant Natural :=
                          aclProfiles (i).entries (j).prefixLen;
                        match : Boolean := True;
                     begin
                        for k in 0 .. pLen - 1 loop
                           if key (key'First + k) /=
                              aclProfiles (i).entries (j).prefix (1 + k)
                           then
                              match := False;
                              exit;
                           end if;
                        end loop;
                        if match then
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

   ---------------------------------------------------------------------------
   --  Handle OP_SET_ACL (same protocol as FS server)
   --  words(0) = target PID
   --  words(1) = entry count (0 = wildcard full access)
   --  words(2) = grant_id (when count > 0)
   ---------------------------------------------------------------------------
   procedure handleSetACL (sender : ProcessID; msg : Message) is
      targetPID  : constant ProcessID := msg.words (0);
      entryCount : constant Natural := Natural (msg.words (1));
      slotIdx    : Integer := -1;
   begin
      --  Auto-register first sender as primary admin (devmgr)
      if primaryAdmin = NO_PROCESS then
         primaryAdmin := sender;
      end if;

      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      if entryCount > MAX_ACL_ENTRIES then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

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

      aclProfiles (slotIdx).pid    := targetPID;
      aclProfiles (slotIdx).active := True;

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
            grantId   : constant Unsigned_64 := msg.words (2);
            grantAddr : constant Unsigned_64 :=
              GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;
            buf : array (0 .. entryCount * 72 - 1) of Unsigned_8
              with Import, Address => toAddr (grantAddr);
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
      end if;

      debugPrint ("Config: ACL set for PID" & LF);
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

      sendReply (sender, REPLY_OK, 0);
   end handleRevokeACL;

   ---------------------------------------------------------------------------
   --  KV store helpers
   ---------------------------------------------------------------------------

   --  Check if key starts with "system/" prefix (7 chars)
   function isSystemKey (key : String) return Boolean is
   begin
      if key'Length < 7 then
         return False;
      end if;
      return key (key'First)     = 's' and then
             key (key'First + 1) = 'y' and then
             key (key'First + 2) = 's' and then
             key (key'First + 3) = 't' and then
             key (key'First + 4) = 'e' and then
             key (key'First + 5) = 'm' and then
             key (key'First + 6) = '/';
   end isSystemKey;

   --  Find a store entry by key. Returns index or -1 if not found.
   function findEntry (key : String) return Integer is
   begin
      for i in store'Range loop
         if store (i).active and then store (i).keyLen = key'Length then
            declare
               match : Boolean := True;
            begin
               for c in 0 .. key'Length - 1 loop
                  if store (i).key (1 + c) /= key (key'First + c) then
                     match := False;
                     exit;
                  end if;
               end loop;
               if match then
                  return i;
               end if;
            end;
         end if;
      end loop;
      return -1;
   end findEntry;

   --  Find a free store slot. Returns index or -1 if full.
   function allocEntry return Integer is
   begin
      for i in store'Range loop
         if not store (i).active then
            return i;
         end if;
      end loop;
      return -1;
   end allocEntry;

   ---------------------------------------------------------------------------
   --  Handle OP_CONFIG_GET
   --  words(0) = grantId
   --  words(1) = keyLen
   --  Reply: word0 = valueLen (or REPLY_ERR)
   ---------------------------------------------------------------------------
   procedure handleGet (sender : ProcessID; msg : Message) is
      grantId   : constant Unsigned_64 := msg.words (0);
      keyLen    : constant Natural := Natural (msg.words (1));
      grantAddr : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;
      idx : Integer;
   begin
      if keyLen = 0 or keyLen > MAX_KEY_LEN then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      --  Read key from grant buffer
      declare
         keyBuf : String (1 .. keyLen)
           with Import, Address => toAddr (grantAddr);
      begin
         if not checkAccess (sender, keyBuf, ACL_READ) then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;

         idx := findEntry (keyBuf);
      end;

      if idx < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      --  Write value to grant buffer
      declare
         outBuf : array (0 .. store (idx).valueLen - 1) of Unsigned_8
           with Import, Address => toAddr (grantAddr);
      begin
         for v in 0 .. store (idx).valueLen - 1 loop
            outBuf (v) := store (idx).value (v);
         end loop;
      end;

      sendReply (sender, REPLY_OK, Unsigned_64 (store (idx).valueLen));
   end handleGet;

   ---------------------------------------------------------------------------
   --  Handle OP_CONFIG_SET
   --  words(0) = grantId
   --  words(1) = keyLen
   --  words(2) = valueLen
   ---------------------------------------------------------------------------
   procedure handleSet (sender : ProcessID; msg : Message) is
      grantId   : constant Unsigned_64 := msg.words (0);
      keyLen    : constant Natural := Natural (msg.words (1));
      valueLen  : constant Natural := Natural (msg.words (2));
      grantAddr : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;
      idx : Integer;
   begin
      if keyLen = 0 or keyLen > MAX_KEY_LEN or valueLen > MAX_VALUE_LEN then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      --  Read key and value from grant buffer: key at offset 0, value at
      --  offset keyLen (packed sequentially).
      declare
         grantBuf : array (0 .. keyLen + valueLen - 1) of Unsigned_8
           with Import, Address => toAddr (grantAddr);
         keyStr : String (1 .. keyLen);
      begin
         --  Extract key
         for c in 0 .. keyLen - 1 loop
            keyStr (1 + c) := Character'Val (grantBuf (c));
         end loop;

         if not checkAccess (sender, keyStr, ACL_WRITE) then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;

         --  System namespace: reject writes unless admin
         if isSystemKey (keyStr) and then not isAdmin (sender) then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;

         --  Upsert: find existing or allocate new
         idx := findEntry (keyStr);
         if idx < 0 then
            idx := allocEntry;
            if idx < 0 then
               sendReply (sender, REPLY_ERR, 0);
               return;
            end if;
         end if;

         store (idx).active := True;
         store (idx).keyLen := keyLen;
         for c in 0 .. keyLen - 1 loop
            store (idx).key (1 + c) := keyStr (1 + c);
         end loop;

         store (idx).valueLen := valueLen;
         for v in 0 .. valueLen - 1 loop
            store (idx).value (v) := grantBuf (keyLen + v);
         end loop;
      end;

      sendReply (sender, REPLY_OK, 0);
   end handleSet;

   ---------------------------------------------------------------------------
   --  Handle OP_CONFIG_DELETE
   --  words(0) = grantId
   --  words(1) = keyLen
   ---------------------------------------------------------------------------
   procedure handleDelete (sender : ProcessID; msg : Message) is
      grantId   : constant Unsigned_64 := msg.words (0);
      keyLen    : constant Natural := Natural (msg.words (1));
      grantAddr : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;
      idx : Integer;
   begin
      if keyLen = 0 or keyLen > MAX_KEY_LEN then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      declare
         keyBuf : String (1 .. keyLen)
           with Import, Address => toAddr (grantAddr);
      begin
         if not checkAccess (sender, keyBuf, ACL_WRITE) then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;

         --  System namespace: reject deletes unless admin
         if isSystemKey (keyBuf) and then not isAdmin (sender) then
            sendReply (sender, REPLY_ACCESS_DENIED, 0);
            return;
         end if;

         idx := findEntry (keyBuf);
      end;

      if idx < 0 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      store (idx).active := False;
      sendReply (sender, REPLY_OK, 0);
   end handleDelete;

   ---------------------------------------------------------------------------
   --  Handle OP_CONFIG_LIST
   --  words(0) = grantId
   --  words(1) = prefixLen (0 = list all)
   --  Reply: word0 = count of matching keys
   --  Grant buffer filled with NUL-separated key strings.
   ---------------------------------------------------------------------------
   procedure handleList (sender : ProcessID; msg : Message) is
      grantId   : constant Unsigned_64 := msg.words (0);
      prefixLen : constant Natural := Natural (msg.words (1));
      grantAddr : constant Unsigned_64 :=
        GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE;
      writeOff  : Natural := 0;
      count     : Natural := 0;
      maxWrite  : constant Natural := Natural (GRANT_SLOT_SIZE);
   begin
      if prefixLen > MAX_KEY_LEN then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      --  Read prefix from grant buffer (if any) before we start writing
      declare
         prefix : String (1 .. Natural'Max (prefixLen, 1));
      begin
         if prefixLen > 0 then
            declare
               prefBuf : String (1 .. prefixLen)
                 with Import, Address => toAddr (grantAddr);
            begin
               for c in 1 .. prefixLen loop
                  prefix (c) := prefBuf (c);
               end loop;
            end;

            if not checkAccess (sender, prefix (1 .. prefixLen), ACL_READ)
            then
               sendReply (sender, REPLY_ACCESS_DENIED, 0);
               return;
            end if;
         else
            --  Empty prefix: check wildcard read access with empty string
            prefix (1) := Character'Val (0);  -- unused
            if not checkAccess (sender, "", ACL_READ) then
               sendReply (sender, REPLY_ACCESS_DENIED, 0);
               return;
            end if;
         end if;

         --  Write matching keys to grant buffer as NUL-separated strings
         declare
            outBuf : array (0 .. maxWrite - 1) of Unsigned_8
              with Import, Address => toAddr (grantAddr);
         begin
            for i in store'Range loop
               if store (i).active then
                  declare
                     match : Boolean := True;
                  begin
                     if prefixLen > 0 then
                        if store (i).keyLen < prefixLen then
                           match := False;
                        else
                           for c in 0 .. prefixLen - 1 loop
                              if store (i).key (1 + c) /=
                                 prefix (1 + c)
                              then
                                 match := False;
                                 exit;
                              end if;
                           end loop;
                        end if;
                     end if;

                     if match then
                        --  Check we have room: keyLen + NUL
                        if writeOff + store (i).keyLen + 1 > maxWrite then
                           exit;
                        end if;

                        for c in 0 .. store (i).keyLen - 1 loop
                           outBuf (writeOff + c) :=
                             Unsigned_8 (Character'Pos (
                               store (i).key (1 + c)));
                        end loop;
                        writeOff := writeOff + store (i).keyLen;
                        outBuf (writeOff) := 0;  --  NUL separator
                        writeOff := writeOff + 1;
                        count := count + 1;
                     end if;
                  end;
               end if;
            end loop;
         end;
      end;

      sendReply (sender, REPLY_OK, Unsigned_64 (count));
   end handleList;

   ---------------------------------------------------------------------------
   --  Internal set (no ACL check, used by handleLoad)
   ---------------------------------------------------------------------------
   procedure internalSet (key : String; val : String) is
      idx : Integer;
   begin
      if key'Length = 0 or key'Length > MAX_KEY_LEN or
         val'Length > MAX_VALUE_LEN
      then
         return;
      end if;

      idx := findEntry (key);
      if idx < 0 then
         idx := allocEntry;
         if idx < 0 then
            return;
         end if;
      end if;

      store (idx).active := True;
      store (idx).keyLen := key'Length;
      for c in 0 .. key'Length - 1 loop
         store (idx).key (1 + c) := key (key'First + c);
      end loop;

      store (idx).valueLen := val'Length;
      for v in 0 .. val'Length - 1 loop
         store (idx).value (v) :=
           Unsigned_8 (Character'Pos (val (val'First + v)));
      end loop;
   end internalSet;

   ---------------------------------------------------------------------------
   --  FS grant initialization (lazy, called from handleLoad/handleSave)
   ---------------------------------------------------------------------------
   procedure ensureFsGrant is
      rawAddr : Unsigned_64;
      aligned : Unsigned_64;
      fsPID   : Unsigned_64;
      gid     : Unsigned_64;
      ok      : Boolean;
   begin
      if fsReady then
         return;
      end if;

      --  Discover FS PID
      fsPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_FS);
      if fsPID = 0 or fsPID = Unsigned_64'Last then
         debugPrint ("Config: FS not registered" & LF);
         return;
      end if;

      --  Allocate 2 pages for page-alignment
      rawAddr := syscall (SYSCALL_SBRK, 2 * PAGE_SIZE);
      if rawAddr = Unsigned_64'Last then
         debugPrint ("Config: sbrk failed for FS grant" & LF);
         return;
      end if;

      aligned := (rawAddr + PAGE_SIZE - 1) and
                 not Unsigned_64 (PAGE_SIZE - 1);
      fsGrantBuf := To_Address (Integer_Address (aligned));

      --  Create grant to FS server (1 page RW)
      createGrant
        (grantee   => fsPID,
         localAddr => fsGrantBuf,
         numPages  => 1,
         readWrite => True,
         grantId   => gid,
         success   => ok);

      if not ok then
         debugPrint ("Config: FS grant creation failed" & LF);
         return;
      end if;

      fsGrantId := gid;
      fsReady := True;
      debugPrint ("Config: FS grant ready" & LF);
   end ensureFsGrant;

   ---------------------------------------------------------------------------
   --  Parse key=value lines from a buffer of the given length.
   --  Skips blank lines and lines starting with '#'.
   ---------------------------------------------------------------------------
   procedure parseKeyValueLines (buf : System.Address; len : Natural) is
      data : array (0 .. len - 1) of Unsigned_8
        with Import, Address => buf;
      pos      : Natural := 0;
      lineStart : Natural;
      lineEnd   : Natural;
      eqPos     : Integer;
   begin
      while pos < len loop
         --  Find line start (skip any leftover)
         lineStart := pos;

         --  Find end of line (LF or end of data)
         lineEnd := pos;
         while lineEnd < len and then data (lineEnd) /= 16#0A# loop
            lineEnd := lineEnd + 1;
         end loop;

         --  Trim trailing CR
         declare
            eol : Natural := lineEnd;
         begin
            if eol > lineStart and then data (eol - 1) = 16#0D# then
               eol := eol - 1;
            end if;

            --  Skip past newline for next iteration
            if lineEnd < len then
               pos := lineEnd + 1;
            else
               pos := len;
            end if;

            --  Skip blank lines and comments
            if eol > lineStart and then data (lineStart) /= 16#23# then
               --  Find '=' separator
               eqPos := -1;
               for e in lineStart .. eol - 1 loop
                  if data (e) = 16#3D# then  --  '='
                     eqPos := e;
                     exit;
                  end if;
               end loop;

               if eqPos > Integer (lineStart) and eqPos < Integer (eol) then
                  declare
                     kLen : constant Natural :=
                       Natural (eqPos) - lineStart;
                     vLen : constant Natural :=
                       eol - Natural (eqPos) - 1;
                     key : String (1 .. kLen);
                     val : String (1 .. vLen);
                  begin
                     for c in 0 .. kLen - 1 loop
                        key (1 + c) :=
                          Character'Val (data (lineStart + c));
                     end loop;
                     for c in 0 .. vLen - 1 loop
                        val (1 + c) :=
                          Character'Val (data (Natural (eqPos) + 1 + c));
                     end loop;
                     internalSet (key, val);
                  end;
               end if;
            end if;
         end;
      end loop;
   end parseKeyValueLines;

   ---------------------------------------------------------------------------
   --  Handle OP_CONFIG_LOAD
   --  Triggered by devmgr after seeding. Reads config.store key to find
   --  the backing file, then loads key=value pairs from disk.
   ---------------------------------------------------------------------------
   procedure handleLoad (sender : ProcessID) is
      fsMsg     : Message;
      fileHandle : Unsigned_64;
      fileSize   : Unsigned_64;
      bytesRead  : Unsigned_64;
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      --  Read "config.store" from own store to get the file path
      declare
         storeKey : constant String := "config.store";
         idx      : Integer;
      begin
         idx := findEntry (storeKey);
         if idx < 0 then
            debugPrint ("Config: no config.store key, skip load" & LF);
            sendReply (sender, REPLY_OK, 0);
            return;
         end if;

         storePathLen := store (idx).valueLen;
         if storePathLen > storePath'Length then
            storePathLen := storePath'Length;
         end if;
         for c in 0 .. storePathLen - 1 loop
            storePath (1 + c) :=
              Character'Val (store (idx).value (c));
         end loop;
      end;

      ensureFsGrant;
      if not fsReady then
         debugPrint ("Config: FS not ready, skip load" & LF);
         sendReply (sender, REPLY_OK, 0);
         return;
      end if;

      --  Open file: write path to fsGrantBuf, send OP_OPEN
      declare
         pathBuf : String (1 .. storePathLen)
           with Import, Address => fsGrantBuf;
      begin
         for c in 1 .. storePathLen loop
            pathBuf (c) := storePath (c);
         end loop;
      end;

      fsMsg := (tag => (label  => OP_OPEN,
                        length => 3,
                        flags  => 0,
                        badge  => 0),
                capBadge => 0,
                words => (0 => fsGrantId,
                          1 => Unsigned_64 (storePathLen),
                          2 => 0,
                          others => 0));
      fsMsg.tag := capCall (CAP_SLOT_FS_LOCAL, fsMsg);

      if fsMsg.tag.label /= REPLY_OK then
         debugPrint ("Config: load open failed" & LF);
         sendReply (sender, REPLY_OK, 0);
         return;
      end if;

      fileHandle := fsMsg.words (0);
      fileSize   := fsMsg.words (1);

      --  Clamp to grant buffer size
      if fileSize > PAGE_SIZE then
         fileSize := PAGE_SIZE;
      end if;

      --  Read file contents into fsGrantBuf
      fsMsg := (tag => (label  => OP_READ,
                        length => 3,
                        flags  => 0,
                        badge  => 0),
                capBadge => 0,
                words => (0 => fileHandle,
                          1 => fsGrantId,
                          2 => fileSize,
                          others => 0));
      fsMsg.tag := capCall (CAP_SLOT_FS_LOCAL, fsMsg);

      if fsMsg.tag.label /= REPLY_OK then
         debugPrint ("Config: load read failed" & LF);
         --  Close file before returning
         fsMsg := (tag => (label => OP_CLOSE, length => 1,
                           flags => 0, badge => 0),
                   capBadge => 0,
                   words => (0 => fileHandle, others => 0));
         fsMsg.tag := capCall (CAP_SLOT_FS_LOCAL, fsMsg);
         sendReply (sender, REPLY_OK, 0);
         return;
      end if;

      bytesRead := fsMsg.words (0);

      --  Parse loaded data (overwrites seeded defaults)
      if bytesRead > 0 then
         parseKeyValueLines (fsGrantBuf, Natural (bytesRead));
         debugPrint ("Config: loaded from disk" & LF);
      end if;

      --  Close file
      fsMsg := (tag => (label => OP_CLOSE, length => 1,
                        flags => 0, badge => 0),
                capBadge => 0,
                words => (0 => fileHandle, others => 0));
      fsMsg.tag := capCall (CAP_SLOT_FS_LOCAL, fsMsg);

      sendReply (sender, REPLY_OK, 0);
   end handleLoad;

   ---------------------------------------------------------------------------
   --  Handle OP_CONFIG_SAVE
   --  Serializes all active config entries to disk as key=value lines.
   ---------------------------------------------------------------------------
   procedure handleSave (sender : ProcessID) is
      fsMsg     : Message;
      fileHandle : Unsigned_64;
      writeOff   : Natural := 0;
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      --  Read "config.store" from own store
      if storePathLen = 0 then
         declare
            storeKey : constant String := "config.store";
            idx      : Integer;
         begin
            idx := findEntry (storeKey);
            if idx < 0 then
               debugPrint ("Config: no config.store, skip save" & LF);
               sendReply (sender, REPLY_OK, 0);
               return;
            end if;

            storePathLen := store (idx).valueLen;
            if storePathLen > storePath'Length then
               storePathLen := storePath'Length;
            end if;
            for c in 0 .. storePathLen - 1 loop
               storePath (1 + c) :=
                 Character'Val (store (idx).value (c));
            end loop;
         end;
      end if;

      ensureFsGrant;
      if not fsReady then
         debugPrint ("Config: FS not ready, skip save" & LF);
         sendReply (sender, REPLY_OK, 0);
         return;
      end if;

      --  Open file for writing
      declare
         pathBuf : String (1 .. storePathLen)
           with Import, Address => fsGrantBuf;
      begin
         for c in 1 .. storePathLen loop
            pathBuf (c) := storePath (c);
         end loop;
      end;

      fsMsg := (tag => (label  => OP_OPEN,
                        length => 3,
                        flags  => 0,
                        badge  => 0),
                capBadge => 0,
                words => (0 => fsGrantId,
                          1 => Unsigned_64 (storePathLen),
                          2 => 0,
                          others => 0));
      fsMsg.tag := capCall (CAP_SLOT_FS_LOCAL, fsMsg);

      if fsMsg.tag.label /= REPLY_OK then
         debugPrint ("Config: save open failed" & LF);
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      fileHandle := fsMsg.words (0);

      --  Seek to beginning
      fsMsg := (tag => (label  => OP_SEEK,
                        length => 3,
                        flags  => 0,
                        badge  => 0),
                capBadge => 0,
                words => (0 => fileHandle,
                          1 => 0,    --  offset 0
                          2 => 0,    --  SEEK_SET
                          others => 0));
      fsMsg.tag := capCall (CAP_SLOT_FS_LOCAL, fsMsg);

      --  Serialize all active entries into the grant buffer, then write
      --  in a single chunk. This keeps it simple and avoids per-entry IPC.
      declare
         outBuf : array (0 .. PAGE_SIZE - 1) of Unsigned_8
           with Import, Address => fsGrantBuf;
      begin
         writeOff := 0;

         for i in store'Range loop
            if store (i).active then
               --  Check we have room: key + '=' + value + LF
               declare
                  needed : constant Natural :=
                    store (i).keyLen + 1 + store (i).valueLen + 1;
               begin
                  if writeOff + needed > PAGE_SIZE then
                     exit;
                  end if;

                  --  Write key
                  for c in 0 .. store (i).keyLen - 1 loop
                     outBuf (writeOff + c) :=
                       Unsigned_8 (Character'Pos (store (i).key (1 + c)));
                  end loop;
                  writeOff := writeOff + store (i).keyLen;

                  --  Write '='
                  outBuf (writeOff) := 16#3D#;
                  writeOff := writeOff + 1;

                  --  Write value
                  for v in 0 .. store (i).valueLen - 1 loop
                     outBuf (writeOff + v) := store (i).value (v);
                  end loop;
                  writeOff := writeOff + store (i).valueLen;

                  --  Write LF
                  outBuf (writeOff) := 16#0A#;
                  writeOff := writeOff + 1;
               end;
            end if;
         end loop;
      end;

      --  Write the serialized data
      if writeOff > 0 then
         fsMsg := (tag => (label  => OP_WRITE,
                           length => 3,
                           flags  => 0,
                           badge  => 0),
                   capBadge => 0,
                   words => (0 => fileHandle,
                             1 => fsGrantId,
                             2 => Unsigned_64 (writeOff),
                             others => 0));
         fsMsg.tag := capCall (CAP_SLOT_FS_LOCAL, fsMsg);

         if fsMsg.tag.label /= REPLY_OK then
            debugPrint ("Config: save write failed" & LF);
         else
            debugPrint ("Config: saved to disk" & LF);
         end if;
      end if;

      --  Close file
      fsMsg := (tag => (label => OP_CLOSE, length => 1,
                        flags => 0, badge => 0),
                capBadge => 0,
                words => (0 => fileHandle, others => 0));
      fsMsg.tag := capCall (CAP_SLOT_FS_LOCAL, fsMsg);

      sendReply (sender, REPLY_OK, 0);
   end handleSave;

   ---------------------------------------------------------------------------
   --  Main message loop variables
   ---------------------------------------------------------------------------
   sender : ProcessID;
   msg    : Message;
begin
   debugPrint ("Config: starting..." & LF);

   --  Register as DRIVER_CONFIG so other services can discover us
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_CONFIG);
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

   debugPrint ("Config: entering message loop" & LF);

   --  Main IPC message loop
   loop
      receive (sender, msg);

      case msg.tag.label is
         when OP_CONFIG_GET =>
            handleGet (sender, msg);
         when OP_CONFIG_SET =>
            handleSet (sender, msg);
         when OP_CONFIG_DELETE =>
            handleDelete (sender, msg);
         when OP_CONFIG_LIST =>
            handleList (sender, msg);
         when OP_CONFIG_LOAD =>
            handleLoad (sender);
         when OP_CONFIG_SAVE =>
            handleSave (sender);
         when OP_SET_ACL =>
            handleSetACL (sender, msg);
         when OP_REVOKE_ACL =>
            handleRevokeACL (sender, msg);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end loop;
end main;
