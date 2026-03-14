------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Config store client API implementation
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

package body CuBit.Config is

   --  IPC labels (must match config service main.adb)
   OP_CONFIG_GET    : constant Unsigned_32 := 16#0600#;
   OP_CONFIG_SET    : constant Unsigned_32 := 16#0601#;
   OP_CONFIG_DELETE : constant Unsigned_32 := 16#0602#;
   OP_CONFIG_LIST   : constant Unsigned_32 := 16#0603#;
   REPLY_OK            : constant Unsigned_32 := 16#F000#;
   REPLY_ERR           : constant Unsigned_32 := 16#F001#;
   REPLY_ACCESS_DENIED : constant Unsigned_32 := 16#F007#;

   PAGE_SIZE : constant := 4096;

   --  Lazy-init state
   grantBuf    : System.Address := System.Null_Address;
   grantId     : Unsigned_64 := 0;
   initialized : Boolean := False;

   procedure ensureInit;
   function mapStatus (label : Unsigned_32) return ConfigStatus;
   function parseDecimal
     (buf : System.Address; len : Natural) return Unsigned_64;

   ---------------------------------------------------------------------------
   --  ensureInit
   --  Lazy initialization: allocate grant buffer, discover config PID,
   --  create grant. Same pattern as CuBit.Audio / FS client.
   ---------------------------------------------------------------------------
   procedure ensureInit is
      rawAddr    : Unsigned_64;
      aligned    : Unsigned_64;
      configPID  : Unsigned_64;
      gid        : Unsigned_64;
      ok         : Boolean;
   begin
      if initialized then
         return;
      end if;

      --  Allocate 2 pages (to guarantee page alignment)
      rawAddr := syscall (SYSCALL_SBRK, 2 * PAGE_SIZE);
      if rawAddr = Unsigned_64'Last then
         return;
      end if;

      aligned := (rawAddr + PAGE_SIZE - 1) and
                 not Unsigned_64 (PAGE_SIZE - 1);
      grantBuf := To_Address (Integer_Address (aligned));

      --  Discover config service PID
      configPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_CONFIG);
      if configPID = 0 then
         return;
      end if;

      --  Create a read-write grant to the config service (1 page)
      createGrant
        (grantee   => configPID,
         localAddr => grantBuf,
         numPages  => 1,
         readWrite => True,
         grantId   => gid,
         success   => ok);

      if not ok then
         return;
      end if;

      grantId := gid;
      initialized := True;
   end ensureInit;

   ---------------------------------------------------------------------------
   --  mapStatus
   ---------------------------------------------------------------------------
   function mapStatus (label : Unsigned_32) return ConfigStatus is
   begin
      if label = REPLY_OK then
         return OK;
      elsif label = REPLY_ACCESS_DENIED then
         return AccessDenied;
      elsif label = REPLY_ERR then
         return NotFound;
      else
         return Error;
      end if;
   end mapStatus;

   ---------------------------------------------------------------------------
   --  get
   ---------------------------------------------------------------------------
   procedure get
     (key      : String;
      value    : out System.Address;
      valueLen : out Natural;
      status   : out ConfigStatus)
   is
      msg : Message;
   begin
      value    := System.Null_Address;
      valueLen := 0;

      ensureInit;
      if not initialized then
         status := Error;
         return;
      end if;

      --  Write key to grant buffer
      declare
         buf : String (1 .. key'Length)
           with Import, Address => grantBuf;
      begin
         for i in key'Range loop
            buf (1 + i - key'First) := key (i);
         end loop;
      end;

      msg := (tag => (label  => OP_CONFIG_GET,
                      length => 2,
                      flags  => 0,
                      badge  => 0),
              capBadge => 0,
              words => (0 => grantId,
                        1 => Unsigned_64 (key'Length),
                        others => 0));

      msg.tag := capCall (CAP_SLOT_CONFIG, msg);

      status := mapStatus (msg.tag.label);
      if status = OK then
         value    := grantBuf;
         valueLen := Natural (msg.words (0));
      end if;
   end get;

   ---------------------------------------------------------------------------
   --  set
   ---------------------------------------------------------------------------
   procedure set
     (key      : String;
      value    : System.Address;
      valueLen : Natural;
      status   : out ConfigStatus)
   is
      msg    : Message;
      keyLen : constant Natural := key'Length;
   begin
      ensureInit;
      if not initialized then
         status := Error;
         return;
      end if;

      --  Write key at offset 0, then value at offset keyLen
      declare
         buf : array (0 .. keyLen + valueLen - 1) of Unsigned_8
           with Import, Address => grantBuf;
         src : array (0 .. valueLen - 1) of Unsigned_8
           with Import, Address => value;
      begin
         --  Copy key
         for i in 0 .. keyLen - 1 loop
            buf (i) := Unsigned_8 (Character'Pos (key (key'First + i)));
         end loop;

         --  Copy value
         for i in 0 .. valueLen - 1 loop
            buf (keyLen + i) := src (i);
         end loop;
      end;

      msg := (tag => (label  => OP_CONFIG_SET,
                      length => 3,
                      flags  => 0,
                      badge  => 0),
              capBadge => 0,
              words => (0 => grantId,
                        1 => Unsigned_64 (keyLen),
                        2 => Unsigned_64 (valueLen),
                        others => 0));

      msg.tag := capCall (CAP_SLOT_CONFIG, msg);

      status := mapStatus (msg.tag.label);
   end set;

   ---------------------------------------------------------------------------
   --  delete
   ---------------------------------------------------------------------------
   procedure delete
     (key    : String;
      status : out ConfigStatus)
   is
      msg : Message;
   begin
      ensureInit;
      if not initialized then
         status := Error;
         return;
      end if;

      --  Write key to grant buffer
      declare
         buf : String (1 .. key'Length)
           with Import, Address => grantBuf;
      begin
         for i in key'Range loop
            buf (1 + i - key'First) := key (i);
         end loop;
      end;

      msg := (tag => (label  => OP_CONFIG_DELETE,
                      length => 2,
                      flags  => 0,
                      badge  => 0),
              capBadge => 0,
              words => (0 => grantId,
                        1 => Unsigned_64 (key'Length),
                        others => 0));

      msg.tag := capCall (CAP_SLOT_CONFIG, msg);

      status := mapStatus (msg.tag.label);
   end delete;

   ---------------------------------------------------------------------------
   --  list
   ---------------------------------------------------------------------------
   procedure list
     (prefix   : String;
      keys     : out System.Address;
      count    : out Natural;
      status   : out ConfigStatus)
   is
      msg : Message;
   begin
      keys  := System.Null_Address;
      count := 0;

      ensureInit;
      if not initialized then
         status := Error;
         return;
      end if;

      --  Write prefix to grant buffer (may be empty)
      if prefix'Length > 0 then
         declare
            buf : String (1 .. prefix'Length)
              with Import, Address => grantBuf;
         begin
            for i in prefix'Range loop
               buf (1 + i - prefix'First) := prefix (i);
            end loop;
         end;
      end if;

      msg := (tag => (label  => OP_CONFIG_LIST,
                      length => 2,
                      flags  => 0,
                      badge  => 0),
              capBadge => 0,
              words => (0 => grantId,
                        1 => Unsigned_64 (prefix'Length),
                        others => 0));

      msg.tag := capCall (CAP_SLOT_CONFIG, msg);

      status := mapStatus (msg.tag.label);
      if status = OK then
         keys  := grantBuf;
         count := Natural (msg.words (0));
      end if;
   end list;

   ---------------------------------------------------------------------------
   --  parseDecimal
   --  Parse a decimal integer from a byte buffer. Returns 0 on failure.
   ---------------------------------------------------------------------------
   function parseDecimal
     (buf : System.Address; len : Natural) return Unsigned_64
   is
      data : array (0 .. len - 1) of Unsigned_8
        with Import, Address => buf;
      result : Unsigned_64 := 0;
      ch     : Unsigned_8;
   begin
      for i in 0 .. len - 1 loop
         ch := data (i);
         if ch >= 16#30# and ch <= 16#39# then  --  '0'..'9'
            result := result * 10 + Unsigned_64 (ch - 16#30#);
         else
            return 0;
         end if;
      end loop;
      return result;
   end parseDecimal;

   ---------------------------------------------------------------------------
   --  resolveScheme
   ---------------------------------------------------------------------------
   function resolveScheme (name : String) return SchemeInfo is
      --  Build key: "scheme." + name + ".driver" = max 7+name+7 = 128 max
      MAX_SCHEME_KEY : constant := 128;
      keyBuf : String (1 .. MAX_SCHEME_KEY);
      keyLen : Natural;

      val    : System.Address;
      valLen : Natural;
      st     : ConfigStatus;

      info   : SchemeInfo := (driverID => 0, capSlot => 0,
                               pid => 0, found => False);
      prefix : constant String := "scheme.";
      drvSuffix  : constant String := ".driver";
      slotSuffix : constant String := ".slot";
   begin
      if name'Length = 0 or
         prefix'Length + name'Length + drvSuffix'Length > MAX_SCHEME_KEY
      then
         return info;
      end if;

      --  Build "scheme.<name>.driver"
      keyLen := 0;
      for c in prefix'Range loop
         keyLen := keyLen + 1;
         keyBuf (keyLen) := prefix (c);
      end loop;
      for c in name'Range loop
         keyLen := keyLen + 1;
         keyBuf (keyLen) := name (c);
      end loop;

      declare
         drvKeyLen : constant Natural := keyLen;
      begin
         for c in drvSuffix'Range loop
            keyLen := keyLen + 1;
            keyBuf (keyLen) := drvSuffix (c);
         end loop;

         get (keyBuf (1 .. keyLen), val, valLen, st);
         if st /= OK or valLen = 0 then
            return info;
         end if;

         info.driverID := parseDecimal (val, valLen);

         --  Build "scheme.<name>.slot" (reuse prefix+name, just swap suffix)
         keyLen := drvKeyLen;
         for c in slotSuffix'Range loop
            keyLen := keyLen + 1;
            keyBuf (keyLen) := slotSuffix (c);
         end loop;
      end;

      get (keyBuf (1 .. keyLen), val, valLen, st);
      if st /= OK or valLen = 0 then
         return info;
      end if;

      info.capSlot := parseDecimal (val, valLen);

      --  Resolve driver PID via sysinfo
      info.pid := getInfo (SYSINFO_REGISTERED_DRIVER, info.driverID);
      if info.pid = 0 or info.pid = Unsigned_64'Last then
         info.pid := 0;
      else
         info.found := True;
      end if;

      return info;
   end resolveScheme;

end CuBit.Config;
