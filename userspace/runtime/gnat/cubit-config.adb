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
with CuBit.Memory_Grants;
with CuBit.Handles.Startup;
with CuBit.Config_Protocol;

package body CuBit.Config is
   use type System.Address;

   --  IPC labels (must match config service main.adb)
   OP_CONFIG_GET    : constant Unsigned_32 := 16#0600#;
   OP_CONFIG_SET    : constant Unsigned_32 := 16#0601#;
   OP_CONFIG_DELETE : constant Unsigned_32 := 16#0602#;
   OP_CONFIG_LIST   : constant Unsigned_32 := 16#0603#;
   REPLY_OK            : constant Unsigned_32 := 16#F000#;
   REPLY_ACCESS_DENIED : constant Unsigned_32 := 16#F007#;

   PAGE_SIZE : constant := 4096;

   --  Lazy-init state
   grantBuf    : System.Address := System.Null_Address;
   Grant : CuBit.Memory_Grants.Grant_Reference;
   initialized : Boolean := False;

   Service : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;

   procedure Bind (Service : CuBit.Handles.Endpoint) is
   begin
      Config.Service := Service;
   end Bind;

   procedure ensureInit;
   function mapStatus (label : Unsigned_32) return ConfigStatus;

   ---------------------------------------------------------------------------
   --  ensureInit
   --  Allocate a buffer and create a generation-bearing endpoint grant.
   ---------------------------------------------------------------------------
   procedure ensureInit is
      rawAddr    : Unsigned_64;
      aligned    : Unsigned_64;
      ok         : Boolean;
   begin
      if initialized then
         return;
      end if;
      --  Unbound: the program's "config" startup entry, if it has one.
      if not CuBit.Handles.Valid (Service) then
         declare
            Found : constant CuBit.Handles.Startup.Optional_Endpoint :=
              CuBit.Handles.Startup.Optional_Endpoint_Named (Config_Binding);
         begin
            if Found.Present then
               Service := Found.Value;
            end if;
         end;
      end if;

      --  Allocate 3 pages to obtain 2 page-aligned payload pages.
      rawAddr := syscall (SYSCALL_SBRK, 3 * PAGE_SIZE);
      if rawAddr = Unsigned_64'Last then
         return;
      end if;

      aligned := (rawAddr + PAGE_SIZE - 1) and
                 not Unsigned_64 (PAGE_SIZE - 1);
      grantBuf := To_Address (Integer_Address (aligned));

      CuBit.Memory_Grants.Create_Via_Capability
        (Service, grantBuf, 2, True, Grant, ok);
      if not ok then
         return;
      end if;
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
      elsif label = 16#F060# then
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

      if key'Length not in 1 .. CuBit.Config_Protocol.Maximum_Key then
         status := Error; return;
      end if;
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
                      length => 4,
                      protocol  => 0, ipc => <>,
                      reserved  => 0),
              authorityTag => 0,
              words => (0 => Grant.slot,
                        1 => Grant.generation,
                        2 => Unsigned_64 (key'Length),
                        others => 0));

      msg.tag := capCall (Service, msg, CuBit.Messages.Wait_Forever);

      status := mapStatus (msg.tag.label);
      if status = OK and then
        (msg.tag.length /= 1 or else
         msg.words (0) > CuBit.Config_Protocol.Maximum_Value)
      then
         status := Error;
      end if;
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
      if keyLen not in 1 .. CuBit.Config_Protocol.Maximum_Key or else
        valueLen > CuBit.Config_Protocol.Maximum_Value or else
        (valueLen > 0 and value = System.Null_Address)
      then
         status := Error;
         return;
      end if;
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
                      length => 4,
                      protocol  => 0, ipc => <>,
                      reserved  => 0),
              authorityTag => 0,
              words => (0 => Grant.slot,
                        1 => Grant.generation,
                        2 => Unsigned_64 (keyLen),
                        3 => Unsigned_64 (valueLen)));

      msg.tag := capCall (Service, msg, CuBit.Messages.Wait_Forever);

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
      if key'Length not in 1 .. CuBit.Config_Protocol.Maximum_Key then
         status := Error; return;
      end if;
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
                      length => 4,
                      protocol  => 0, ipc => <>,
                      reserved  => 0),
              authorityTag => 0,
              words => (0 => Grant.slot,
                        1 => Grant.generation,
                        2 => Unsigned_64 (key'Length),
                        others => 0));

      msg.tag := capCall (Service, msg, CuBit.Messages.Wait_Forever);

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

      if prefix'Length > CuBit.Config_Protocol.Maximum_Key then
         status := Error; return;
      end if;
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
                      length => 4,
                      protocol  => 0, ipc => <>,
                      reserved  => 0),
              authorityTag => 0,
              words => (0 => Grant.slot,
                        1 => Grant.generation,
                        2 => Unsigned_64 (prefix'Length),
                        others => 0));

      msg.tag := capCall (Service, msg, CuBit.Messages.Wait_Forever);

      status := mapStatus (msg.tag.label);
      if status = OK and then
        (msg.tag.length /= 1 or else msg.words (0) > 256)
      then
         status := Error;
      end if;
      if status = OK then
         keys  := grantBuf;
         count := Natural (msg.words (0));
      end if;
   end list;

end CuBit.Config;
