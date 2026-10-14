------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Config store client API
--
--  @description
--  Provides typed access to the config store service for userspace
--  applications, through the config endpoint given to Bind (the program's
--  startup "config" handle); before Bind every call fails. A single
--  page-aligned two-page grant buffer is lazily allocated and shared with
--  Config using an owner- and generation-checked reference. Two pages allow
--  the full 4096-byte value plus its key without overrunning the buffer.
--
--  get and list return System.Address pointers into the internal grant
--  buffer. Callers must copy data before the next config call.
--  This borrowed-buffer API is not reentrant; callers must serialize access.
------------------------------------------------------------------------------
with System;
with CuBit.Handles;

package CuBit.Config is

   type ConfigStatus is (OK, NotFound, Error, AccessDenied);

   --  The config service endpoint every following call uses. Without a
   --  Bind, the first call uses the program's "config" startup entry
   --  (manifest: request-service config read-write config).
   Config_Binding : constant String := "config";
   procedure Bind (Service : CuBit.Handles.Endpoint);

   ---------------------------------------------------------------------------
   --  get
   --  Retrieve a config value by key. On success, value points into the
   --  internal grant buffer containing the raw value bytes, and valueLen
   --  gives the byte count. Caller must copy before the next config call.
   ---------------------------------------------------------------------------
   procedure get
     (key      : String;
      value    : out System.Address;
      valueLen : out Natural;
      status   : out ConfigStatus);

   ---------------------------------------------------------------------------
   --  set
   --  Set a config key to a byte-array value.
   ---------------------------------------------------------------------------
   procedure set
     (key      : String;
      value    : System.Address;
      valueLen : Natural;
      status   : out ConfigStatus);

   ---------------------------------------------------------------------------
   --  delete
   --  Delete a config key.
   ---------------------------------------------------------------------------
   procedure delete
     (key    : String;
      status : out ConfigStatus);

   ---------------------------------------------------------------------------
   --  list
   --  List keys matching a prefix. On success, keys points into the
   --  internal grant buffer containing NUL-separated key names, and count
   --  gives the number of keys. Caller must copy before the next call.
   --  Pass empty string to list all keys.
   ---------------------------------------------------------------------------
   procedure list
     (prefix   : String;
      keys     : out System.Address;
      count    : out Natural;
      status   : out ConfigStatus);

end CuBit.Config;
