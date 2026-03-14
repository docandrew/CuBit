------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Config store client API
--
--  @description
--  Provides typed access to the config store service for userspace
--  applications. Connects via capCall on CAP_SLOT_CONFIG. A single
--  page-aligned grant buffer (1 page = 4KB) is lazily allocated and
--  shared with the config service for key/value transfer.
--
--  get and list return System.Address pointers into the internal grant
--  buffer. Callers must copy data before the next config call.
------------------------------------------------------------------------------
with Interfaces;
with System;

package CuBit.Config is

   type ConfigStatus is (OK, NotFound, Error, AccessDenied);

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

   ---------------------------------------------------------------------------
   --  SchemeInfo
   --  Result of resolving a scheme name (e.g., "config", "fs", "net").
   ---------------------------------------------------------------------------
   type SchemeInfo is record
      driverID : Interfaces.Unsigned_64;
      capSlot  : Interfaces.Unsigned_64;
      pid      : Interfaces.Unsigned_64;
      found    : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  resolveScheme
   --  Look up a scheme by name. Builds "scheme.<name>.driver" and
   --  "scheme.<name>.slot" keys, queries config store, parses decimal
   --  values, and resolves driver PID via sysinfo.
   ---------------------------------------------------------------------------
   function resolveScheme (name : String) return SchemeInfo;

end CuBit.Config;
