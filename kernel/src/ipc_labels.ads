-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Standard IPC message labels (operation codes)
--
-- @description
-- Central registry of IPC operation codes that servers and clients agree on.
-- Labels are carried in the MessageTag.label field (Unsigned_32).
-- Ranges are reserved by category to avoid collisions:
--   16#0001# - 16#00FF#  Filesystem operations
--   16#0100# - 16#01FF#  Process operations
--   16#0200# - 16#02FF#  Device operations
--   16#0300# - 16#03FF#  Memory operations
--   16#F000# - 16#FFFF#  Status/error reply labels
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package IPC_Labels with
    SPARK_Mode => On
is
    -- Filesystem operations
    OP_OPEN    : constant Unsigned_32 := 16#0001#;
    OP_CLOSE   : constant Unsigned_32 := 16#0002#;
    OP_READ    : constant Unsigned_32 := 16#0003#;
    OP_WRITE   : constant Unsigned_32 := 16#0004#;
    OP_STAT    : constant Unsigned_32 := 16#0005#;
    OP_SEEK    : constant Unsigned_32 := 16#0006#;

    -- Process operations
    OP_SPAWN   : constant Unsigned_32 := 16#0100#;
    OP_EXIT    : constant Unsigned_32 := 16#0101#;
    OP_WAIT    : constant Unsigned_32 := 16#0102#;

    -- Device operations
    OP_IOCTL       : constant Unsigned_32 := 16#0200#;

    -- Block device operations
    OP_READ_BLOCK  : constant Unsigned_32 := 16#0210#;
    OP_WRITE_BLOCK : constant Unsigned_32 := 16#0211#;
    OP_IDENTIFY    : constant Unsigned_32 := 16#0212#;

    -- Memory operations (for future shared memory grants)
    OP_GRANT   : constant Unsigned_32 := 16#0300#;
    OP_REVOKE  : constant Unsigned_32 := 16#0301#;
    OP_MAP     : constant Unsigned_32 := 16#0302#;

    -- Status/error reply labels
    REPLY_OK       : constant Unsigned_32 := 16#F000#;
    REPLY_ERR      : constant Unsigned_32 := 16#F001#;
    REPLY_BUSY     : constant Unsigned_32 := 16#F002#;
    REPLY_CANCELED : constant Unsigned_32 := 16#F003#;
    REPLY_TIMEOUT  : constant Unsigned_32 := 16#F004#;
    REPLY_FULL     : constant Unsigned_32 := 16#F005#;

end IPC_Labels;
