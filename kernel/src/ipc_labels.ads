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
--   16#0400# - 16#04FF#  Network operations
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
    OP_READDIR    : constant Unsigned_32 := 16#0007#;
    OP_SET_ACL    : constant Unsigned_32 := 16#0080#;
    OP_REVOKE_ACL : constant Unsigned_32 := 16#0081#;
    OP_QUERY_ACL  : constant Unsigned_32 := 16#0082#;

    -- Process operations
    OP_SPAWN   : constant Unsigned_32 := 16#0100#;
    OP_EXIT    : constant Unsigned_32 := 16#0101#;
    OP_WAIT    : constant Unsigned_32 := 16#0102#;

    --  Process lifecycle events (kernel -> userspace)
    EVENT_CHILD_EXIT    : constant Unsigned_32 := 16#0103#;
    EVENT_CAP_FAULT     : constant Unsigned_32 := 16#0104#;
    EVENT_PROCESS_FAULT : constant Unsigned_32 := 16#0105#;

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

    -- Network operations (driver ↔ netstack)
    OP_NET_ATTACH     : constant Unsigned_32 := 16#0400#;
    OP_NET_RX         : constant Unsigned_32 := 16#0401#;
    OP_NET_TX         : constant Unsigned_32 := 16#0402#;
    OP_NET_GET_MAC    : constant Unsigned_32 := 16#0403#;
    OP_NET_GET_CONFIG : constant Unsigned_32 := 16#0404#;

    -- Network operations (app -> netstack, legacy socket-style)
    OP_NET_RESOLVE    : constant Unsigned_32 := 16#0410#;
    OP_NET_CONNECT    : constant Unsigned_32 := 16#0411#;
    OP_NET_SEND       : constant Unsigned_32 := 16#0412#;
    OP_NET_RECV       : constant Unsigned_32 := 16#0413#;
    OP_NET_CLOSE      : constant Unsigned_32 := 16#0414#;

    -- Network operations (app -> netstack, channel API)
    OP_NET_OPEN       : constant Unsigned_32 := 16#0420#;
    OP_NET_WRITE      : constant Unsigned_32 := 16#0421#;
    OP_NET_READ       : constant Unsigned_32 := 16#0422#;
    OP_NET_SHUT       : constant Unsigned_32 := 16#0423#;
    OP_NET_BIND       : constant Unsigned_32 := 16#0424#;
    OP_NET_ACCEPT     : constant Unsigned_32 := 16#0425#;

    -- Audio operations (app ↔ mixer)
    OP_AUDIO_OPEN     : constant Unsigned_32 := 16#0500#;
    OP_AUDIO_CLOSE    : constant Unsigned_32 := 16#0501#;
    OP_AUDIO_SET_VOL  : constant Unsigned_32 := 16#0502#;
    OP_AUDIO_GET_VOL  : constant Unsigned_32 := 16#0503#;
    OP_AUDIO_SET_PAN  : constant Unsigned_32 := 16#0504#;
    OP_AUDIO_SET_FMT  : constant Unsigned_32 := 16#0505#;

    -- Audio operations (mixer ↔ HDA driver)
    OP_AUDIO_HW_INIT  : constant Unsigned_32 := 16#0510#;
    OP_AUDIO_HW_START : constant Unsigned_32 := 16#0511#;
    OP_AUDIO_HW_STOP  : constant Unsigned_32 := 16#0512#;
    OP_AUDIO_HW_FILL  : constant Unsigned_32 := 16#0513#;
    OP_AUDIO_HW_DRAIN : constant Unsigned_32 := 16#0514#;
    OP_AUDIO_HW_CAPS  : constant Unsigned_32 := 16#0515#;

    -- Status/error reply labels
    REPLY_OK       : constant Unsigned_32 := 16#F000#;
    REPLY_ERR      : constant Unsigned_32 := 16#F001#;
    REPLY_BUSY     : constant Unsigned_32 := 16#F002#;
    REPLY_CANCELED : constant Unsigned_32 := 16#F003#;
    REPLY_TIMEOUT  : constant Unsigned_32 := 16#F004#;
    REPLY_FULL     : constant Unsigned_32 := 16#F005#;
    REPLY_EOF           : constant Unsigned_32 := 16#F006#;
    REPLY_ACCESS_DENIED : constant Unsigned_32 := 16#F007#;

end IPC_Labels;
