-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Wait and Wake (IPC revision 2, S1; Notification_ABI): waiting
-- on one object, a notification or a private futex word, and signalling it.
-------------------------------------------------------------------------------

package Syscall.Waits with SPARK_Mode => Off is

    -- WAIT_ONE: arg0 = kind, arg1 = object, arg2 = expected (a private
    -- word's), arg3 = absolute monotonic microsecond deadline.
    procedure handleWait (arg0, arg1, arg2, arg3 : Unsigned_64; retval : out Unsigned_64);

    -- WAKE: arg0 = kind, arg1 = object, arg2 = count (a private word's).
    procedure handleWake (arg0, arg1, arg2 : Unsigned_64; retval : out Unsigned_64);

end Syscall.Waits;
