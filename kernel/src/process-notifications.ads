-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Notifications (IPC revision 2, S1): the kernel's table of
-- notification objects, the Wait and Wake system calls on them, and the
-- Calls that create, mint, bind and unbind them (Notification_ABI).
--
-- @description
-- Each object's rules are Notification_Objects (proved, with the two-level
-- fan-in model in tests/notifications); this unit holds them in an
-- Object_Table, one spinlock each, and names them by (slot, generation) in
-- CAP_NOTIFICATION_OBJECT handles.
--
-- Life and charge. Create makes an object charged to its creator, at most
-- Notification_ABI.Per_Process_Limit per process, and one Wait handle for
-- it (RIGHT_READ and RIGHT_WRITE, unbadged, no RIGHT_GRANT, so it is never
-- copied). The object lives while that handle does: closing it, or the
-- holder's exit, ends the object, its waiter returns Ended, and every Wake
-- handle of it answers Ended (its generation is gone). So nothing is ever
-- charged to a dead process and there is no orphan. (The move of a Wait
-- right, with the charge, is S2's meeting.)
--
-- Wake takes the object's lock and, for a leaf, then its root's lock, one
-- after the other (never both), with no allocation: it is safe from
-- interrupt context. A binding racing a wake loses nothing: bits pending
-- in a leaf when it is bound are signalled to the root at the binding.
--
-- Lock order: a deadline heap lock (Process.Wait_Timers), then one
-- notification lock (two, in ascending slot order, only for Bind_Leaf),
-- then Process.lock or a mailbox lock. Handles are looked up under the
-- table lock before any object lock is taken.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Capabilities;

package Process.Notifications is

    -- Once, at boot (Process.setup), before any other operation.
    procedure Setup;

    -- Quiescent point and page reclamation (Object_Table).
    procedure Quiescent (cpu : Natural);
    procedure Reclaim;

    -- The system calls (Notification_ABI results).
    function wait (handle, deadlineUs : Unsigned_64) return Unsigned_64;
    function wake (handle : Unsigned_64) return Unsigned_64;

    -- Kernel_Object_Calls: a Call on a CAP_NOTIFICATION_OBJECT handle.
    procedure Call (caller : ProcessID; handle : Unsigned_64; cap : Capabilities.Capability;
                    request : Message; reply : out Message);

    -- Kernel_Object_Calls: Create on the caller's own CAP_PROCESS handle.
    procedure Create_Call (caller : ProcessID; cap : Capabilities.Capability;
                           request : Message; reply : out Message);

    -- CLOSE_HANDLE removed cap from its holder's table: a Wait handle ends
    -- its object.
    procedure Closed (cap : Capabilities.Capability);

    -- Process teardown, before its table is released: end every object it
    -- holds the Wait right of. No lock held.
    procedure Release_Held (pid : ProcessID);

    -- Process.Wait_Timers: t's deadline came (its heap lock held).
    procedure Time_Out (t : ThreadID);

    -- Process.Wait_Timers.Cancel: t will not run again.
    procedure Cancel (t : ThreadID);

    -- t exits or is reaped: its bound notification, if any, is unbound.
    procedure Unbind_Thread_Of (t : ThreadID);

end Process.Notifications;
