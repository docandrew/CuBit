-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Launch arguments and termination reports (docs/process-arguments.md).
--
-- @description
-- The spawner (procmgr) installs a validated launch-argument block into a
-- suspended, never-started child with SYSCALL_INSTALL_LAUNCH_ARGUMENTS. The
-- kernel maps it read-only and non-executable at Arguments_Base and starts
-- the main thread with RDI = its length (zero: no block). The kernel does not
-- interpret the bytes; procmgr and the child validate them
-- (userspace/runtime/gnat/cubit-launch_arguments.ads, which mirrors these
-- constants).
--
-- When a process retires, EVENT_CHILD_EXIT carries its termination report:
-- words (0) = PID, (1) = Termination_Kind, (2) = exit code, (3) = the
-- process's generation (its capability generation while it lived), so a
-- launcher can tell it from an earlier or later process with the same PID.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Process_Launch with
    SPARK_Mode => On,
    Pure
is
    Arguments_Base      : constant := 16#0000_5A00_0000_0000#;
    Maximum_Bytes       : constant := 64 * 1024;
    Page_Bytes          : constant := 4096;
    Maximum_Pages       : constant := Maximum_Bytes / Page_Bytes;

    subtype Argument_Bytes is Unsigned_64 range 1 .. Maximum_Bytes;
    subtype Argument_Pages is Natural range 1 .. Maximum_Pages;

    function Page_Count (Length : Argument_Bytes) return Argument_Pages is
        (Argument_Pages ((Length + Page_Bytes - 1) / Page_Bytes));

    -- Where a process is in its launch: arguments may be installed only
    -- before its first resume, and only once. The all-zero value is
    -- Unstarted, so a reset process record starts there.
    type Launch_Phase is (Unstarted, Arguments_Installed, Started);
    for Launch_Phase use
        (Unstarted => 0, Arguments_Installed => 1, Started => 2);

    -- How a process ended. Exited: it asked to (SYSCALL_EXIT), with a code.
    -- Stopped: anything else (killed, faulted, its main thread ended).
    -- Values are the EVENT_CHILD_EXIT wire encoding; zero is never sent.
    type Termination_Kind is (Exited, Stopped);
    for Termination_Kind use (Exited => 1, Stopped => 2);

    -- POSIX keeps the low 8 bits of an exit status; so does CuBit.
    Exit_Code_Modulus : constant := 256;
    subtype Exit_Code is Unsigned_64 range 0 .. Exit_Code_Modulus - 1;

    function To_Exit_Code (Requested : Unsigned_64) return Exit_Code is
        (Requested mod Exit_Code_Modulus);

    type Termination_Report is record
        kind : Termination_Kind := Stopped;
        code : Exit_Code := 0;
    end record;

    Child_Exit_Words : constant := 4;
end Process_Launch;
