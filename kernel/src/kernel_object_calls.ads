-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Call on a kernel-object handle (ABI-002 slice 1, KERN-004).
--
-- @description
-- The one Call takes a handle and a message. An endpoint handle goes down
-- the mailbox path (Process.IPC.capCall), unchanged. Any other handle the
-- caller holds is resolved here (type, generation, rights) and, when it
-- names a kernel object, the operation runs in ring 0 on the message's
-- label, with no process switch, and the reply comes back in the same
-- message, exactly as an endpoint's would. The register layout is the
-- same, so a userspace service can stand in for a kernel object behind the
-- same handle.
--
-- Kernel objects: CAP_PLATFORM_POWER (Platform_Power), CAP_FIRMWARE_SNAPSHOT
-- (Firmware_Snapshot_Export), CAP_TRACE (Trace_Object), CAP_PROCESS_LIST
-- (Process_List_Object), CAP_TEST_CONSOLE (Test_Console_Object), and a
-- CAP_NOTIFICATION's milestone mark (Boot_Timeline_Marks), and debug
-- sessions (Debug_Sessions: Mint_Debug and Revoke_Debug on a CAP_PROCESS,
-- and the four debug kinds' operations), and virtual machines (VM_Objects:
-- CAP_VM_CREATE, CAP_VM, CAP_VCPU). The other kernel
-- operations move here slice by slice (docs/syscall-abi-audit.md).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Process;

package Kernel_Object_Calls is

    type Outcome is (Not_A_Kernel_Object, Answered);

    -- Handle names a kernel object in Caller's table: run Request on it and
    -- set Reply (Answered). Otherwise touch nothing (Not_A_Kernel_Object).
    procedure Call (Caller  : Process.ProcessID;
                    Handle  : Unsigned_64;
                    Request : Process.Message;
                    Reply   : out Process.Message;
                    Result  : out Outcome);

end Kernel_Object_Calls;
