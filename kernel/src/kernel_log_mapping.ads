-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel log ring (Kernel_Log) mapped into its one reader.
--
-- @description
-- The registered log service reads the ring (SYSCALL_REGISTER_DRIVER for the
-- logstore role, which needs the logstore notification capability): every
-- page of it read-only and non-executable at
-- Kernel_Log_Publication.Page_Address. No other process ever has it: the
-- ring holds every process's crash output and the kernel's diagnostics.
-- The frames are the kernel image's: never charged to, tracked with or
-- freed with the reader (its teardown frees page tables, not leaves).
-------------------------------------------------------------------------------
with Process;

package Kernel_Log_Mapping with
    SPARK_Mode => Off -- page-table writes
is
    -- Mapped is False only if a page could not be mapped; the registration
    -- is then refused, so a registered log service can always read it.
    procedure Map_Into (Reader : Process.ProcessID; Mapped : out Boolean);
end Kernel_Log_Mapping;
