-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary
-- Per-CPU Data Region
--
-- @description
-- Contains info about the currently running process.
-- The address of the Per-CPU Data Region is always available at KERNEL_GS_BASE
-------------------------------------------------------------------------------
with System; use System;

pragma Warnings (Off);
with System.Secondary_Stack; use System.Secondary_Stack;
pragma Warnings (On);

with Config;
with Process;
with Segment;
with Util;

package PerCPUData with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Each CPU has one of these data structures associated with it.
    -- @field cpuNum - the CPU number this record is for
    -- @field runningPID - PID currently running on this CPU
    -- @field savedRSP - the stack pointer into our process' kernel stack when
    --  we entered user mode
    -- @field gdt - a Global Descriptor Table
    -- @field gdtPointer - a GDT Pointer structure containing the size and
    --  address of the gdt here
    --
    -- @field tss - Task Switch Segment for this CPU
    --
    -- @field oldContext - saved stack location of previously running thread
    -- @field currentContext - currently running stack on this CPU
    -- @field schedulerContext - stack for the scheduling thread on this CPU
    --
    -- @field intsEnabled - Whether interrupts are enabled or not on THIS CPU.
    -- @field numCLI - number of cli instructions we've executed. Used by locks
    --  to enable nesting.
    -- 
    -- @WARNING Make sure these match the offsets in cubit.inc, this record is
    --  accessed from assembly code!
    ---------------------------------------------------------------------------
    type PerCPUData is
    record
        cpuNum              : Natural range 0..Config.MAX_CPUS - 1 := 0;
        currentPID          : Process.ProcessID := 0;
        savedProcessRSP     : System.Address;
        savedKernelRSP      : System.Address;

        -- The per-CPU GDT
        gdt                 : Segment.GDT;
        gdtPointer          : Segment.GDTPointer;

        -- These contain the per-CPU stack pointers
        -- for both user and kernel modes.
        tss                 : Segment.TaskSwitchSegment;

        -- Saved context pointers (stack locations for where we left off)
        oldContext          : System.Address;
        currentContext      : System.Address;
        schedulerContext    : System.Address;

        intsEnabled         : Boolean;
        numCLI              : Integer;
    end record;

    -- Alignment of fields needs to be precisely specified because we're going
    -- to access some of these from assembly.
    for PerCPUData use
    record
        cpuNum              at 0    range 0..31;
        currentPID          at 4    range 0..31;
        savedProcessRSP     at 8    range 0..63;
        savedKernelRSP      at 16   range 0..63;
        gdt                 at 24   range 0..511;
        gdtPointer          at 88   range 0..79;
        tss                 at 98   range 0..831;
        oldContext          at 202  range 0..63;
        currentContext      at 210  range 0..63;
        schedulerContext    at 218  range 0..63;
        intsEnabled         at 226  range 0..31;
        numCLI              at 230  range 0..31;
    end record;

    -- If the secondary stack is used before per-CPU data is set up, this
    -- exception will be raised.
    SecondaryStackNotAvailable : exception;

    ---------------------------------------------------------------------------
    -- setupPerCPUData
    -- On boot, we're using the bootstrap GDT set up in boot.asm. This function
    -- creates a new GDT with Kernel Code+Data segments and User Code+Data
    -- Segments. It will also install a pointer to this per-CPU data into the
    -- KERNEL_GS_BASE MSR, which can be used with the SWAPGS instruction.
    --
    -- This function must be called once by each CPU during boot.
    --
    -- @TODO: consider adding a ghost initialized flag here, though this may be
    --  infeasible because we would need one per-CPU, to make sure that this
    --  function is called before use.
    ---------------------------------------------------------------------------
    procedure setupPerCPUData (cpuNum          : in Natural;
                               cpuData         : in out PerCPUData;
                               cpuDataAddr     : in System.Address;
                               gdtAddr         : in System.Address;
                               gdtPointerAddr  : in System.Address;
                               tssAddr         : in System.Address) with
        Pre => (cpuNum < Config.MAX_CPUS and
                cpuDataAddr /= System.Null_Address and
                gdtAddr /= System.Null_Address and 
                gdtPointerAddr /= System.Null_Address and
                tssAddr /= System.Null_Address);

    ---------------------------------------------------------------------------
    -- getPerCPUData returns the address of the per-CPU data structure.
    --
    -- WARNING: setupPerCPUData _must_ be called on each CPU prior to using
    --  this.
    ---------------------------------------------------------------------------
    function getPerCPUDataAddr return System.Address;

    ---------------------------------------------------------------------------
    -- getCPUNumber
    -- @return the number of the CPU the caller is running on.
    ---------------------------------------------------------------------------
    function getCPUNumber return Natural;

    ---------------------------------------------------------------------------
    -- getCurrentPID
    -- Convenience function for getting the currently-running Process ID on
    -- this CPU.
    ---------------------------------------------------------------------------
    function getCurrentPID return Process.ProcessID;

    InterruptException : exception;

    ---------------------------------------------------------------------------
    -- intsEnabled
    -- Returns True if interrupts are enabled on this CPU
    ---------------------------------------------------------------------------
    function intsEnabled return Boolean;

    ---------------------------------------------------------------------------
    -- numCLI
    -- @return depth of CLI (interrupt disable) instructions for nested locks.
    ---------------------------------------------------------------------------
    function numCLI return Integer;

    ---------------------------------------------------------------------------
    -- pushCLI
    -- Disable interrupts on this CPU and increase the CLI depth by one.
    --
    -- Heavily inspired by Xv6 and Linux' enable_irq / disable_irq
    ---------------------------------------------------------------------------
    procedure pushCLI;

    ---------------------------------------------------------------------------
    -- popCLI
    -- Undoes the effect of one call to pushCLI, decreases the CLI depth by one.
    -- If the CLI depth reaches 0 (i.e. all pushCLIs have been matched), then
    -- interrupts are enabled for this CPU.
    -- @raise InterruptException if interrupts not previously disabled prior to
    --  this call.
    -- @raise InterruptException if this is called without a previous matching
    --  call to pushCLI
    ---------------------------------------------------------------------------
    procedure popCLI;

    ---------------------------------------------------------------------------
    -- Used to get pointers to the per-CPU secondary stacks.
    ---------------------------------------------------------------------------
    function getSecondaryStack return SS_Stack_Ptr
        with Export, Convention => C,
            External_Name => "__gnat_get_secondary_stack";

private
    syscallEntryPoint : Util.Symbol
        with Import => True, Convention => C, External_Name => "syscallEntry";

end PerCPUData;
