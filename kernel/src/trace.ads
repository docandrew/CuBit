-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Low-overhead kernel trace ring.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Trace with
    SPARK_Mode => Off
is
    subtype EventKind is Unsigned_16 range 0 .. 15;

    EVENT_SYSCALL_ENTER  : constant EventKind := 1;
    EVENT_SCHEDULE_RUN   : constant EventKind := 2;
    EVENT_SCHEDULE_STOP  : constant EventKind := 3;
    EVENT_SYSCALL_TIME   : constant EventKind := 4;
    EVENT_RUN_TIME       : constant EventKind := 5;
    EVENT_READY_LATENCY  : constant EventKind := 6;

    -- Clear all trace buffers and enable recording. Tracing starts disabled so
    -- normal boots and UI work do not pay for benchmark instrumentation.
    procedure Reset;

    -- Stop recording without clearing the buffers. This freezes the trace long
    -- enough to print or inspect it after a benchmark finishes.
    procedure Disable;

    -- Let hot paths avoid timestamp reads entirely while tracing is disabled.
    function IsEnabled return Boolean with Inline;

    -- Emit one compact event in the current CPU's ring. This routine must
    -- stay tiny: no allocation, no serial output, no blocking calls.
    procedure Emit
        (event : in EventKind;
         arg0  : in Unsigned_64 := 0;
         arg1  : in Unsigned_64 := 0) with Inline;

    -- Add a duration sample to the event's fixed TSC histogram. Durations are
    -- kept as raw TSC deltas for now; later trace export can convert them to
    -- microseconds once we expose the calibrated TSC rate cleanly.
    procedure ObserveDuration
        (event : in EventKind;
         ticks : in Unsigned_64) with Inline;

    -- Print an aggregate summary. This is intentionally separate from Record
    -- so slow serial output happens after the measured workload, not during it.
    procedure PrintSummary;
end Trace;
