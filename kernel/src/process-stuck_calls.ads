-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- The on-screen stuck-call and evidence panel (moved from Process.IPC,
-- IPC revision 2 S0): the visibility tool for machines without serial.
-------------------------------------------------------------------------------
package Process.Stuck_Calls is

    --  Diagnostics for machines without serial: every Stuck_Report_Period_Ms
    --  the time keeper's timer draws the threads blocked in a synchronous call for at
    --  least Stuck_Call_Ms (Boot_Diagnostics.Stuck_Calls), with the server's
    --  main-thread state. Reads thread records without locks: a torn line is
    --  possible and harmless; it grants and changes nothing. Each line is
    --  also printed to serial as "STUCK-CALL: ...".
    Stuck_Report_Period_Ms : constant := 2_000;
    Stuck_Call_Ms          : constant := 3_000;
    procedure reportStuckCalls (nowMs : Unsigned_64);

    --  Evidence for the same panel (the screen is what survives a hang
    --  without serial or logs): a few short kernel verdicts (the clock's,
    --  a DMA fault's), drawn above the stuck calls at the next reports for
    --  Evidence_Show_Ms after the latest. Text past a row is cut. Any CPU,
    --  interrupts masked or not.
    Evidence_Rows    : constant := 3;
    Evidence_Show_Ms : constant := 60_000;
    procedure noteEvidence (text : String);

end Process.Stuck_Calls;
