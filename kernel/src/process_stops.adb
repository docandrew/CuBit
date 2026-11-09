-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Typed stop reports into the kernel log ring (process_stops.ads).
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with Fault_Reports;
with Kernel_Log;
with Process_Identities;

package body Process_Stops with
    SPARK_Mode => Off -- the process table and the ring
is
    package FR renames Fault_Reports;

    function word (A : System.Address) return Unsigned_64 is
        (Unsigned_64 (To_Integer (A)));

    -- What Process keeps of Victim's address space (unlocked reads: the
    -- process is stopping; a diagnostic snapshot).
    function layoutOf (Victim : Process.ProcessID) return FR.Layout is
        P : constant Process.Process_Table.Element_Ref := Process.proctab (Victim);
    begin
        return (Stack_First => word (P.stackBottom), Stack_Limit => word (P.stackTop),
                Heap_First  => word (P.heapStart),   Heap_Limit  => word (P.heapEnd),
                Image_First => word (P.istart),      Image_Limit => word (P.iend));
    end layoutOf;

    procedure append (Victim : Process.ProcessID; Fault : FR.Report) is
    begin
        Kernel_Log.Append_Stop
          (Source => Process_Identities.To_Word (Process.identityOf (Victim)),
           Name   => Process.proctab (Victim).name,
           Fault  => Fault);
    end append;

    procedure Report_Exception
      (Victim : Process.ProcessID;
       Vector, Error_Code, Address, RIP, RSP : Unsigned_64) is
    begin
        if Victim = Process.NO_PROCESS then
            return;
        end if;
        append (Victim, FR.Exception_Report
                  (Vector, Error_Code, Address, RIP, RSP, layoutOf (Victim)));
    end Report_Exception;

    procedure Report_Out_Of_Memory
      (Victim : Process.ProcessID; Address, RIP, RSP : Unsigned_64) is
    begin
        if Victim = Process.NO_PROCESS then
            return;
        end if;
        append (Victim, FR.Out_Of_Memory_Report (Address, RIP, RSP, layoutOf (Victim)));
    end Report_Out_Of_Memory;

    procedure Report_Stopped (Victim, Stopper : Process.ProcessID) is
    begin
        if Victim = Process.NO_PROCESS then
            return;
        end if;
        append (Victim, FR.Stopped_Report
                  (if Stopper = Process.NO_PROCESS then 0
                   else Process_Identities.To_Word (Process.identityOf (Stopper))));
    end Report_Stopped;
end Process_Stops;
