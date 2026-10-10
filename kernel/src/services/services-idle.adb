-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- Idle Service
-------------------------------------------------------------------------------
with PerCPUData;
with Process;
with TextIO;
with x86;

package body Services.Idle is

    procedure start is
        More : Boolean;
    begin
        -- An idle thread now exists to drain the console.
        TextIO.startAsynchronous;
        loop
            -- Quiescent point: an idle CPU holds no process-table record,
            -- and may not pass through its scheduler for a long time.
            Process.Process_Table.Quiescent (PerCPUData.getCPUNumber);
            Process.Thread_Table.Quiescent (PerCPUData.getCPUNumber);
            -- Queued console output is written here, off every caller's
            -- path. Halt once it is written or the UART is still busy.
            TextIO.drainConsole (More);
            if not More then
                x86.halt;
            end if;
        end loop;
    end start;

end Services.Idle;
