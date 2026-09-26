-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- Idle Service
-------------------------------------------------------------------------------
with PerCPUData;
with Process;
with x86;

package body Services.Idle is

    procedure start is
    begin
        loop
            -- Quiescent point: an idle CPU holds no process-table record,
            -- and may not pass through its scheduler for a long time.
            Process.Process_Table.Quiescent (PerCPUData.getCPUNumber);
            Process.Thread_Table.Quiescent (PerCPUData.getCPUNumber);
            x86.halt;
        end loop;
    end start;

end Services.Idle;
