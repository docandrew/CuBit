-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- Idle Service
-------------------------------------------------------------------------------
with x86;

package body Services.Idle is

    procedure start is
    begin
        loop
            x86.halt;
        end loop;
    end start;

end Services.Idle;
