-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- IRQ Capability Owner Table - Implementation
-------------------------------------------------------------------------------
package body Capabilities.IRQ with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- registerIRQ
    ---------------------------------------------------------------------------
    procedure registerIRQ (vector : IRQVector;
                           pid    : Unsigned_64;
                           status : out Boolean)
    is
    begin
        if irqOwners(vector) = 0 then
            irqOwners(vector) := pid;
            status := True;
        else
            status := False;
        end if;
    end registerIRQ;

    ---------------------------------------------------------------------------
    -- unregisterIRQ
    ---------------------------------------------------------------------------
    procedure unregisterIRQ (vector : IRQVector;
                             pid    : Unsigned_64)
    is
    begin
        if irqOwners(vector) = pid then
            irqOwners(vector) := 0;
        end if;
    end unregisterIRQ;

    ---------------------------------------------------------------------------
    -- getOwner
    ---------------------------------------------------------------------------
    function getOwner (vector : IRQVector) return Unsigned_64 is
    begin
        return irqOwners(vector);
    end getOwner;

end Capabilities.IRQ;
