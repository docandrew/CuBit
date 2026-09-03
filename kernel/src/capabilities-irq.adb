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
        status := False;

        --  Make registration idempotent. This matters when a device manager
        --  retries setup after a driver restart.
        for index in IRQOwnerIndex loop
            if irqOwners(vector)(index) = pid then
                status := True;
                return;
            end if;
        end loop;

        for index in IRQOwnerIndex loop
            if irqOwners(vector)(index) = 0 then
                irqOwners(vector)(index) := pid;
                status := True;
                return;
            end if;
        end loop;
    end registerIRQ;

    ---------------------------------------------------------------------------
    -- unregisterIRQ
    ---------------------------------------------------------------------------
    procedure unregisterIRQ (vector : IRQVector;
                             pid    : Unsigned_64)
    is
    begin
        for index in IRQOwnerIndex loop
            if irqOwners(vector)(index) = pid then
                irqOwners(vector)(index) := 0;
            end if;
        end loop;
    end unregisterIRQ;

    ---------------------------------------------------------------------------
    -- getOwner
    ---------------------------------------------------------------------------
    function getOwner (vector : IRQVector;
                       index  : IRQOwnerIndex) return Unsigned_64 is
    begin
        return irqOwners(vector)(index);
    end getOwner;

    ---------------------------------------------------------------------------
    -- unregisterAllByPID
    ---------------------------------------------------------------------------
    procedure unregisterAllByPID (pid : Unsigned_64) is
    begin
        for v in IRQVector loop
            for index in IRQOwnerIndex loop
                if irqOwners(v)(index) = pid then
                    irqOwners(v)(index) := 0;
                end if;
            end loop;
        end loop;
    end unregisterAllByPID;

end Capabilities.IRQ;
