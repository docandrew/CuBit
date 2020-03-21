-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CMOS & Real-Time Clock (RTC)
-------------------------------------------------------------------------------

package body cmos with
    SPARK_Mode => On
is

    -- NMIs are controlled through the CMOS controller on legacy systems.
    -- We don't mess with it here, for now.
    CMOS_NMI_DISABLED   : constant Unsigned_8 := 16#80#;    -- bit 7
    CMOS_NMI_ENABLED    : constant Unsigned_8 := 16#00#;

    cmosAddr            : constant x86.IOPort := 16#70#;
    cmosData            : constant x86.IOPort := 16#71#;

    ---------------------------------------------------------------------------
    -- write CMOS
    ---------------------------------------------------------------------------
    procedure write(reg : in CMOSRegister; val : in Unsigned_8)
    with
        SPARK_Mode => On
    is
        -- we'll restore this interrupt status
        priorFlags : x86.RFlags := x86.getFlags;
        
        -- cmosNMIStatus : constant Boolean := 
        --     (if disableNMI then CMOS_NMI_DISABLED else CMOS_NMI_ENABLED);
    begin
        x86.cli;

        x86.out8(cmosAddr, Unsigned_8(reg));
        -- TODO: small delay here?
        x86.out8(cmosData, val);

        if priorFlags.interrupt then
            x86.sti;
        end if;
    end write;

    ---------------------------------------------------------------------------
    -- read CMOS
    ---------------------------------------------------------------------------
    procedure read(reg : in CMOSRegister; ret : out Unsigned_8)
    with
        SPARK_Mode => On
    is
        -- we'll restore this interrupt status
        priorFlags : x86.RFlags := x86.getFlags;

    begin
        x86.cli;

        x86.out8(cmosAddr, Unsigned_8(reg));
        -- TODO: small delay here?
        x86.in8(cmosData, ret);

        if priorFlags.interrupt then
            x86.sti;
        end if;
    end read;

end cmos;