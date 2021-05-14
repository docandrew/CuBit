-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Keyboard handling service. This is the lower-half of the keyboard driver. It
-- is necessary to perform port I/O. This runs as a kernel thread which receives
-- an event from the interrupt handler when a PS/2 interrupt occurs. This
-- thread wakes up, reads the scan code from the I/O port and then sends a
-- message to the upper-half, userspace keyboard driver to process it.
-------------------------------------------------------------------------------

package Services.Keyboard with 
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- start
    -- Entry for keyboard service
    ---------------------------------------------------------------------------
    procedure start;

end Services.Keyboard;
