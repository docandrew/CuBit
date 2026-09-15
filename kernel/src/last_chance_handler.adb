-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Ada Exception Handling
-------------------------------------------------------------------------------
with TextIO; use TextIO;
with x86;

package body Last_Chance_Handler with
    SPARK_Mode => Off -- trusted runtime/output boundary, not a proved unwinder
is

    ---------------------------------------------------------------------------
    -- Last_Chance_Handler
    ---------------------------------------------------------------------------
    procedure Last_Chance_Handler (msg : System.Address; line : Integer)
    is
    begin
        -- Stop local scheduling before reporting. Other CPUs and NMIs require
        -- a separate coordinated panic protocol; this is a local fatal stop.
        x86.cli;
        --  Normal boot hands the framebuffer to the display service and
        --  disables kernel video logging.  A fatal kernel exception must take
        --  the diagnostic surface back: real hardware may have no serial
        --  console at all.  enableVideo is harmless before a driver is set.
        TextIO.enableVideo;
        TextIO.setCursor (0, 0);
        println ("CUBIT KERNEL PANIC", LT_RED, BLACK);

        if line /= 0 then
            print ("EXCEPTION: ", RED, BLACK); 
            printz (msg); 
            print (":"); 
            println (line);
        else
            print ("EXCEPTION: ", RED, BLACK); 
            printz (msg);
            println;
        end if;

        -- Optimized code does not guarantee a linked RBP frame chain. Do not
        -- reinterpret arbitrary register contents as diagnostic pointers.
        println ("Stack trace unavailable; halting this CPU.");

        loop
            -- panic raises a software interrupt and then an Ada exception;
            -- neither is appropriate inside the last-chance handler itself.
            x86.halt;
        end loop;
    end Last_Chance_Handler;

end Last_Chance_Handler;
