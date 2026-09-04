-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Ada Exception Handling
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements;

with TextIO; use TextIO;
with Util;
with Virtmem;
with x86;

package body Last_Chance_Handler with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- printCallStack
    -- @TODO I think only works in debug mode when base pointer is used for
    --  stack frame chain
    -- @TODO would be sweet to get debug symbols here.
    ---------------------------------------------------------------------------
    procedure printCallStack is
        rbp : Unsigned_64 := x86.getRBP;
    begin
        println ("Call Stack: ");
        for i in 1..16 loop
            exit when rbp = 0 or 
                      rbp < Unsigned_64(Virtmem.LINEAR_BASE) or
                      rbp = Unsigned_64'Last;

            declare
                rip : Unsigned_64 with
                    Import, Address => Util.numToAddr (rbp + 8);
                nextrbp : Unsigned_64 with
                    Import, Address => Util.numToAddr (rbp);
            begin
                print (i); print (": ");
                println (rip);
                rbp := nextrbp;
            end;
        end loop;
    end printCallStack;

    ---------------------------------------------------------------------------
    -- Last_Chance_Handler
    ---------------------------------------------------------------------------
    procedure Last_Chance_Handler (msg : System.Address; line : Integer)
    with
        SPARK_Mode => On
    is
        use System.Storage_Elements;    -- get "+" operator on System.Address

    begin
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

        printCallStack;        

        x86.cli;

        loop
            x86.panic;
        end loop;
    end Last_Chance_Handler;

end Last_Chance_Handler;
