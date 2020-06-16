-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Debugging Routines
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Textmode; use Textmode;

package body Debug is

    procedure dumpMem(base : in System.Address; len : in Natural) with
        SPARK_Mode => Off   -- Storage Elements
    is
        mem : Storage_Array(0 .. Storage_Offset(len) - 1)
            with Import, Address => base;
    begin
        println("-----   MEMORY DUMP   -----");
        for i in mem'Range loop
            if i mod 16 = 0 then
                println;
                print(To_Integer(base) + Integer_Address(i)); print(": ");
            end if;

            print(Unsigned_8(mem(i))); print(" ");
        end loop;
        println;
        println("----- END MEMORY DUMP -----");
    end;

end Debug;
