-------------------------------------------------------------------------------
-- Cubit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Debugging Routines
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Textmode; use Textmode;

package body Debug is
    procedure dumpMem(base : System.Address; len : Natural) with
        SPARK_Mode => Off   -- Storage Elements
    is
        mem : Storage_Array(1..Storage_Offset(len))
            with Import, Address => base;
    begin
        for i in mem'Range loop
            if i mod 32 = 0 then
                println;
            end if;

            print(Unsigned_8(mem(i))); print(" ");
        end loop;
    end;
end Debug;