-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Ada Exception Handling
-------------------------------------------------------------------------------
with System.Storage_Elements;
with Textmode; use Textmode;
with x86;

package body Last_Chance_Handler with
    SPARK_Mode => On
is

    procedure Last_Chance_Handler(msg : System.Address; line : Integer)
    with
        SPARK_Mode => On
    is
        use System.Storage_Elements;    -- get "+" operator on System.Address

        -- function peek(addr : System.Address) return Character
        -- is
        --     c : Character with Address => addr;
        -- begin
        --     return c;
        -- end peek;

        -- a : System.Address := sourceLoc;
    begin
        if line /= 0 then
            print("EXCEPTION: ", RED, BLACK); 
            printz(msg); 
            print(":"); 
            print(line);
        else
            print("EXCEPTION: ", RED, BLACK); 
            printz(msg);
        end if;

        x86.cli;

        loop
            x86.panic;
        end loop;
    end Last_Chance_Handler;

end Last_Chance_Handler;