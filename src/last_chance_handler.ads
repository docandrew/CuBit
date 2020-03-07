-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary Ada Exception Handling
--
-- @description For bare metal runtime, we don't support any notion of
-- exception propagation, but would still like the ability to panic when a
-- required operation fails. When we call Raise_Exception, it will result in a
-- call to the Last_Chance_Handler specified here.
-------------------------------------------------------------------------------
with System;

package Last_Chance_Handler with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Last_Chance_Handler is inserted by the compiler into the same subprogram
    -- where an exception is raised, if not immediately caught by the same
    -- subprogram.
    --
    -- TODO: Make this halt all the other processors.
    ---------------------------------------------------------------------------
    procedure Last_Chance_Handler(msg : System.Address; line : Integer)
    with
        Export => True,
        Convention => C,
        External_Name => "__gnat_last_chance_handler";
end Last_Chance_Handler;