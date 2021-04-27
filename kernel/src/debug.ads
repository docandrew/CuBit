-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Debugging Routines
-------------------------------------------------------------------------------
with System;

package Debug with
    SPARK_Mode => On
is
    procedure dumpMem(base : in System.Address; len : in Natural);
end Debug;