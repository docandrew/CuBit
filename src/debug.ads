-------------------------------------------------------------------------------
-- Cubit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Debugging Routines
-------------------------------------------------------------------------------
with System;

package Debug with
    SPARK_Mode => On
is
    procedure dumpMem(base : System.Address; len : Natural);
end Debug;