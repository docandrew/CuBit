--  The hosted tests load startup tables themselves and check them against
--  Startup.Expected; the generated body's startup check (CuBit.Startup_Check,
--  which stops the process) runs only on CuBit.
package body Startup with SPARK_Mode is
end Startup;
