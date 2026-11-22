------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  PASS/FAIL lines for ada-runtime-check (headless --test ada-runtime).
------------------------------------------------------------------------------
pragma Ada_2022;
package Check_Results is

   procedure Check (Condition : Boolean; Name : String);
   --  Print "ada-runtime-check: NAME PASS" or "... FAIL" and count failures.

   procedure Note (Text : String);
   --  Print "ada-runtime-check: TEXT" (diagnostic values).

   function Failures return Natural;

end Check_Results;
