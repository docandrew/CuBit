------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The test console object (Test_Console_ABI): one line of text onto the
--  kernel's serial console, through Call on a Test_Console handle.
--
--  @description
--  A kernel object: the Call runs in the kernel with no process switch.
--  Only test builds' logstore holds one, to mirror programs' debug text
--  (CuBit.Console_Route) onto the serial console the headless tests read.
------------------------------------------------------------------------------
pragma Ada_2022;
with Test_Console_ABI;

package CuBit.Handles.Test_Consoles is

   package ABI renames Test_Console_ABI;

   type Outcome (Answered : Boolean := False) is record
      case Answered is
         --  The kernel's answer (Done, or why not).
         when True  => Result : ABI.Outcome := ABI.Malformed;
         --  The handle names no test console, or the reply was unreadable.
         when False => null;
      end case;
   end record;

   function Done (Result : Outcome) return Boolean is
     (Result.Answered and then ABI."=" (Result.Result, ABI.Done));

   --  Print Text (1 .. ABI.Maximum_Bytes bytes, as they are).
   function Write (Object : Test_Console; Text : String) return Outcome;

end CuBit.Handles.Test_Consoles;
