-- Minimal test reporting: count checks, print failures, exit status.
package Checks is
   procedure Check (Condition : Boolean; What : String);
   procedure Section (Name : String);
   function Failures return Natural;
   function Passed return Natural;
   procedure Finish;  -- prints the summary and sets the exit status
end Checks;
