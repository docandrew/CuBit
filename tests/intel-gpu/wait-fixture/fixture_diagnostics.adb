with CuBit.Messages;
--  Hosted stand-in body: completions come straight from the fixture queue
--  (no log grant in flight in these unit tests).
package body Intel_GPU_Diagnostics is
   procedure Capture (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information) is null;
   procedure Tick is null;
   function Poll_Driver (Result : System.Address) return Interfaces.Unsigned_64 is
     (CuBit.Messages.Poll_Completion (Result));
   function Pending return Boolean is (False);
end Intel_GPU_Diagnostics;
