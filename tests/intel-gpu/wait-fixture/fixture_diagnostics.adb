--  Hosted stand-in body: these unit tests publish no lines.
package body Intel_GPU_Diagnostics is
   procedure Capture (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information) is null;
   function Flush (Limit_Ms : Interfaces.Unsigned_64) return Boolean is (True);
end Intel_GPU_Diagnostics;
