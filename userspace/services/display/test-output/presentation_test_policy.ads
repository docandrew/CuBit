with Interfaces;
with System;
package Presentation_Test_Policy is
   Enabled : constant Boolean := False;
   Rebind_After_Frame : constant Interfaces.Unsigned_64 := 10_000;
   function Rebind_Enabled return Boolean is (True);
   function Verify_Buffer
     (Address : System.Address; Bytes, Frame : Interfaces.Unsigned_64)
      return Boolean is (True);
end Presentation_Test_Policy;
