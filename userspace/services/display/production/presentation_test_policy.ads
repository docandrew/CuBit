with Interfaces;
with System;
package Presentation_Test_Policy is
   Enabled : constant Boolean := False;
   function Verify_Buffer
     (Address : System.Address; Bytes, Frame : Interfaces.Unsigned_64)
      return Boolean is (True);
end Presentation_Test_Policy;
