with Interfaces;
with System;
with System.Storage_Elements;
package Virtmem is
   subtype PhysAddress is Interfaces.Unsigned_64;
   STACK_TOP : constant System.Storage_Elements.Integer_Address := 16#20_0000#;
   function V2P (Value : System.Address)
     return System.Storage_Elements.Integer_Address is
       (System.Storage_Elements.To_Integer (Value));
end Virtmem;
