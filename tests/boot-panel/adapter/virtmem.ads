with System;
with System.Storage_Elements;
package Virtmem is
   function P2Va (Value : System.Storage_Elements.Integer_Address) return System.Address
     renames System.Storage_Elements.To_Address;
end Virtmem;
