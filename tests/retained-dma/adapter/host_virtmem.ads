with System;
with System.Storage_Elements;
package Virtmem is
   subtype PhysAddress is System.Storage_Elements.Integer_Address;
   function P2Va (Frame : PhysAddress) return System.Address
     renames System.Storage_Elements.To_Address;
end Virtmem;
