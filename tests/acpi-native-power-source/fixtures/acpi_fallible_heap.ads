with Interfaces;with System;
package ACPI_Fallible_Heap is
   Fail_Next:Boolean:=False;
   Reserves,Releases:Natural:=0;
   Last_Bytes:Interfaces.Unsigned_64:=0;
   function Reserve(Bytes,Alignment:Interfaces.Unsigned_64) return System.Address;
   procedure Release(Address:System.Address);
end ACPI_Fallible_Heap;
