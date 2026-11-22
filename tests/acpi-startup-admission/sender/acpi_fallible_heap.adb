-- Hosted test model only; never a native runtime/provider implementation.
with Sender_Model; with System.Storage_Elements;
package body ACPI_Fallible_Heap is
 use type Interfaces.Unsigned_64, System.Address, System.Storage_Elements.Integer_Address;
 function Reserve(Bytes,Alignment : Interfaces.Unsigned_64) return System.Address is
 begin
 Sender_Model.Allocations:=Sender_Model.Allocations+1;
 Sender_Model.Assert(Bytes=4096 and Alignment=4096);
 if not Sender_Model.Allocate_OK then return System.Null_Address; end if;
 if Sender_Model.Mode=27 then return System.Storage_Elements.To_Address(System.Storage_Elements.To_Integer(Sender_Model.Buffer'Address)+1); end if;
 return Sender_Model.Buffer'Address;
 end Reserve;
 procedure Release(Address : System.Address) is begin
 Sender_Model.Assert(Address/=System.Null_Address); Sender_Model.Frees:=Sender_Model.Frees+1;
 end Release;
end ACPI_Fallible_Heap;