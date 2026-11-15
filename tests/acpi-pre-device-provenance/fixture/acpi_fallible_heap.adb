-- Hosted test model only; never a native runtime/provider implementation.
with Sender_Model; with Prep_Model; package body ACPI_Fallible_Heap is
 use type Interfaces.Unsigned_64,System.Address;
 function Reserve(Bytes,Alignment : Interfaces.Unsigned_64) return System.Address is begin
 if Bytes=4096 then Sender_Model.Allocations:=Sender_Model.Allocations+1; Sender_Model.Assert(Alignment=4096); return Sender_Model.Buffer'Address;
 else Prep_Model.Raw_Allocations:=Prep_Model.Raw_Allocations+1;
 if Prep_Model.Mode=5 then return System.Null_Address; end if;
 Sender_Model.Assert(Bytes<=32); return Prep_Model.Raw'Address; end if;
 end Reserve;
 procedure Release(Address : System.Address) is begin
 if Address=Sender_Model.Buffer'Address then Sender_Model.Frees:=Sender_Model.Frees+1;
 elsif Address=Prep_Model.Raw'Address then Prep_Model.Raw_Frees:=Prep_Model.Raw_Frees+1;
 else raise Program_Error; end if;
 end Release;
end ACPI_Fallible_Heap;