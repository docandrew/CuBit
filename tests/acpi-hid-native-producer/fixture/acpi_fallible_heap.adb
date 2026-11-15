with Sender_Model;
package body ACPI_Fallible_Heap is
   use type Interfaces.Unsigned_64, System.Address;
   function Reserve (Bytes, Alignment : Interfaces.Unsigned_64) return System.Address is
   begin
      Sender_Model.Assert (Bytes = 4096 and then Alignment = 4096);
      Sender_Model.Allocations := Sender_Model.Allocations + 1;
      if not Sender_Model.Allocate_OK then return System.Null_Address; end if;
      return (if Sender_Model.Allocations = 1 then Sender_Model.Buffer'Address else Sender_Model.Second_Buffer'Address);
   end Reserve;
   procedure Release (Address : System.Address) is
   begin
      if Address = Sender_Model.Second_Buffer'Address then
         Sender_Model.Assert (Sender_Model.Second_Retired); Sender_Model.Second_Frees := Sender_Model.Second_Frees + 1;
      else
         Sender_Model.Assert (Address = Sender_Model.Buffer'Address and then Sender_Model.Confirmed);
         Sender_Model.First_Frees := Sender_Model.First_Frees + 1;
      end if;
      Sender_Model.Frees := Sender_Model.Frees + 1;
   end Release;
end ACPI_Fallible_Heap;
