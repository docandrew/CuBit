pragma Ada_2022;
with Interfaces.C;
with ACPI_Target_Heap_Test_Control;
package body ACPI_Target_Heap is
   use System.Storage_Elements;
   use type Interfaces.C.int;
   function Aligned_Allocate
     (Item : out System.Address; Alignment, Bytes : Interfaces.C.size_t) return Interfaces.C.int
     with Import, Convention => C, External_Name => "posix_memalign";
   procedure Free (Item : System.Address)
     with Import, Convention => C, External_Name => "free";
   Minimum_Alignment : constant Storage_Count := System.Address'Size / System.Storage_Unit;
   function Allocate
     (Bytes : Storage_Count; Alignment : Storage_Count) return System.Address is
      Item : System.Address;
      Status : Interfaces.C.int;
   begin
      ACPI_Target_Heap_Test_Control.Attempts := ACPI_Target_Heap_Test_Control.Attempts + 1;
      if ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget = 0 then
         return System.Null_Address;
      end if;
      Status := Aligned_Allocate (Item,
        Interfaces.C.size_t (Storage_Count'Max (Alignment, Minimum_Alignment)),
        Interfaces.C.size_t (Bytes));
      if Status /= 0 then return System.Null_Address; end if;
      ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget :=
        ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget - 1;
      return Item;
   end Allocate;
   procedure Release (Item : System.Address) is
   begin Free (Item); end Release;
end ACPI_Target_Heap;
