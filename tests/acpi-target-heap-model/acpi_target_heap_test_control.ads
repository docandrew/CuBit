pragma Ada_2022;
-- Hosted allocator fault injection only. Never copied to production sources.
package ACPI_Target_Heap_Test_Control is
   Successful_Allocation_Budget : Natural := Natural'Last;
   Attempts : Natural := 0;
end ACPI_Target_Heap_Test_Control;
