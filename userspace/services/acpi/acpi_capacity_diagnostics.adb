pragma Ada_2022;
package body ACPI_Capacity_Diagnostics with SPARK_Mode is
   function Format (Item : Report) return String is
      function Cache_Name return String is
        (case Item.Cache is when No_Cache => "NONE", when NVS_Targets => "NVS_TARGETS",
         when Combined_Targets => "COMBINED_TARGETS", when PCI_Targets => "PCI_TARGETS");
      function Limit_Name return String is
        (case Item.Failure is when No_Failure => "NONE",
         when Process_Heap => "PROCESS_HEAP_BUDGET",
         when Entry_Representation => "ENTRY_REPRESENTATION" & Item.Entry_Limit'Image,
         when Byte_Representation => "BYTE_REPRESENTATION" & Item.Byte_Limit'Image);
   begin
      return "cache=" & Cache_Name & " limit=" & Limit_Name
        & " demand=" & Item.Demand'Image & " capacity=" & Item.Capacity'Image
        & " bytes=" & Item.Requested_Bytes'Image;
   end Format;
end ACPI_Capacity_Diagnostics;
