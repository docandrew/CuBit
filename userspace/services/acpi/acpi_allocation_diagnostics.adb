pragma Ada_2022;
package body ACPI_Allocation_Diagnostics with SPARK_Mode is
   function Digits_Of (Value : Count) return String is
      Text : constant String := Value'Image;
   begin return Text (Text'First + 1 .. Text'Last); end Digits_Of;
   function Format (Item : Report) return String is
      function Component_Name return String is
        (case Item.Component is when No_Component => "NONE",
         when Snapshot_Catalog => "SNAPSHOT_CATALOG", when Copied_Input => "COPIED_INPUT",
         when Table_Copy => "TABLE_COPY", when AML_Owner => "AML_OWNER");
      function Limit_Name return String is
        (case Item.Limit is when No_Limit => "NONE",
         when Entry_Representation => "ENTRY_REPRESENTATION",
         when Byte_Representation => "BYTE_REPRESENTATION", when Process_Storage => "PROCESS_STORAGE");
      function Unit_Name return String is
        (case Item.Unit is when Entries => "entries", when Payload_Bytes => "payload-bytes",
         when Owner_Graph_Bytes => "owner-graph-bytes",
         when Owner_Block_Bytes => "owner-block-bytes");
   begin
      return "component=" & Component_Name & " limit=" & Limit_Name &
        " demand=" & Digits_Of (Item.Demand) & " unit=" & Unit_Name &
        (if Item.Limit in Entry_Representation | Byte_Representation
         then " bound=" & Digits_Of (Item.Bound) else "");
   end Format;
   function Format (Item : Value_Sample) return String is
   begin
      if not Item.Available then return "sample=UNAVAILABLE"; end if;
      return "sample=EXIT objects=" & Digits_Of (Item.Objects_Used) & "/" & Digits_Of (Item.Objects_Capacity) &
        " bytes=" & Digits_Of (Item.Bytes_Used) & "/" & Digits_Of (Item.Bytes_Capacity) &
        " elements=" & Digits_Of (Item.Elements_Used) & "/" & Digits_Of (Item.Elements_Capacity);
   end Format;
end ACPI_Allocation_Diagnostics;
