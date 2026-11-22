pragma Ada_2022;
with Firmware_Tables;
with ACPI_Resource_Data.Address_Spaces.Extended_Descriptors;
with ACPI_Resource_Data.Basic_Descriptors;
with ACPI_Resource_Data.Memory_Ranges;
-- Descriptions from one copied _CRS result. Neither this package nor a
-- Complete report establishes mapping authority or free allocation space.
package ACPI_Host_Bridge_Resources with SPARK_Mode, Pure is
   package R renames ACPI_Resource_Data;
   package A renames R.Address_Spaces;
   package E renames A.Extended_Descriptors;
   package B renames R.Basic_Descriptors;
   package M renames R.Memory_Ranges;
   use type R.Item_Count;
   type Descriptor_Form is (Empty, Address_Space, Extended_Address,
      Basic_Resource, Legacy_Memory);
   type Descriptor (Form : Descriptor_Form := Empty) is record
      Offset, Extent : R.Byte_Count := 0;
      case Form is
         when Empty => null;
         when Address_Space => Address : A.Decode_Result;
         when Extended_Address => Extended : E.Decode_Result;
         when Basic_Resource => Basic : B.Decode_Result;
         when Legacy_Memory => Legacy : M.Decode_Result;
      end case;
   end record;
   type Descriptor_List is array (Positive range <>) of Descriptor;
   type Collection_Status is (Complete, Invalid_Template,
      Unsupported_Descriptor, Invalid_Descriptor, Insufficient_Capacity);
   type Collection_Report is record
      Status : Collection_Status := Invalid_Template;
      Required, Count : R.Item_Count := 0;
      Failure_Offset : R.Byte_Offset := 0;
   end record;
   -- Caller owns Data for the snapshot lifetime: optional source-name offsets
   -- in decoded address items refer to these copied bytes, never to AML memory.
   -- All descriptors retain original ordering/encoding/flags, including zero
   -- length, translation and vendor address claims. Unknown items explicitly
   -- refuse completeness. No geometry is invented for legacy BASE ranges.
   -- Storage is caller-sized, with arbitrary lower bounds and no item ceiling
   -- independent of Data. No partial collection escapes on any failure.
   procedure Collect (Data : Firmware_Tables.Bytes;
      Items : out Descriptor_List; Report : out Collection_Report)
     with Global => null,
       Post => (if Report.Status = Complete then
          Report.Count = Report.Required
          and then Report.Count <= R.Item_Count (Items'Length)
        else Report.Count = 0
          and then (for all I in Items'Range => Items (I).Form = Empty));
end ACPI_Host_Bridge_Resources;
