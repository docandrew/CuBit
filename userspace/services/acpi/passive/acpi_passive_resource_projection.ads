pragma Ada_2022;
with ACPI_Resource_Data;
with ACPI_Passive_Query.Resource_Records;
with Firmware_Tables;
package ACPI_Passive_Resource_Projection with SPARK_Mode, Pure is
   package Wire renames ACPI_Passive_Query;
   package Records renames ACPI_Passive_Query.Resource_Records;
   package Data_IO renames ACPI_Resource_Data;
   type Outcome is (Record_Ready, Empty_Descriptor, End_Tag, Bad_Subentry,
                    Malformed_Descriptor, Output_Required, Invalid_Pool_Offset);
   type Report is record
      Status : Outcome := Malformed_Descriptor;
      Next_Offset : Data_IO.Byte_Offset := 0;
      Next_Subentry : Records.U32 := 0;
      Source_Bytes, Required_Source_Bytes : Wire.Word := 0;
   end record;
   use type Firmware_Tables.Byte;
   -- Projects one descriptor subentry from a stable private template. The page
   -- owner must Scan the full template and establish Offset at an actual frame
   -- boundary; a client byte offset alone does not establish that boundary.
   -- Known malformed descriptors refuse; unknown/unsupported descriptors retain
   -- their encoding/name/offset as explicit records. No resource admission,
   -- hardware authority, controller resolution or namespace access occurs.
   -- Full unresolved source spans (including encoded NUL) are copied intact.
   procedure Read
     (Data : Firmware_Tables.Bytes; Offset : Data_IO.Byte_Offset;
      Subentry, Pool_Offset : Records.U32; Source : out Firmware_Tables.Bytes;
      Item : out Records.Resource_Result; Result : out Report)
     with Global => null, Post =>
       (if Result.Status = Record_Ready then Item.Accepted
        else not Item.Accepted and then (for all B of Source => B = 0));
end ACPI_Passive_Resource_Projection;
