pragma Ada_2022;
with ACPI_Host_Bridge_Resources;
with ACPI_Host_Bridge_Wire;
-- Producer projection only. Source-name windows remain offsets into the
-- original copied _CRS bytes, which must accompany the eventual snapshot.
package ACPI_Host_Bridge_Projection with SPARK_Mode, Pure is
   package H renames ACPI_Host_Bridge_Resources;
   package W renames ACPI_Host_Bridge_Wire;
   use type W.Row;
   type Outcome is (Complete, Insufficient_Capacity, Invalid_Descriptor);
   procedure Project
     (Items : H.Descriptor_List; Count : H.R.Item_Count;
      Source_Bytes : H.R.Byte_Count; Data : out W.Rows; Status : out Outcome)
     with Global => null,
       Post => (if Status /= Complete then
         (for all I in Data'Range => Data (I) = W.Empty_Row));
end ACPI_Host_Bridge_Projection;
