pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Passive_Members;
with ACPI_Passive_Query.Fields;
with Firmware_Tables;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Reader is new ACPI_Passive_Members (Core => Core);
package ACPI_Passive_Identity with SPARK_Mode is
   package Wire renames ACPI_Passive_Query;
   package Fields renames ACPI_Passive_Query.Fields;
   subtype Member_Name is Fields.Field_Name range Fields.Hardware_ID .. Fields.Device_Status;
   type Outcome is (Field_Ready, End_Of_List, Bad_Position, Scratch_Required,
                    Output_Required, Observation_Failed);
   type Report is record
      Status : Outcome := Observation_Failed;
      Text_Bytes, Required_Text_Bytes : Wire.Word := 0;
      More_CIDs : Boolean := False;
      Observation : Reader.Report;
   end record;
   use type Firmware_Tables.Byte;
   -- Owner has already validated the live device, observer role and scope.
   -- Reads only direct static metadata, never executes AML or Field IO.
   -- CID Ordinal selects one complete element; UID Offset permits text chunks.
   -- Text's copied prefix belongs at Pool_Offset in the final Describe payload;
   -- the owner must reserve the record area and rebase when composing a page.
   -- All arrays are separate private buffers. Scratch is sized for the complete
   -- observed value, even when only a UID chunk or one CID is emitted.
   procedure Read
     (Service : in out Core.State; Device : Core.Node_Descriptor; Member : Member_Name;
      Ordinal : Fields.U32; Offset : Wire.Word; Pool_Offset : Fields.U32;
      Scratch_Data : out Firmware_Tables.Bytes; Scratch_Atoms : out Reader.Atoms;
      Text : out Firmware_Tables.Bytes; Field : out Fields.Field_Result; Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then (if Result.Status = Field_Ready then Field.Accepted)
       and then (if Result.Status /= Field_Ready then
         not Field.Accepted and then (for all B of Text => B = 0));
end ACPI_Passive_Identity;
