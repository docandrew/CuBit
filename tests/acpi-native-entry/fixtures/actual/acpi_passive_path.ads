pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Passive_Query.Fields;
with Firmware_Tables;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Passive_Path with SPARK_Mode is
   package Wire renames ACPI_Passive_Query;
   package Fields renames ACPI_Passive_Query.Fields;
   type Node_Array is array (Natural range <>) of Core.Namespace.Node_ID;
   type Outcome is (Field_Ready, Bad_Position, Invalid_Device, Scratch_Required,
                    Output_Required, Observation_Failed);
   type Report is record
      Status : Outcome := Observation_Failed;
      Required_Nodes, Visited_Nodes, Total_Segments, Copied_Segments : Natural := 0;
      Text_Bytes, Required_Text_Bytes : Wire.Word := 0;
      Next_Segment : Fields.U32 := 0;
      More : Boolean := False;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
   end record;
   use type Firmware_Tables.Byte;
   -- Canonical rooted name: each untrimmed four-character segment has a
   -- one-byte prefix (root backslash first, dot thereafter). Path cursor Offset
   -- counts completed segments, NOT bytes or physical namespace slots. UID
   -- cursors instead count bytes; CID cursors count list ordinals.
   -- Owner validates role/scope/revision and serializes access. Scratch is the
   -- reverse parent chain; it is private and confers no authority. No AML/IO.
   -- Visited_Nodes counts parent-chain hops (at most Max_Namespace_Nodes).
   -- Rendering then describes each copied segment once more. This is a bound
   -- on namespace work, not a wall-clock or complete service operation budget.
   procedure Read
     (Service : Core.State; Device : Core.Node_Descriptor;
      Segment_Offset, Pool_Offset : Fields.U32; Scratch : out Node_Array;
      Text : out Firmware_Tables.Bytes; Field : out Fields.Field_Result; Result : out Report)
     with Pre => Core.Valid (Service), Post =>
       Result.Visited_Nodes <= Core.Max_Namespace_Nodes
       and then (if Result.Status = Field_Ready then Field.Accepted)
       and then (if Result.Status /= Field_Ready then
         not Field.Accepted and then (for all B of Text => B = 0));
end ACPI_Passive_Path;
