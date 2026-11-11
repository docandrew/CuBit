pragma Ada_2022;
with AML_Decode;
-- Copies of diagnostic syntax/table-index metadata only. No value, arena,
-- source handle, physical address or permission is exposed to an observer.
package AML_Profile_Events with Pure, SPARK_Mode is
   type Event_Kind is (Statement_Visit, Operand_Visit, Table_Enter, Table_Leave);
   type Code_Position is new Natural;
   type Logical_Table_ID is new Natural;
   Unknown_Table : constant Logical_Table_ID := 0;
   Extended_Prefix : constant AML_Decode.Byte := 16#5B#;
   No_Extension : constant AML_Decode.Byte := 0;
   type Event is record
      Kind : Event_Kind := Statement_Visit;
      Position : Code_Position := 0;
      Opcode : AML_Decode.Byte := 0;
      Has_Extension : Boolean := False;
      Extension : AML_Decode.Byte := No_Extension;
      Logical_Table : Logical_Table_ID := Unknown_Table;
   end record;
end AML_Profile_Events;
