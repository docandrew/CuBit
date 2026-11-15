pragma Ada_2022;
with AML_Decode;
with AML_Execute;
-- Copied field-operation metadata only. No Arena, handle, target authority or
-- hardware address escapes. This is an operation boundary, not an asserted
-- bytecode failure PC: callers must keep those provenance claims distinct.
package AML_Field_Notes with Pure, SPARK_Mode is
   type Node_Position is new Natural;
   type Boundary is (Requested, Resolving, Resolution_Finished, Pending_Refused, Work_Refused,
      Lock_Policy_Refused, Entry_Refused, Finished);
   type Event is record
      At_Point : Boundary := Requested;
      Known_Field, Known_Region, For_Write : Boolean := False;
      Field, Region : Node_Position := 0;
      Space, Flags : AML_Decode.Byte := 0;
      Bit_Offset, Bit_Length : AML_Decode.Integer_Value := 0;
      Direct_Binding, Writes_Enabled, Locked_Fields_Enabled : Boolean := False;
      Read_Datums : Natural := 0;
      Status : AML_Execute.Execution_Status := AML_Execute.Unsupported_Value;
   end record;
end AML_Field_Notes;
