with CuBit.Messages;
with ACPI_Native_Query_Input;
with ACPI_Query_Admission;
-- Compile-only generic binding against actual runtime APIs. It supplies no
-- real wire selector/layout and is never a native runtime test.
package Native_Query_Input_Model is
   procedure Decode
     (Packet : CuBit.Messages.Message; Selected, Decoded : out Boolean;
      Reference_Word, Declared_Length : out ACPI_Query_Admission.Wire.Word);
   package Input is new ACPI_Native_Query_Input (Decode);
end Native_Query_Input_Model;
