package body Native_Query_Input_Model is
   procedure Decode
     (Packet : CuBit.Messages.Message; Selected, Decoded : out Boolean;
      Reference_Word, Declared_Length : out ACPI_Query_Admission.Wire.Word) is
      pragma Unreferenced (Packet);
   begin
      Selected := False; Decoded := False; Reference_Word := 0; Declared_Length := 0;
   end Decode;
end Native_Query_Input_Model;
