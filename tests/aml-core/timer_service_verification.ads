with AML_Delays;
with ACPI_Service_Core;
with AML_Decode;
package Timer_Service_Verification with SPARK_Mode is
   use type AML_Decode.Integer_Value;
   -- Explicit software model of input from a clock provider. This is not
   -- hardware verification; flow must account for both inputs at every layer.
   Sample_Microseconds : AML_Decode.Integer_Value := 0;
   Sample_Available : Boolean := True;
   procedure Read_Raw
     (Value : out AML_Decode.Integer_Value; Available : out Boolean)
     with Global => (Input => (Sample_Microseconds, Sample_Available)),
       Post => Value = Sample_Microseconds and Available = Sample_Available;
   package Service is new ACPI_Service_Core (AML_Delays.Unavailable_Provider, Read_Raw);
end Timer_Service_Verification;
