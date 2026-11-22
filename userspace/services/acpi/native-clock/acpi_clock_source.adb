with CuBit.Monotonic;
package body ACPI_Clock_Source with SPARK_Mode => Off is
   procedure Read (Value : out AML_Decode.Integer_Value; Available : out Boolean) is
      Sample : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      Available := Sample.Available;
      Value := 0;
      if Sample.Available then Value := Sample.Microseconds; end if;
   end Read;
end ACPI_Clock_Source;
