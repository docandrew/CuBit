with AML_Decode;
-- Trusted syscall boundary; the clock is external, asynchronously changing
-- input. No hardware address is supplied by AML or by the service caller.
package ACPI_Clock_Source with SPARK_Mode,
  Abstract_State => (Hardware_Clock with External => Async_Writers)
is
   use type AML_Decode.Integer_Value;
   procedure Read (Value : out AML_Decode.Integer_Value; Available : out Boolean)
     with Global => (Input => Hardware_Clock),
       Post => (if not Available then Value = 0);
end ACPI_Clock_Source;
