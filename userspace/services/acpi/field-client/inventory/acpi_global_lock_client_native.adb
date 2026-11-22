pragma Ada_2022;
with CuBit.Monotonic;
package body ACPI_Global_Lock_Client_Native is
   package body Callbacks is
      procedure Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean) is
         Sample : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
      begin
         Available := Sample.Available; Now := (if Sample.Available then Sample.Microseconds else 0);
      end Clock;
   end Callbacks;
end ACPI_Global_Lock_Client_Native;
