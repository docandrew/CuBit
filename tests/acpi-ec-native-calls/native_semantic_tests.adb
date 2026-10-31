with ACPI_EC_Types;
with ACPI_EC_Transactions;
with ACPI_EC_Registers;
with ACPI_EC_Endpoint;
with ACPI_EC_Native_Calls;
procedure Native_Semantic_Tests is
   procedure Read
     (Register : ACPI_EC_Types.Read_Register; Value : out ACPI_EC_Types.Byte;
      Result : out ACPI_EC_Types.Access_Result) is
      pragma Unreferenced (Register);
   begin Value := 0; Result := ACPI_EC_Types.Refused; end Read;
   procedure Write
     (Register : ACPI_EC_Types.Write_Register; Value : ACPI_EC_Types.Byte;
      Result : out ACPI_EC_Types.Access_Result) is
      pragma Unreferenced (Register, Value);
   begin Result := ACPI_EC_Types.Refused; end Write;
   procedure Clock (Now : out ACPI_EC_Types.Microseconds; Available : out Boolean) is
   begin Now := 0; Available := False; end Clock;
   function Owned return Boolean is (False);
   package Engine is new ACPI_EC_Transactions (Read, Write, Clock, Owned);
   package Registers is new ACPI_EC_Registers (Engine);
   package Calls is new ACPI_EC_Endpoint (Registers);
   package Native is new ACPI_EC_Native_Calls (Registers, Calls);
   Result : Native.Report;
begin
   Native.Advance (Result);
end Native_Semantic_Tests;
