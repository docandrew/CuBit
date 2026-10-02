-- Process-lifetime storage; Start may be entered only once. Fatal returns
-- retain the adapter until kernel process teardown retires any outstanding loan.
package ACPI_Native_Instance is
   procedure Start;
end ACPI_Native_Instance;
