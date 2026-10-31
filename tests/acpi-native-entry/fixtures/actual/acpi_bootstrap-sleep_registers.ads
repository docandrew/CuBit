pragma Ada_2022;
with ACPI_FADT.Sleep_Registers;
package ACPI_Bootstrap.Sleep_Registers with SPARK_Mode is
   -- Exact immutable catalog only; reject missing/duplicate/malformed FADT.
   -- Device initialization may still be blocked on an unavailable region provider.
   -- Executes no AML and changes no ownership,notification state or hardware.
   procedure Observe (Session : State; Result : out ACPI_FADT.Sleep_Registers.Source_Report)
     with Pre => Valid (Session);
end ACPI_Bootstrap.Sleep_Registers;
