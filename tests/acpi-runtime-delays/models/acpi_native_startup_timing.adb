package body ACPI_Native_Startup_Timing is
 procedure Enter (Item : P.Kind; Handle : out Token) is
  pragma Unreferenced (Item); begin Handle := 1; end Enter;
 procedure Leave (Handle : in out Token) is begin Handle := 0; end Leave;
end ACPI_Native_Startup_Timing;
