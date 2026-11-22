pragma Ada_2022;
package ACPI_Native_GPE_Control is
   -- Actual local provider/SCI geometry audit, never a caller-supplied flag.
   -- Requires faithful, separate x86 IO/MMIO resources and serialized owners.
   function Exclusive return Boolean;
end ACPI_Native_GPE_Control;
