pragma Ada_2022;
-- One diagnostic capture through the already owned read-only bus-zero window.
-- No setup, mapping, PCI write, authority change or retry. Results are facts
-- for the platform owner to evaluate; this unit cannot admit pool memory.
package ACPI_Native_LPC_Probe is
   procedure Capture;
   -- Established logging ring only; bounded copied observations after capture.
   procedure Report;
end ACPI_Native_LPC_Probe;
