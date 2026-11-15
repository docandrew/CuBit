with ACPI_Battery_Legacy_Information;
package Battery_Format_Instances with SPARK_Mode is
   -- Exact capacities currently instantiated by the hosted callers and native
   -- service. Proof of these does not establish every possible generic budget.
   package Small is new ACPI_Battery_Legacy_Information (8);
   package Selection is new ACPI_Battery_Legacy_Information (16);
   package Native is new ACPI_Battery_Legacy_Information;
end Battery_Format_Instances;
