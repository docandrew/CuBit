pragma Ada_2022;
with ACPI_GPE_Lifecycle;
-- Analysis-only concrete ID type and abstract backend. Imported callbacks have
-- arbitrary output values and no global effects under these assumed contracts.
-- This is not a native provider, exception model or hardware implementation.
package GPE_Flow_Instance with SPARK_Mode is
   package G renames ACPI_GPE_Lifecycle;
   type Source_ID is range 0 .. 255;
   No_Source : constant Source_ID := 0;
   procedure Mask (Source : Source_ID; Result : out G.IO_Outcome)
     with Import, Global => null;
   procedure Clear (Source : Source_ID; Result : out G.IO_Outcome)
     with Import, Global => null;
   procedure Enable (Source : Source_ID; Result : out G.IO_Outcome)
     with Import, Global => null;
   procedure Invoke
     (Source : Source_ID; Budget : Natural; Result : out G.Handler_Outcome;
      Charged : out Natural) with Import, Global => null;
   package Controller is new G.Driver (Source_ID, No_Source, Mask, Clear, Enable, Invoke);
end GPE_Flow_Instance;
