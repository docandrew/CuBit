pragma Ada_2022;
with ACPI_GPE_Lifecycle;
with Interfaces;
-- Analysis-only concrete ID type and abstract backend. Imported callbacks have
-- arbitrary output values and no global effects under these assumed contracts.
-- This is not a native provider, exception model or hardware implementation.
package GPE_Flow_Instance with SPARK_Mode is
   package G renames ACPI_GPE_Lifecycle;
   type Source_ID is new Interfaces.Unsigned_64;
   No_Source : constant Source_ID := 0;
   procedure Mask (Source : Source_ID; Result : out G.IO_Outcome)
     with Import, Global => null, Always_Terminates;
   procedure Clear (Source : Source_ID; Result : out G.IO_Outcome)
     with Import, Global => null, Always_Terminates;
   procedure Enable (Source : Source_ID; Result : out G.IO_Outcome)
     with Import, Global => null, Always_Terminates;
   procedure Invoke
     (Source : Source_ID; Budget : Natural; Result : out G.Handler_Outcome;
      Charged : out Natural) with Import, Global => null, Always_Terminates;
   package Controller is new G.Driver (Source_ID, No_Source, Mask, Clear, Enable, Invoke);
   Fresh : Controller.Session;
   pragma Assert (Controller.Valid (Fresh));
end GPE_Flow_Instance;
