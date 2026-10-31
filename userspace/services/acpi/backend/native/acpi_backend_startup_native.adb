pragma Ada_2022;
package body ACPI_Backend_Startup_Native is
   procedure Initialize
     (Run : in out Owner.Context; Hardware : in out Catalog.Context;
      Input : aliased in CuBit.ACPI_Windows.Grant;
      FADT : ACPI_FADT.Description; Deadline : Owner.B.Tick; Result : out Owner.Report)
   is
      Saved : aliased Owner.Grant;
   begin
      -- No whole-grant assignment: unused runtime slots need not be valid
      -- initialized scalar objects. The private grant has safe defaults.
      Saved.IO_Used := Input.IO_Used; Saved.Memory_Used := Input.Memory_Used;
      for I in 1 .. Saved.IO_Used loop
         Saved.IO (I) := (Input.IO (I).Ports, Input.IO (I).First, Input.IO (I).Count);
      end loop;
      for I in 1 .. Saved.Memory_Used loop
         Saved.Memory (I) := (Input.Memory (I).Memory, Input.Memory (I).First_Page, Input.Memory (I).Pages);
      end loop;
      Owner.Initialize (Run, Hardware, Saved, FADT, Deadline, Result);
   end Initialize;
end ACPI_Backend_Startup_Native;
