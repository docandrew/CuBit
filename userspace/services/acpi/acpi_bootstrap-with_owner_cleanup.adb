pragma Ada_2022;
package body ACPI_Bootstrap.With_Owner_Cleanup with SPARK_Mode is
   procedure Advance (Session : in out State; Completed : out Boolean) is
   begin
      Step(Session.Core,Completed);
   end Advance;
end ACPI_Bootstrap.With_Owner_Cleanup;
