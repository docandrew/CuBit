pragma Ada_2022;
package body ACPI_Requests.With_Owner_Cleanup with SPARK_Mode is
   procedure Advance (Server : in out State; Completed : out Boolean) is
   begin
      Controller.Advance(Server.Boot,Completed);
   end Advance;
end ACPI_Requests.With_Owner_Cleanup;
