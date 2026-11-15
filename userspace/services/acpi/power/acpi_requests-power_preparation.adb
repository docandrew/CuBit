pragma Ada_2022;
package body ACPI_Requests.Power_Preparation with SPARK_Mode is
   function Current_Types (Server : State; Item : Run) return Driver.Copied_Types is
     (Bridge.Current_Types (Server.Boot, Item));
   procedure Prepare (Server : in out State; Item : in out Run;
      Registers : Objects.Register_Model; Budget : Natural;
      Deadline_Microseconds : Interfaces.Unsigned_64; Result : out Report) is
   begin
      if Notifications_Pending (Server) then
         Invalidate (Item); Result := (others => <>); return;
      end if;
      Bridge.Prepare (Server.Boot, Item, Registers, Budget, Deadline_Microseconds, Result);
   end Prepare;
end ACPI_Requests.Power_Preparation;
