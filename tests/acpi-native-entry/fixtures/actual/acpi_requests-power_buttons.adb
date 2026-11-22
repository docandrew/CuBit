pragma Ada_2022;
package body ACPI_Requests.Power_Buttons with SPARK_Mode is
   procedure Discover (Server : in out State; Item : in out Run; Budget : Natural;
      Deadline : Interfaces.Unsigned_64; Result : out Report) is
   begin
      Result := (others => <>);if Notifications_Pending (Server) then return;end if;
      Bridge.Discover (Server.Boot, Item, Budget, Deadline, Result);
   end Discover;
   function Available (Server : State; Item : Run) return Boolean is (Bridge.Available (Server.Boot, Item));
   procedure Route (Server : State; Item : in out Run; Accepted : out Boolean) is
   begin
      if Receipts.Current (Server.Receipt) = 0 then Accepted := True;return;end if;
      Bridge.Observe_Retained (Server.Boot, Item,
        Bridge.Bindings.Receipt_Number (Receipts.Current (Server.Receipt)), Server.Staged_Notification, Accepted);
   end Route;
end ACPI_Requests.Power_Buttons;
