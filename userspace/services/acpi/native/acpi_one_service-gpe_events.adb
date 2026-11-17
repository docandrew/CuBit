pragma Ada_2022;
package body ACPI_One_Service.GPE_Events is
   procedure Deliver_Selected (Service : aliased in out ACPI_Service.State;
      Item : Methods.Binding; Budget : Natural; Completed : out Boolean) is
      Result : Delivery.Delivery_Result;
      use type Delivery.Delivery_Result;
   begin
      Delivery.Deliver (Service, Item, Budget, Result);
      Completed := Result = Delivery.Delivered;
   end Deliver_Selected;
   procedure Run_Delivery is new Requests.Deliver_Local (Deliver_Selected);
   procedure Run_Close is new Requests.Close_Local (Delivery.Close_Result);
   procedure Dispatch (Server : in out ACPI_Requests.State; Item : Methods.Binding;
      Budget : Natural; Allowed : out Requests.Controller.Admission; Completed : out Boolean) is
   begin Run_Delivery (Server, Item, Budget, Allowed, Completed); end Dispatch;
   procedure Close_Result (Server : in out ACPI_Requests.State; Action : out Methods.Action_Result) is
   begin Run_Close (Server, Action); end Close_Result;
end ACPI_One_Service.GPE_Events;
