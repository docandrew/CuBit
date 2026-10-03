with Mesa_Service;
package body Vulkan_Device_FFI with SPARK_Mode => Off is
   Device : Mesa_Service.Owner;
   function Prepare (View : access Mesa_Service.Device_View) return System.Address
     with Import, Convention => C,
       External_Name => "cubit_vulkan_device_context_request";
   procedure Start
     (Slot : Interfaces.Unsigned_64; Owned : out Boolean;
      Description : out System.Address)
   is
      Status : Mesa_Service.Result;
      View : aliased Mesa_Service.Device_View;
      Ready : Boolean;
      use type Mesa_Service.Result;
   begin
      Description := System.Null_Address;
      Mesa_Service.Start (Device, Slot, Status);
      Owned := Mesa_Service.Accepted (Device);
      if Status = Mesa_Service.Success and then Owned then
         Mesa_Service.Borrow (Device, View, Ready);
         if Ready then Description := Prepare (View'Access); end if;
      end if;
   end Start;
   procedure Health (Healthy : out Boolean) is
      use type Mesa_Service.Result;
   begin
      Healthy := Mesa_Service.Health (Device) = Mesa_Service.Success;
   end Health;
   procedure Close (Result : out Retirement) is
   begin
      case Mesa_Service.Close (Device, Consumers_Retired => True) is
         when Mesa_Service.Retired => Result := Retired;
         when Mesa_Service.Pending => Result := Pending;
         when Mesa_Service.Unsafe => Result := Unsafe;
      end case;
   end Close;
end Vulkan_Device_FFI;
