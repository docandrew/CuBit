-- Hosted boundary substitution; the owner itself is unchanged.
package body Vulkan_Device_FFI with SPARK_Mode => Off is
   procedure Native_Start (Slot : Interfaces.Unsigned_64;
     Owned : out Interfaces.Unsigned_32; Description : out System.Address)
     with Import, Convention => C, External_Name => "device_mock_start";
   function Native_Close return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "device_mock_close";
   procedure Start (Slot : Interfaces.Unsigned_64; Owned : out Boolean;
                    Description : out System.Address) is
      Value : Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_32;
   begin Native_Start (Slot, Value, Description); Owned := Value /= 0; end Start;
   function Native_Health return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "device_mock_health";
   procedure Health (Healthy : out Boolean) is
      use type Interfaces.Unsigned_32;
   begin Healthy := Native_Health = 0; end Health;
   procedure Close (Result : out Retirement) is
   begin
      case Native_Close is
         when 0 => Result := Retired;
         when 1 => Result := Pending;
         when others => Result := Unsafe;
      end case;
   end Close;
end Vulkan_Device_FFI;
