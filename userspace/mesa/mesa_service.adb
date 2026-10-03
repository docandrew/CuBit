package body Mesa_Service is
   use type System.Address;
   use type Interfaces.Unsigned_32;
   function C_Start (Slot : Interfaces.Unsigned_64; Handle : access System.Address)
                     return Result
     with Import, Convention => C, External_Name => "cubit_mesa_service_start";
   function C_Borrow (Handle : System.Address; View : access Device_View)
                      return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_mesa_service_device";
   function C_Health (Handle : System.Address) return Result
     with Import, Convention => C, External_Name => "cubit_mesa_service_status";
   function C_Close (Handle : System.Address) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_mesa_service_close";

   procedure Start (Object : in out Owner; Slot : Interfaces.Unsigned_64;
                    Status : out Result) is
      Handle : aliased System.Address := System.Null_Address;
   begin
      Status := Initialization_Failed;
      if Object.Attempted then
         return;
      end if;
      Object.Attempted := True;
      Status := C_Start (Slot, Handle'Access);
      Object.Handle := Handle; --  Retain even on a failing VkResult.
   end Start;

   function Accepted (Object : Owner) return Boolean is
     (Object.Handle /= System.Null_Address);

   procedure Borrow (Object : Owner; View : out Device_View; Ready : out Boolean) is
      Temporary : aliased Device_View;
   begin
      View := (others => <>);
      Ready := False;
      if not Accepted (Object) or else Object.Closing then
         return;
      end if;
      if C_Borrow (Object.Handle, Temporary'Access) /= 0 then
         View := Temporary;
         Ready := True;
      end if;
   end Borrow;

   function Health (Object : Owner) return Result is
   begin
      if not Accepted (Object) or else Object.Closing then
         return Initialization_Failed;
      end if;
      return C_Health (Object.Handle);
   end Health;

   function Close (Object : in out Owner; Consumers_Retired : Boolean)
                   return Retirement is
      Code : Interfaces.Unsigned_32;
   begin
      if not Accepted (Object) then
         return Unsafe;
      elsif not Consumers_Retired then
         return Pending;
      end if;
      Object.Closing := True; --  No new borrows, even after uncertain teardown.
      Code := C_Close (Object.Handle);
      case Code is
         when 0 => return Retired;
         when 1 => return Pending;
         when others => return Unsafe;
      end case;
   end Close;
end Mesa_Service;
