with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Control;

package body Startup is
   Cached : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;

   function Device_Manager return CuBit.Handles.Endpoint is
   begin
      if not CuBit.Handles.Valid (Cached) then
         Cached := CuBit.Handles.Control.Derive
           (CuBit.Handles.Badged (CuBit.Handles.Boot_Roots.Get.Self_Grantable,
                                  CuBit.Handles.Read_Write, Driver_Badge));
      end if;
      return Cached;
   end Device_Manager;
end Startup;
