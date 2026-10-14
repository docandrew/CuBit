with CuBit.Handles.Startup;

package body UI_Desktop_Service is
   Found : constant CuBit.Handles.Startup.Optional_Endpoint :=
     CuBit.Handles.Startup.Optional_Endpoint_Named (Binding);
   Desktop : constant CuBit.Handles.Endpoint :=
     (if Found.Present then Found.Value else CuBit.Handles.No_Endpoint);

   function Endpoint return CuBit.Handles.Endpoint is (Desktop);
end UI_Desktop_Service;
