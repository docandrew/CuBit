with Main_Model;
with CuBit.Handles;
with CuBit.Handles.Services;
with CuBit.Handles.Startup;
-- Symbolic host bindings; not generated capability startup or native authority.
package Startup is
   use type Main_Model.Scenario;
   function Community0 return CuBit.Handles.Device_Memory is (1);
   function Community1 return CuBit.Handles.Device_Memory is (2);
   function Community4 return CuBit.Handles.Device_Memory is
     (if Main_Model.Current = Main_Model.Missing_Community then 0 else 3);
   function Community5 return CuBit.Handles.Device_Memory is (4);
   function Registration return CuBit.Handles.Notification is (1);
   function Ready return CuBit.Handles.Endpoint is (1);
   function Logstore return CuBit.Handles.Services.Logstore.Optional_Endpoint is ((Present => False));
   function Parent return CuBit.Handles.Startup.Optional_Interrupt is
      (if Main_Model.Current = Main_Model.No_Parent then (Present => False) else (True, 1));
end Startup;
