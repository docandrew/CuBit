with Main_Model;
with CuBit.Handles;
with CuBit.Handles.Startup;
with CuBit.Handles.Services;
-- Symbolic host bindings; not generated capability startup or native authority.
package Startup is
   use type Main_Model.Scenario;
   function Community0 return CuBit.Handles.Startup.Optional_Device_Memory is ((True, 1));
   function Community1 return CuBit.Handles.Startup.Optional_Device_Memory is ((True, 2));
   function Community4 return CuBit.Handles.Startup.Optional_Device_Memory is
     (if Main_Model.Current = Main_Model.Missing_Community then (Present => False) else (True, 3));
   function Community5 return CuBit.Handles.Startup.Optional_Device_Memory is ((True, 4));
   function Launcher return CuBit.Handles.Process is (1);
   function Registration return CuBit.Handles.Notification is (1);
   function Ready return CuBit.Handles.Endpoint is (1);
   function Logstore return CuBit.Handles.Services.Logstore.Optional_Endpoint is ((Present => False));
end Startup;
