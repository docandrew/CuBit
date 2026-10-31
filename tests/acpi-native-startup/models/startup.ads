with CuBit.Handles;
with CuBit.Handles.Startup;
-- Required startup-table delivery is modeled, not authenticated here.
package Startup is
   function Registers return CuBit.Handles.Startup.Optional_Endpoint is
     ((Present => False));
   function Registration return CuBit.Handles.Notification is
     (CuBit.Handles.No_Notification);
end Startup;
