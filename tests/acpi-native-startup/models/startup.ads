with CuBit.Handles;
-- Required startup-table delivery is modeled, not authenticated here.
package Startup is
   function Registration return CuBit.Handles.Notification is
     (CuBit.Handles.No_Notification);
end Startup;
