with GPIO_Native_Driver;
with GPIO_Service_Pump;
generic
   with package Driver is new GPIO_Native_Driver (<>);
   Events_Per_Turn, Requests_Per_Turn, Steps_Per_Turn : Positive;
   Setup_Window, Cleanup_Window : Driver.Controller.I.Tick;
   with procedure Disable_Logging (Reason : String);
package GPIO_Native_Pump is
   procedure Poll_Event (S : in out Driver.Admin.State; Found : out Boolean);
   procedure Poll_Request (S : in out Driver.Admin.State; Found : out Boolean);
   package Pump is new GPIO_Service_Pump
     (Driver.Admin, Events_Per_Turn, Requests_Per_Turn, Steps_Per_Turn, Poll_Event, Poll_Request);
end GPIO_Native_Pump;
