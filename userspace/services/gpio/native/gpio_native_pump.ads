pragma Ada_2022;
with Interfaces;
with GPIO_Native_Driver;
with GPIO_Service_Pump;
generic
   with package Driver is new GPIO_Native_Driver (<>);
   Events_Per_Turn, Requests_Per_Turn, Steps_Per_Turn : Positive;
   Setup_Window, Cleanup_Window : Driver.Controller.I.Tick;
   with procedure Disable_Logging (Reason : String);
package GPIO_Native_Pump is
   type Statistics is record
      Events, Ignored_Events, Requests : Interfaces.Unsigned_64 := 0;
   end record;
   function Observe return Statistics;
   procedure Configure_Parent (Present : Boolean; Accepted : out Boolean);
   -- Trusted Main only, after no-parent configuration and registry sealing.
   -- This never fabricates parent IRQ presence or broadens pin admission.
   procedure Enable_Polling (Accepted : out Boolean);
   function Events_Available return Boolean;
   procedure Take_Parent_Notice (Pending : out Boolean);
   procedure Hold_Requests (Held : Boolean);
   function Requests_Held return Boolean;
   procedure Poll_Event (S : in out Driver.Admin.State; Found : out Boolean);
   procedure Poll_Request (S : in out Driver.Admin.State; Found : out Boolean);
   package Pump is new GPIO_Service_Pump
     (Driver.Admin, Events_Per_Turn, Requests_Per_Turn, Steps_Per_Turn, Poll_Event, Poll_Request);
   -- Pump.Turn is the native one-turn entry. Only typed service inbox entries
   -- reach client/admin decoders; only kernel event metadata authorizes stop,
   -- logging teardown and channel wakes. No peer calls, sleep or IRQ enabling.
   -- Main supplies explicit finite windows, waits on activity while idle and
   -- sets a finite progress wake while Pump reports Progress_Needed.
   -- Setup/cleanup deadlines are sampled once at acceptance, never extended by
   -- later Turns. Clock refusal yields an expired deadline, no guessed time.
end GPIO_Native_Pump;
