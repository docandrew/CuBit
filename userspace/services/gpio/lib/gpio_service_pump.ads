pragma Ada_2022;
with GPIO_Admin_Owner;
generic
   with package Admin is new GPIO_Admin_Owner (<>);
   Events_Per_Turn : Positive;
   Requests_Per_Turn : Positive;
   Steps_Per_Turn : Positive;
   with procedure Poll_Event (S : in out Admin.State; Found : out Boolean);
   with procedure Poll_Request (S : in out Admin.State; Found : out Boolean);
package GPIO_Service_Pump is
   type Turn_Report is record
      Events, Requests, Steps : Natural := 0;
      Progress_Needed, Terminal, Quiescent : Boolean := False;
   end record;
   procedure Turn (S : in out Admin.State; Result : out Turn_Report);
   -- One bounded turn: events, requests, then controller/admin progress.
   -- Separate bounds keep a flooded lane from starving another lane or MMIO.
   -- Poll callbacks consume at most one authenticated native queue entry, never
   -- wait, and never step the controller. Registry.Step is the sole advancer.
   -- No sleep/clock fallback is hidden here. Caller waits on activity when idle
   -- and uses a finite progress wake when Progress_Needed. Terminal alone never
   -- permits resource release: Quiescent must separately confirm all ownership.
end GPIO_Service_Pump;
