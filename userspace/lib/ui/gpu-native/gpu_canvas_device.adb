------------------------------------------------------------------------------
--  Native body: the application's admitted render session (manifest entry
--  "render", optional) through the Mesa_Service bridge.
------------------------------------------------------------------------------
with Interfaces.C;
with CuBit.Handles;
with Mesa_Service;
with Startup;

package body GPU_Canvas_Device is

   use type Mesa_Service.Result, Mesa_Service.Retirement;

   --  Bounded wait for the session to retire after close: Pending passes,
   --  each after a short sleep.
   Retire_Poll_Limit : constant := 50;
   Retire_Poll_Microseconds : constant := 20_000;

   Owner : Mesa_Service.Owner;
   Current : Device_Status := Not_Opened;
   Cached : Device_View;

   function Sleep_Microseconds (Duration : Interfaces.C.unsigned) return Interfaces.C.int
     with Import, Convention => C, External_Name => "usleep";

   procedure Open (View : out Device_View; Status : out Device_Status) is
      Result : Mesa_Service.Result;
      Borrowed : Mesa_Service.Device_View;
      Ready_Now : Boolean;
   begin
      if Current = Not_Opened then
         if not Startup.Render.Present then
            Current := Unavailable;
         else
            Mesa_Service.Start (Owner, Startup.Render.Value, Result);
            if Result = Mesa_Service.Success then
               Mesa_Service.Borrow (Owner, Borrowed, Ready_Now);
               if Ready_Now then
                  Cached := (Instance => Borrowed.Instance, Physical => Borrowed.Physical,
                             Device => Borrowed.Device, Queue => Borrowed.Queue,
                             Family => Borrowed.Family, Instance_Proc => Borrowed.Instance_Proc);
                  Current := Ready;
               else
                  Current := Failed;
               end if;
            else
               Current := Failed;
            end if;
         end if;
      end if;
      View := (if Current = Ready then Cached else (others => <>));
      Status := Current;
   end Open;

   function Status return Device_Status is (Current);

   procedure Close is
      State : Mesa_Service.Retirement;
      Ignored : Interfaces.C.int;
   begin
      if Current in Ready | Failed and then Mesa_Service.Accepted (Owner) then
         State := Mesa_Service.Close (Owner, Consumers_Retired => True);
         for Attempt in 1 .. Retire_Poll_Limit loop
            exit when State /= Mesa_Service.Pending;
            Ignored := Sleep_Microseconds (Retire_Poll_Microseconds);
            State := Mesa_Service.Close (Owner, Consumers_Retired => True);
         end loop;
      end if;
      if Current /= Not_Opened then
         Current := Retired;
      end if;
      Cached := (others => <>);
   end Close;

end GPU_Canvas_Device;
