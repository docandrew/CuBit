------------------------------------------------------------------------------
--  Linux-hosted body (tests/gpu-canvas): the system Vulkan loader. A test
--  can force Unavailable to exercise the placeholder path.
------------------------------------------------------------------------------
with GPU_Canvas_Host_Control;

package body GPU_Canvas_Device is

   Current : Device_Status := Not_Opened;
   Cached : aliased Device_View;

   function Host_Open (View : access Device_View) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "gpu_canvas_host_open";
   procedure Host_Close
     with Import, Convention => C, External_Name => "gpu_canvas_host_close";

   procedure Open (View : out Device_View; Status : out Device_Status) is
      use type Interfaces.Unsigned_32;
   begin
      if Current = Not_Opened then
         if GPU_Canvas_Host_Control.Force_Unavailable then
            Current := Unavailable;
         elsif Host_Open (Cached'Access) = 0 then
            Current := Ready;
         else
            Current := Failed;
         end if;
      end if;
      View := (if Current = Ready then Cached else (others => <>));
      Status := Current;
   end Open;

   function Status return Device_Status is (Current);

   procedure Close is
   begin
      if Current = Ready then
         Host_Close;
      end if;
      if Current /= Not_Opened then
         Current := Retired;
      end if;
   end Close;


end GPU_Canvas_Device;
