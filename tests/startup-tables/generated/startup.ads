--  Generated with the ELF manifest; do not edit.
--  Startup capabilities of com.cubit.test.startup, by name (CAP-002).
--  Handle limit: at most 512.
pragma Ada_2022;
with CuBit.Handles.Startup;
with CuBit.Startup_State;
with CuBit.Startup_Tables;
with CuBit.Channel_Limits;
package Startup with SPARK_Mode is

   --  Device_Memory "registers", required.
   function Registers return Standard.CuBit.Handles.Device_Memory is
     (Standard.CuBit.Handles.Startup.Device_Memory_Named ("registers", Standard.CuBit.Startup_Tables.Device_Memory));

   --  Interrupt "device", required.
   function Device return Standard.CuBit.Handles.Interrupt is
     (Standard.CuBit.Handles.Startup.Interrupt_Named ("device", Standard.CuBit.Startup_Tables.Interrupt));

   --  Endpoint "devmgr", required.
   function Devmgr return Standard.CuBit.Handles.Endpoint is
     (Standard.CuBit.Handles.Startup.Endpoint_Named ("devmgr", Standard.CuBit.Startup_Tables.Endpoint));

   --  Notification "pointer-events", optional.
   function Pointer_Events return Standard.CuBit.Handles.Startup.Optional_Notification is
     (Standard.CuBit.Handles.Startup.Optional_Notification_Named ("pointer-events", Standard.CuBit.Startup_Tables.Notification));

   --  Scheduling "real-time", required.
   function Real_Time return Standard.CuBit.Handles.Scheduling is
     (Standard.CuBit.Handles.Startup.Scheduling_Named ("real-time", Standard.CuBit.Startup_Tables.Scheduling));

   --  Whether the loaded startup table holds every capability above,
   --  with its kind; call once at startup and stop if it is not Ready.
   function Verify return Standard.CuBit.Startup_State.Verification is
     (Standard.CuBit.Startup_State.Verify
        ([(Standard.CuBit.Startup_Tables.To_Name ("registers"), Standard.CuBit.Startup_Tables.Device_Memory, Standard.CuBit.Startup_Tables.Required),
          (Standard.CuBit.Startup_Tables.To_Name ("device"), Standard.CuBit.Startup_Tables.Interrupt, Standard.CuBit.Startup_Tables.Required),
          (Standard.CuBit.Startup_Tables.To_Name ("devmgr"), Standard.CuBit.Startup_Tables.Endpoint, Standard.CuBit.Startup_Tables.Required),
          (Standard.CuBit.Startup_Tables.To_Name ("pointer-events"), Standard.CuBit.Startup_Tables.Notification, Standard.CuBit.Startup_Tables.Optional),
          (Standard.CuBit.Startup_Tables.To_Name ("real-time"), Standard.CuBit.Startup_Tables.Scheduling, Standard.CuBit.Startup_Tables.Required)]));

   package Channels is
      --  "gpu.display"
      Gpu_Display_Per_Client : constant Standard.CuBit.Channel_Limits.Channel_Limit :=
        (Kind => Standard.CuBit.Channel_Limits.At_Most, Count => 1);
      Gpu_Display_Total : constant Standard.CuBit.Channel_Limits.Channel_Limit :=
        (Kind => Standard.CuBit.Channel_Limits.At_Most, Count => 1);
      Gpu_Display_Class : constant Standard.CuBit.Channel_Limits.Channel_Class :=
        Standard.CuBit.Channel_Limits.Urgent;
      --  "gpu.command"
      Gpu_Command_Per_Client : constant Standard.CuBit.Channel_Limits.Channel_Limit :=
        (Kind => Standard.CuBit.Channel_Limits.At_Most, Count => 1);
      Gpu_Command_Total : constant Standard.CuBit.Channel_Limits.Channel_Limit :=
        (Kind => Standard.CuBit.Channel_Limits.At_Most, Count => 1);
      Gpu_Command_Class : constant Standard.CuBit.Channel_Limits.Channel_Class :=
        Standard.CuBit.Channel_Limits.Interactive;
      --  "gpu.render"
      Gpu_Render_Per_Client : constant Standard.CuBit.Channel_Limits.Channel_Limit :=
        (Kind => Standard.CuBit.Channel_Limits.Server_Limit);
      Gpu_Render_Total : constant Standard.CuBit.Channel_Limits.Channel_Limit :=
        (Kind => Standard.CuBit.Channel_Limits.Server_Limit);
      Gpu_Render_Class : constant Standard.CuBit.Channel_Limits.Channel_Class :=
        Standard.CuBit.Channel_Limits.Normal;
   end Channels;
end Startup;
