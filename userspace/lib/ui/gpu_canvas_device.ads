------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The process's Vulkan device for GPU canvases (CuBit.UI.GPU_Canvas). One
--  device per process, opened by the first canvas and retired when the last
--  canvas closes; a process cannot reopen it (the native render session is
--  one attempt per process).
--
--  Two bodies. Native (gpu-native/): the application's own admitted render
--  session, through Mesa_Service; the manifest declares
--  (request-render-optional read-write render), and a launch without that
--  entry (software start, no Intel GPU) reports Unavailable. Hosted
--  (tests/gpu-canvas/host/): the Linux Vulkan loader.
------------------------------------------------------------------------------
with Interfaces;
with System;

package GPU_Canvas_Device is

   --  Same-process borrowed Vulkan handles, never capabilities: VkInstance,
   --  VkPhysicalDevice, VkDevice, VkQueue, the queue family and
   --  vkGetInstanceProcAddr. Layout of struct gpu_canvas_device.
   type Device_View is record
      Instance, Physical, Device, Queue : System.Address := System.Null_Address;
      Family : Interfaces.Unsigned_32 := 0;
      Instance_Proc : System.Address := System.Null_Address;
   end record with Convention => C;

   type Device_Status is
     (Not_Opened,   --  no canvas asked yet
      Ready,        --  View is usable
      Unavailable,  --  no render authority or no GPU: draw a placeholder
      Failed,       --  the device failed; no retry in this process
      Retired);     --  closed; no reopen in this process

   --  First call opens; later calls return the same view and status.
   procedure Open (View : out Device_View; Status : out Device_Status);
   function Status return Device_Status;
   --  Every consumer (canvas storage, application objects, submitted work)
   --  has retired. Waits a bounded time for the session to retire.
   procedure Close;

end GPU_Canvas_Device;
