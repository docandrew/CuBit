------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  GPU canvas: a rectangle of a window that the application fills with
--  Vulkan. The widget is not a Vulkan wrapper. It hands the application the
--  process's Vulkan device and a render target sized to the widget, and
--  brings the finished frame into the window through the normal desktop
--  path. The application records and submits its own Vulkan work.
--
--  Per frame:
--     Acquire (C, T, Status);          --  Status = Acquired: render into T
--     ...  record a pass that writes T.Image (or T.View), ends with the
--          image in VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, and submit it
--          to Device (C).Queue signalling the semaphore T.Ready ...
--     Present (C, T);                  --  the widget reads the frame back
--  and in the window's event loop:
--     Poll (C, New_Frame);             --  New_Frame: repaint Area (C)
--     Paint (C, Window_Canvas);        --  inside the window's Render
--
--  T.Width/T.Height are physical pixels (the area times the window's
--  density); T.Format is VK_FORMAT_B8G8R8A8_UNORM, alpha ignored. When
--  T.Generation differs from the last one the application saw, the targets
--  were rebuilt (resize): rebuild framebuffers and other per-target objects.
--
--  Visibility and pacing: Acquire answers Hidden while the application has
--  hidden the canvas or its area is empty, and Busy while the newest frame
--  has not been painted yet. A window the desktop is not repainting (its
--  previous frame not retired) therefore stops the application rendering;
--  the desktop has no separate occlusion or minimise event today.
--
--  Device: one per process, opened by the first canvas, retired when the
--  last canvas closes (no reopen). Natively it is the application's own
--  admitted render session: the manifest declares
--  (request-render-optional read-write render). Without it (software
--  launch, a machine without an Intel GPU) Acquire answers Unavailable and
--  Paint draws a placeholder.
--
--  Frames reach the window by copy: GPU image -> host-visible buffer ->
--  the window's frame at Area. Zero-copy hand-off of the GPU image to the
--  compositor is a later step behind this same interface.
------------------------------------------------------------------------------
with GPU_Canvas_Device;
with GPU_Canvas_Slots;

package CuBit.UI.GPU_Canvas is

   --  Canvases a process may have open at once.
   Canvas_Limit : constant := 4;

   subtype Device_View is GPU_Canvas_Device.Device_View;
   subtype Extent is GPU_Canvas_Slots.Extent;
   subtype Slot_Index is GPU_Canvas_Slots.Slot_Index;
   subtype Generation_Count is GPU_Canvas_Slots.Generation_Count;

   --  Every target's VkFormat: VK_FORMAT_B8G8R8A8_UNORM, the desktop's
   --  pixel order.
   Target_Format : constant Unsigned_32 := 44;

   --  A Vulkan non-dispatchable handle (VkImage, VkImageView, VkSemaphore).
   type Vulkan_Handle is new Unsigned_64;
   Null_Handle : constant Vulkan_Handle := 0;

   --  What the application renders into.
   type Target is record
      Image, View, Ready : Vulkan_Handle := Null_Handle;
      Width, Height : Extent := 0;
      Format : Unsigned_32 := 0;          --  VkFormat
      Slot : Slot_Index := Slot_Index'First;
      Generation : Generation_Count := 0;
   end record;

   type Availability is
     (Closed,        --  not opened, or closed
      Ready,         --  Device is usable
      Unavailable,   --  no render authority or GPU: placeholder
      Failed);       --  device or storage failure: placeholder

   type Acquire_Status is (Acquired, Hidden, Busy, Unavailable);

   type Canvas_Widget is limited private;

   procedure Open (C : in out Canvas_Widget);
   function State (C : Canvas_Widget) return Availability;
   --  Plain Vulkan handles; meaningful while State (C) = Ready.
   function Device (C : Canvas_Widget) return Device_View;

   --  Layout: the canvas's logical rectangle in Parent (normally the
   --  window's canvas). The target follows its physical size.
   procedure Set_Area (C : in out Canvas_Widget; Parent : CuBit.UI.Canvas; Area : CuBit.UI.Rect);
   function Area (C : Canvas_Widget) return CuBit.UI.Rect;
   procedure Set_Visible (C : in out Canvas_Widget; Visible : Boolean);

   procedure Acquire (C : in out Canvas_Widget; T : out Target; Status : out Acquire_Status);
   --  T came from Acquire and its work, signalling T.Ready, was submitted.
   procedure Present (C : in out Canvas_Widget; T : Target);
   --  T came from Acquire and nothing signalling T.Ready was submitted.
   procedure Cancel (C : in out Canvas_Widget; T : Target);

   --  Retire finished readbacks. New_Frame: a newer frame is ready to paint.
   procedure Poll (C : in out Canvas_Widget; New_Frame : out Boolean);
   --  Draw the newest frame (or the placeholder) into Area of Into.
   procedure Paint (C : in out Canvas_Widget; Into : CuBit.UI.Canvas);

   procedure Close (C : in out Canvas_Widget);

private
   subtype Canvas_Number is Natural range 0 .. Canvas_Limit - 1;
   type Slot_Targets is array (Slot_Index) of Target;

   type Canvas_Widget is limited record
      Opened : Boolean := False;
      Number : Canvas_Number := 0;
      Status : Availability := Closed;
      Slots : GPU_Canvas_Slots.State;
      Targets : Slot_Targets;
      Logical : CuBit.UI.Rect := (others => 0);
   end record;
end CuBit.UI.GPU_Canvas;
