------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's pointer as a display plane request (docs/display-planes.md).
--
--  The desktop owns one request, the primary pointer. When the display's
--  newest plan puts it on a hardware plane the desktop stops compositing it:
--  no overlay, no save/restore, and motion is a coalesced asynchronous Move
--  instead of a frame. Frames are tagged with the plan epoch whose cursor
--  composition they show, so display swaps plane and composite in one frame.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with System;
with CuBit.Messages;
with CuBit.Display_Planes;

package Desktop_Pointer_Plane is
   use type Interfaces.Unsigned_64;
   package DPL renames CuBit.Display_Planes;

   --  After output zero's display lease: query planes, create the pointer.
   --  Without cursor planes the pointer simply stays composited.
   procedure Start;
   function Active return Boolean;

   --  Desktop space: output Output's top-left in pointer coordinates. Only
   --  unscaled, unrotated outputs can take a hardware pointer: Exact = False
   --  keeps every pointer composited.
   procedure Place (Output : Natural; X, Y : Integer; Exact : Boolean);

   --  Premultiplied ARGB, Width * 4 pitch, read during this call. Submitted
   --  asynchronously like Move (the image carries its hotspot): one shape
   --  request in flight, and a newer shape replaces one not yet sent, so the
   --  event loop never waits for display.
   procedure Set_Shape
     (Pixels : System.Address; Width, Height, Hot_X, Hot_Y : Natural;
      Sequence : in out Interfaces.Unsigned_64);

   --  Newest pointer position; sent at once or after the move in flight.
   procedure Move
     (X, Y : Integer; Sequence : in out Interfaces.Unsigned_64);

   --  The newest plan puts the pointer on a hardware plane.
   function Hardware return Boolean;
   --  Called by the pointer present, which restores and draws the pointer
   --  footprints into the frame damage: from here on frames composite the
   --  pointer exactly when Composed_Hardware is False.
   procedure Note_Composed;
   function Composed_Hardware return Boolean;
   --  Plan epoch to tag frames with (No_Epoch before any plan).
   function Frame_Epoch return DPL.Plan_Epoch;
   --  Hardware changed since the last call: the pointer area needs a frame.
   function Take_Change return Boolean;

   function Token return Interfaces.Unsigned_64;
   function Shape_Token return Interfaces.Unsigned_64;
   function Matches (Completion_Token : Interfaces.Unsigned_64) return Boolean is
     (Completion_Token /= 0 and then
      (Completion_Token = Token or else Completion_Token = Shape_Token));
   procedure Collect (Completion : CuBit.Messages.CompletionEntry;
                      Sequence : in out Interfaces.Unsigned_64);
end Desktop_Pointer_Plane;
