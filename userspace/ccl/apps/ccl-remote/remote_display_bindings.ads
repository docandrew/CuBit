------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The display interface (CCL.Interfaces.Display) in the remote session:
--  (display.capture) and (display.capture-area (Capture_Area ...)) return
--  the frame as an Image (CCL.Image_Store's large images), at the output's
--  native pixels. Installed only when the startup entry granted
--  display-capture (Remote_Capture.Available); every call is an activity
--  entry and a "ccl-remote: host capture ..." line, taken or refused.
------------------------------------------------------------------------------
with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

package Remote_Display_Bindings is
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   --  Publish the interface and grant its operations (after the image
   --  interface). Success is False only when publishing failed.
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end Remote_Display_Bindings;
