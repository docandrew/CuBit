with Interfaces; use Interfaces;
with System;
with Servo_Shell; use Servo_Shell;
generic
package Servo_Session is
   function Open return Unsigned_32;
   procedure Metrics (Result : access Viewport);
   procedure Begin_Input;
   function Poll (Result : access Event) return Unsigned_32;
   function Location (Text : System.Address; Capacity : Unsigned_32) return Unsigned_32;
   procedure State
     (URL : System.Address; URL_Length : Unsigned_32;
      Title : System.Address; Title_Length : Unsigned_32;
      Flags : Unsigned_32);
   procedure Tab_Title (Index : Unsigned_32; Text : System.Address; Length : Unsigned_32);
   procedure Tab_Parked (Index : Unsigned_32);
   procedure Navigation_Error;
   -- Acquire before SWGL readback. 0 defers without copying; 1 reserves a
   -- synchronous hidden destination lease. Exactly one Present or Cancel
   -- follows, before any other shell mutation. No address crosses the ABI.
   function Prepare return Unsigned_32;
   procedure Cancel;
   -- 0 deferred/closed, 1 published, 2 configuration changed: resize/repaint.
   -- Full page readback is consumed synchronously, never retained.
   function Present
     (RGBA : System.Address; Length : Unsigned_64;
      Width, Height : Unsigned_32) return Unsigned_32;
   function Pending return Unsigned_32;
   procedure Window_Error;
   procedure Close;
   function Is_Open return Boolean;
end Servo_Session;
