with Servo_Tab_Projection;
with Interfaces; use Interfaces;
with System;
-- Serialized main-thread ABI. All incoming byte ranges remain borrowed only
-- during the call. No writable frame pointer or grant escapes into Servo.
-- GNAT's servo_shell_hostinit must be called once before this interface.
package Servo_Shell with SPARK_Mode => Off is
   type Viewport is record
      Width, Height, Numerator, Denominator : Unsigned_32 := 0;
   end record with Convention => C;
   type Event is record
      Kind, A, B : Unsigned_64 := 0;
   end record with Convention => C;
   -- Fixed-width C snapshot; Boolean representation never crosses the ABI.
   type Input_Stats is record
      Batch_Enabled, Channel_Disabled : Unsigned_64 := 0;
      Successful_Fetches, Fetched_Events, Delivered_Events : Unsigned_64 := 0;
      Fallback_Polls, Cache_Rejections : Unsigned_64 := 0;
   end record with Convention => C;
   procedure Input_Statistics (Result : access Input_Stats)
     with Export, Convention => C,
       External_Name => "cubit_servo_input_statistics";
   -- Event kinds 1..7 are page input. 16 navigate, 17 back, 18 forward,
   -- 19 reload, 20 configure/resync, 21 close, 22 consumed chrome input,
   -- 23 pointer left page viewport; 24 new tab, 25 select, 26 close tab, 27 new window.
   -- 28 Settings opened (also releases held page input).
   -- Tab IDs are stable u64 values owned by Rust. 24 requests creation;
   -- 25/26 carry the logical ID in A; 29 cycles (A=1 backward, A=0 forward).
   function Select_Window (ID : Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_servo_select_window";
   -- Opt-in disposable-image capability oracle; zero denotes success.
   function Config_Scope_Check return Unsigned_32
     with Export, Convention => C,
       External_Name => "cubit_servo_config_scope_check";
   function Memory_Owned return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_servo_memory_owned";
   function Open return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_servo_open";
   procedure Metrics (Result : access Viewport)
     with Export, Convention => C, External_Name => "cubit_servo_metrics";
   procedure Begin_Input
     with Export, Convention => C, External_Name => "cubit_servo_begin_input";
   function Poll (Result : access Event) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_servo_poll";
   function Location (Text : System.Address; Capacity : Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_servo_location";
   procedure State
     (URL : System.Address; URL_Length : Unsigned_32;
      Title : System.Address; Title_Length : Unsigned_32;
      Flags : Unsigned_32)
     with Export, Convention => C, External_Name => "cubit_servo_state";
   procedure Security (Text : System.Address; Length : Unsigned_32) with Export, Convention => C, External_Name => "cubit_servo_security";
   function Tab_Capacity return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_servo_tab_capacity";
   function Update_Tabs (Value : access constant Servo_Tab_Projection.Snapshot) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_servo_tabs";
   procedure Navigation_Error
     with Export, Convention => C, External_Name => "cubit_servo_navigation_error";
   -- Acquire before SWGL readback. 0 defers without copying; 1 reserves a
   -- synchronous hidden destination lease. Exactly one Present or Cancel
   -- follows, before any other shell mutation. No address crosses the ABI.
   function Prepare return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_servo_prepare";
   procedure Cancel
     with Export, Convention => C, External_Name => "cubit_servo_cancel";
   -- 0 deferred/closed, 1 published, 2 configuration changed: resize/repaint.
   -- Borrowed bottom-up BGRA rows are consumed synchronously, never retained.
   function Present
     (BGRA : System.Address; Length : Unsigned_64;
      Width, Height, Source_Pitch : Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_servo_present";
   function Pending return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_servo_pending";
   procedure Window_Error
     with Export, Convention => C, External_Name => "cubit_servo_window_error";
   procedure Close
     with Export, Convention => C, External_Name => "cubit_servo_close";
end Servo_Shell;
