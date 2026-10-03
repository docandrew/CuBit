------------------------------------------------------------------------------
--  The C-compatible window boundary of CCL desktop applications
------------------------------------------------------------------------------

--  Imported once, for every application. The selected platform body
--  (CCL_Desktop_Platform's native adapter, or native_window.c on Linux)
--  exports these symbols.
with System;
with Interfaces;
package CCL_Window is
   function Open (Width, Height : Interfaces.Integer_32) return System.Address
   with Import, Convention => C, External_Name => "ccl_window_open";
   --  The window's title (Length bytes of Text): console.title.
   procedure Set_Title
     (Handle : System.Address; Text : System.Address; Length : Interfaces.Integer_32)
   with Import, Convention => C, External_Name => "ccl_window_retitle";
   function Has_System_Chrome return Interfaces.Integer_32
   with Import, Convention => C, External_Name => "ccl_window_has_system_chrome";
   function Poll
     (Handle : System.Address; Kind : access Interfaces.Integer_32;
      Code, Modifiers : access Interfaces.Unsigned_32;
      X, Y : access Interfaces.Integer_32) return Interfaces.Integer_32
   with Import, Convention => C, External_Name => "ccl_window_poll";
   function Prepare_Frame
     (Handle : System.Address;
      Minimum_Width, Minimum_Height : Interfaces.Integer_32;
      Maximum_Width, Maximum_Height : Interfaces.Integer_32;
      Width, Height : access Interfaces.Integer_32) return Interfaces.Integer_32
   with Import, Convention => C, External_Name => "ccl_window_prepare_frame";
   procedure Set_Cursor (Handle : System.Address; Style : Interfaces.Integer_32)
   with Import, Convention => C, External_Name => "ccl_window_set_cursor";
   procedure Wait (May_Block : Interfaces.Integer_32)
   with Import, Convention => C, External_Name => "ccl_window_wait";
   procedure Wait_Until (Deadline : Interfaces.Unsigned_64)
   with Import, Convention => C, External_Name => "ccl_window_wait_until";
   function Ticks return Interfaces.Unsigned_64
   with Import, Convention => C, External_Name => "ccl_window_ticks";
   function Clock_Monotonic
     (Success : access Interfaces.Integer_32) return Interfaces.Unsigned_64
   with Import, Convention => C, External_Name => "ccl_window_clock_monotonic";
   procedure Close (Handle : System.Address)
   with Import, Convention => C, External_Name => "ccl_window_close";
end CCL_Window;
