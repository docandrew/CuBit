with System;
with Interfaces.C;
package Desktop_Backdrop_Real_Bridge is
   procedure Reference (Target : System.Address) with Export, Convention => C, External_Name => "desktop_backdrop_reference";
   function Held_Front return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_backdrop_real_front";
   function Held_Pending return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_backdrop_real_pending";
   function Open return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_backdrop_real_open";
   function Start (Version : Interfaces.C.int) return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_backdrop_real_start";
   function Import_Image return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_backdrop_real_import";
   function Render return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_backdrop_real_render";
   function Poll_Upload return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_backdrop_real_poll_upload";
   function Poll_Frame return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_backdrop_real_poll_frame";
   function Close return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_backdrop_real_close";
end Desktop_Backdrop_Real_Bridge;
