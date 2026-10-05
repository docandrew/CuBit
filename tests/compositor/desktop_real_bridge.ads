with Interfaces; with Interfaces.C; with System;
package Desktop_Real_Bridge is
   function Open return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_real_open";
   function Begin_Chunk (First, Rows : access Interfaces.Unsigned_32) return System.Address
     with Export, Convention => C, External_Name => "desktop_real_begin";
   function Submit return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_real_submit";
   function Poll_Upload return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_real_poll_upload";
   function Import_Image return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_real_import";
   function Render return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_real_render";
   function Poll_Frame return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_real_poll_frame";
   function Restart return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_real_restart";
   function Reconfigure (Mask : Interfaces.C.int) return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_real_reconfigure";
   function Close return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_real_close";
end Desktop_Real_Bridge;
