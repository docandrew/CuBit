with Interfaces.C;
package Desktop_Glyph_Residency_Real_Bridge is
   function Open return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_glyph_residency_real_open";
   function Start (Version : Interfaces.C.int) return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_glyph_residency_real_start";
   function Import_Image return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_glyph_residency_real_import";
   function Render return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_glyph_residency_real_render";
   function Poll_Upload return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_glyph_residency_real_poll_upload";
   function Poll_Frame return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_glyph_residency_real_poll_frame";
   function Close return Interfaces.C.int with Export, Convention => C, External_Name => "desktop_glyph_residency_real_close";
end Desktop_Glyph_Residency_Real_Bridge;
