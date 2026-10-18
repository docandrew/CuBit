with System;
package body Teapot_Renderer is

   Status_OK : constant Unsigned_32 := 0;

   type Target_View is record
      Image, View, Ready : Unsigned_64 := 0;
      Width, Height, Format, Slot : Unsigned_32 := 0;
      Generation, Reserved : Unsigned_32 := 0;
   end record with Convention => C;

   function VK_Create (Device : access constant CuBit.UI.GPU_Canvas.Device_View;
                       Format : Unsigned_32; Vertices : System.Address;
                       Count, Bytes : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "teapot_vk_create";
   function VK_Render (T : access constant Target_View; Draws : access constant Draw_List;
                       Count : Unsigned_32; Clear : access constant Clear_Color) return Unsigned_32
     with Import, Convention => C, External_Name => "teapot_vk_render";
   procedure VK_Destroy
     with Import, Convention => C, External_Name => "teapot_vk_destroy";

   procedure Create (Device : CuBit.UI.GPU_Canvas.Device_View; Format : Unsigned_32;
                     Mesh : Teapot_Mesh.Vertex_Array; OK : out Boolean) is
      D : aliased constant CuBit.UI.GPU_Canvas.Device_View := Device;
   begin
      OK := VK_Create (D'Access, Format, Mesh'Address, Teapot_Mesh.Vertex_Count,
                       Teapot_Mesh.Vertex_Bytes) = Status_OK;
   end Create;

   procedure Render (T : CuBit.UI.GPU_Canvas.Target; Draws : Draw_List;
                     Clear : Clear_Color; OK : out Boolean) is
      View : aliased constant Target_View :=
        (Image => Unsigned_64 (T.Image), View => Unsigned_64 (T.View),
         Ready => Unsigned_64 (T.Ready), Width => Unsigned_32 (T.Width),
         Height => Unsigned_32 (T.Height), Format => T.Format,
         Slot => Unsigned_32 (T.Slot) - 1,   --  widget slots count from 1
         Generation => Unsigned_32 (T.Generation), Reserved => 0);
      List : aliased constant Draw_List := Draws;
      Color : aliased constant Clear_Color := Clear;
   begin
      OK := VK_Render (View'Access, List'Access, Draw_List'Length, Color'Access) = Status_OK;
   end Render;

   procedure Destroy is
   begin
      VK_Destroy;
   end Destroy;

end Teapot_Renderer;
