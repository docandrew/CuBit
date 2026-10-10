with Desktop_CPU_Software_Renderer;
package body Desktop_Compositor with SPARK_Mode,
  Refined_State => (Engine => Renderer.State) is
   package Renderer is new Desktop_CPU_Software_Renderer
     (Recovery_Result, Recovery_Unsafe,
     Output_Start, Started, Start_Unsafe,
     Render_Completion, Complete, Unsafe,
     Source_Release, Source_Retired, Source_Unsafe,
     Target_Release, Targets_Retired, Targets_Unsafe);
   function Readback_Work return Transfer_Counters is ((others => <>));
   procedure Configure_Renderer (Evidence : Compositor_Backend_Selection.Readiness; Accepted : out Boolean) renames Renderer.Configure_Renderer;
   procedure Recover_Renderer (Key : Compositor_Backend_Selection.Recovery_Key;
      Writer_Retired, Repaint_Queued : Boolean; Result : out Recovery_Result) renames Renderer.Recover_Renderer;
   function Selected return Boolean renames Renderer.Selected;
   function Full_Output return Boolean renames Renderer.Full_Output;
   function Software_Text return Boolean renames Renderer.Software_Text;
   procedure Begin_Output
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Writer : Compositor_Pool.Ticket; Screen : CuBit.Display_Geometry.Output;
      Secondary : Boolean; Result : out Output_Start;
      Repaint : in out Compositor_Damage.State;
      Writer_Repair : Compositor_Damage.State) renames Renderer.Begin_Output;
   procedure Draw_Fill
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Area : CuBit.Display_Geometry.Physical_Rectangle; Color : Compositor_Formats.Word;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) renames Renderer.Draw_Fill;
   procedure Draw_Backdrop
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) renames Renderer.Draw_Backdrop;
   procedure Draw_Preview
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) renames Renderer.Draw_Preview;
   procedure Draw_Shadow
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Window : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Color : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) renames Renderer.Draw_Shadow;
   procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean) renames Renderer.Draw_Text;
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean) renames Renderer.Draw_Client;
   procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Key : Compositor_Source_Content.Source_Key;
      Version : Compositor_Source_Content.Content_Version;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
      pragma Unreferenced (Key, Version);
   begin
      Renderer.Draw_Output (Target, Source, Target_Bytes, Source_Bytes, Screen, Surface,
        Damage, Secondary, Drawn, Must_Restart);
   end Draw_Output;
   procedure Note_Source_Change
     (Key : Compositor_Source_Content.Source_Key; Rows : Compositor_Source_Content.Row_Band) is null;
   procedure Retire_Source (Key : Compositor_Source_Content.Source_Key) is null;
   procedure Draw_Icon
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Item : Desktop_Icon_Pixels.Asset;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
      pragma Unreferenced (Target, Target_Bytes, Screen, Item, Surface, Damage, Secondary);
   begin
      Drawn := False; Must_Restart := False;
   end Draw_Icon;
   procedure Draw_Cursor
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Cursor : Desktop_Cursors.Cursor_ID;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
      pragma Unreferenced (Target, Target_Bytes, Screen, Cursor, Surface, Damage, Secondary);
   begin
      Drawn := False; Must_Restart := False;
   end Draw_Cursor;
   function Backing_Events (Class : Vulkan_Submission.Source_Class; Freed : Boolean)
     return Interfaces.Unsigned_64 is (0);
   function Resident_Sources return Natural is (0);
   function Upload_Progress return Interfaces.Unsigned_64 is (0);
   function Peak_Scene_Layers return Natural is (0);
   function Placeholder_Draws return Natural is (0);
   function Last_Retry_Cause return Retry_Cause is (No_Retry);
   procedure Complete_Output
     (Target : System.Address; Writer : Compositor_Pool.Ticket;
      Secondary, Poll : Boolean; Result : out Render_Completion) renames Renderer.Complete_Output;
   procedure Forget_Source (Pixels : System.Address; Result : out Source_Release) renames Renderer.Forget_Source;
   procedure Forget_Targets (Result : out Target_Release) renames Renderer.Forget_Targets;
end Desktop_Compositor;
