with Desktop_Mesa_Software_Renderer;
package body Desktop_Compositor with SPARK_Mode,
  Refined_State => (Engine => Renderer.State) is
   package Renderer is new Desktop_Mesa_Software_Renderer
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
      Secondary : Boolean; Drawn, Must_Restart : out Boolean;
      Over : Boolean := False; Straight_Alpha : Boolean := False) renames Renderer.Draw_Output;
   procedure Complete_Output
     (Target : System.Address; Writer : Compositor_Pool.Ticket;
      Secondary, Poll : Boolean; Result : out Render_Completion) renames Renderer.Complete_Output;
   procedure Forget_Source (Pixels : System.Address; Result : out Source_Release) renames Renderer.Forget_Source;
   procedure Forget_Targets (Result : out Target_Release) renames Renderer.Forget_Targets;
end Desktop_Compositor;
