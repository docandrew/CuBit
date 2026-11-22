with Teapot_Mesh;
with Teapot_Renderer;
with Teapot_Scene; use Teapot_Scene;

package body Teapot_Gallery is

   package GC renames CuBit.UI.GPU_Canvas;
   use type GC.Availability, GC.Acquire_Status;

   --  Space between and around the pots, logical pixels.
   Logical_Gap : constant := 8;
   --  The canvas background behind the pots (linear RGBA).
   Clear : constant Teapot_Renderer.Clear_Color := [0.055, 0.063, 0.086, 1.0];
   Label_Color : constant CuBit.UI.Color := 16#D9DEE8#;
   Label_Inset : constant := 6;
   --  Height kept free for the label at the bottom of each cell, logical.
   Label_Strip : constant := 26;

   Mesh : Teapot_Mesh.Vertex_Array;
   Started : Boolean := False;

   --  Presentation rate over windows of Rate_Window_Milliseconds.
   Rate_Window_Milliseconds : constant := 1_000;
   Tenths_Per_Frame_Second : constant := 10;
   Window_Start : Unsigned_64 := 0;
   Window_Frames : Natural := 0;
   Last_Rate : Natural := 0;

   procedure Start (Canvas : in out GC.Canvas_Widget; OK : out Boolean) is
   begin
      OK := True;
      if Started or else GC.State (Canvas) /= GC.Ready then
         return;
      end if;
      Teapot_Mesh.Build (Mesh);
      Teapot_Renderer.Create (GC.Device (Canvas), GC.Target_Format, Mesh, OK);
      Started := OK;
   end Start;

   procedure Step (Canvas : in out GC.Canvas_Widget; Seconds : Float;
                   Status : out GC.Acquire_Status) is
      T : GC.Target;
      Draws : Teapot_Renderer.Draw_List;
      Area : constant CuBit.UI.Rect := GC.Area (Canvas);
      Gap, Strip : Natural;
      Box : Cell;
      Time : constant Scene_Time := Wrap (Seconds);
      OK : Boolean;
   begin
      if not Started then
         Status := GC.Unavailable;
         return;
      end if;
      GC.Acquire (Canvas, T, Status);
      if Status /= GC.Acquired then
         return;
      end if;
      --  The gap scales with the window's density (target pixels per
      --  logical pixel).
      Gap := (if Area.w = 0 then Logical_Gap else Logical_Gap * T.Width / Area.w);
      Strip := (if Area.w = 0 then Label_Strip else Label_Strip * T.Width / Area.w);
      for S in Shading loop
         Box := Cell_Of (S, Natural'Min (T.Width, Maximum_Edge),
                         Natural'Min (T.Height, Maximum_Edge), Natural'Min (Gap, Maximum_Edge));
         --  The label sits under the pot, not over it.
         Box.Height := (if Box.Height > 2 * Strip then Box.Height - Strip else Box.Height);
         if Box.Width > 0 and then Box.Height > 0 then
            Draws (S) :=
              (X => Integer_32 (Box.X), Y => Integer_32 (Box.Y),
               Width => Unsigned_32 (Box.Width), Height => Unsigned_32 (Box.Height),
               Shading => Shading'Pos (S), Reserved | Reserved_2 | Reserved_3 => 0,
               Push => Pot_Constants
                 (S, Aspect_Ratio'Max (Aspect_Ratio'First, Aspect_Ratio'Min
                   (Aspect_Ratio'Last, Float (Box.Width) / Float (Box.Height))), Time));
         end if;
      end loop;
      Teapot_Renderer.Render (T, Draws, Clear, OK);
      if OK then
         GC.Present (Canvas, T);
      else
         GC.Cancel (Canvas, T);
         Status := GC.Unavailable;
      end if;
   end Step;

   procedure Draw_Labels (Into : CuBit.UI.Canvas; Area : CuBit.UI.Rect) is
      Box : Cell;
      Text_H : constant Natural := CuBit.UI.UI_Text_Height;
   begin
      for S in Shading loop
         Box := Cell_Of (S, Natural'Min (Area.w, Maximum_Edge), Natural'Min (Area.h, Maximum_Edge),
                         Logical_Gap);
         if Box.Height > Text_H + Label_Inset and then
           Box.Width > CuBit.UI.UI_Text_Width (Label (S)) + Label_Inset
         then
            CuBit.UI.Draw_UI_Text_Transparent
              (Into, Area.x + Box.X + Label_Inset,
               Area.y + Box.Y + Box.Height - Text_H - Label_Inset, Label (S), Label_Color);
         end if;
      end loop;
   end Draw_Labels;

   function Rate_Tenths return Natural is (Last_Rate);

   procedure Note_Presented (Now_Milliseconds : Unsigned_64) is
      Elapsed : Unsigned_64;
   begin
      if Window_Start = 0 or else Now_Milliseconds < Window_Start then
         Window_Start := Now_Milliseconds;
         Window_Frames := 0;
         return;
      end if;
      Window_Frames := Window_Frames + 1;
      Elapsed := Now_Milliseconds - Window_Start;
      if Elapsed >= Rate_Window_Milliseconds then
         Last_Rate := Natural (Unsigned_64 (Window_Frames) * Rate_Window_Milliseconds *
                               Tenths_Per_Frame_Second / Elapsed);
         Window_Start := Now_Milliseconds;
         Window_Frames := 0;
      end if;
   end Note_Presented;

   procedure Stop is
   begin
      if Started then
         Teapot_Renderer.Destroy;
         Started := False;
      end if;
   end Stop;

end Teapot_Gallery;
