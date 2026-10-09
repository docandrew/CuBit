with Compositor_Damage;
with Compositor_Software_Text;
with Compositor_Software_Text_Target;
package body Desktop_Compositor with SPARK_Mode,
  Refined_State => (Engine => (Text, Choice)) is
   package Glyphs renames Compositor_Software_Text;
   Text : Glyphs.State;
   package Selection renames Compositor_Backend_Selection;
   Choice : Selection.State;
   function Readback_Work return Transfer_Counters is ((others => <>));
   procedure Configure_Renderer
     (Evidence : Selection.Readiness; Accepted : out Boolean)
     with Refined_Global => (In_Out => Choice) is
   begin
      -- A software build cannot authenticate GPU readiness. Reject such
      -- evidence without changing its selection or granting GPU authority.
      if Selection.Ready (Evidence) then
         Accepted := False;
      else
         Selection.Select_Backend (Choice, Evidence, Accepted);
      end if;
   end Configure_Renderer;
   procedure Recover_Renderer
     (Key : Selection.Recovery_Key; Writer_Retired, Repaint_Queued : Boolean;
      Result : out Recovery_Result) is
      pragma Unreferenced (Key, Writer_Retired, Repaint_Queued);
   begin
      -- GPU recovery cannot originate in this backend. Never manufacture
      -- retirement evidence for an unexpected request from the caller.
      Result := Recovery_Unsafe;
   end Recover_Renderer;

   function Selected return Boolean
     is (True);
   function Full_Output return Boolean
     is (False);
   function Software_Text return Boolean
     with Refined_Global => (Input => Text) is
   begin
      pragma Assert (Glyphs.Valid (Text));
      return Glyphs.Quiescent (Text);
   end Software_Text;
   procedure Begin_Output
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Writer : Compositor_Pool.Ticket; Screen : CuBit.Display_Geometry.Output;
      Secondary : Boolean; Result : out Output_Start;
      Repaint : in out Compositor_Damage.State;
      Writer_Repair : Compositor_Damage.State)
     with Refined_Global => (Input => Text, In_Out => Choice) is
      pragma Unreferenced (Target, Target_Bytes, Writer, Screen, Secondary);
   begin
      Selection.Begin_Output (Choice);
      pragma Assert (Glyphs.Valid (Text));
      Result := (if Glyphs.Quiescent (Text) then Started else Start_Unsafe);
   end Begin_Output;
   procedure Draw_Fill
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Area : CuBit.Display_Geometry.Physical_Rectangle; Color : Compositor_Formats.Word;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean)
     with Refined_Global => (Proof_In => Text) is
   begin pragma Assert (Glyphs.Valid (Text)); Drawn := False; Must_Restart := False; end Draw_Fill;
   procedure Draw_Backdrop
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Refined_Global => (Proof_In => Text) is
      pragma Unreferenced (Target, Target_Bytes, Style, Secondary);
   begin
      pragma Assert (Glyphs.Valid (Text));
      Drawn := False; Must_Restart := False;
   end Draw_Backdrop;
   procedure Draw_Preview
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean) is
      pragma Unreferenced (Target, Target_Bytes, Bounds, Damage, Style, Secondary);
   begin
      Drawn := False; Must_Restart := False;
   end Draw_Preview;
   procedure Draw_Shadow
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Window : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Color : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Refined_Global => (Proof_In => Text) is
      pragma Unreferenced (Target, Target_Bytes, Window, Damage, Color, Secondary);
   begin
      pragma Assert (Glyphs.Valid (Text));
      Drawn := False; Must_Restart := False;
   end Draw_Shadow;
   procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean)
     with Refined_Global => (In_Out => Text) is
      pragma Unreferenced (Secondary);
      OK : Boolean;
   begin
      pragma Assert (Glyphs.Valid (Text));
      Drawn := False; Repaint := False; Must_Restart := False;
      if Length = 0 then Drawn := True; return; end if;
      if not Glyphs.Enabled (Text) or else not Compositor_Software_Text_Target.Supported
        (Target, Target_Bytes, Screen) then return; end if;
      for I in 1 .. Length loop
         Compositor_Software_Text_Target.Paint
           (Text, Target, Target_Bytes, (0, Items (I).Code, Screen.Scale),
            Screen, (Items (I).Cell.Left, Items (I).Cell.Top),
            Compositor_Text.Clip (Screen, Items (I).Cell, Damage), Tint, OK);
         if not OK then
            -- Earlier glyphs may have blended into the target. Replay before
            -- publication rather than blending those glyphs a second time.
            Glyphs.Disable (Text);
            Repaint := I > 1;
            return;
         end if;
         pragma Loop_Invariant (Glyphs.Valid (Text));
      end loop;
      Drawn := True;
   end Draw_Text;
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Refined_Global => (Proof_In => Text) is
      pragma Unreferenced (Target, Source, Target_Bytes, Source_Bytes, Plan, Drag_Target);
   begin
      pragma Assert (Glyphs.Valid (Text));
      Drawn := False; Must_Restart := False;
   end Draw_Client;
   procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean;
      Over : Boolean := False; Straight_Alpha : Boolean := False)
     with Refined_Global => (Proof_In => Text) is
   begin
      pragma Assert (Glyphs.Valid (Text));
      Drawn := False;
      Must_Restart := False;
   end Draw_Output;
   procedure Complete_Output
     (Target : System.Address; Writer : Compositor_Pool.Ticket;
      Secondary, Poll : Boolean; Result : out Render_Completion)
     with Refined_Global => (Input => Text) is
      pragma Unreferenced (Target, Writer, Secondary, Poll);
   begin
      pragma Assert (Glyphs.Valid (Text));
      Result := (if Glyphs.Quiescent (Text) then Complete else Unsafe);
   end Complete_Output;
   procedure Forget_Source (Pixels : System.Address; Result : out Source_Release)
     with Refined_Global => (Proof_In => Text) is
      pragma Unreferenced (Pixels);
   begin
      pragma Assert (Glyphs.Valid (Text));
      Result := Source_Retired;
   end Forget_Source;
   procedure Forget_Targets (Result : out Target_Release)
     with Refined_Global => (Proof_In => Text) is
   begin
      pragma Assert (Glyphs.Valid (Text));
      Result := Targets_Retired;
   end Forget_Targets;
end Desktop_Compositor;
