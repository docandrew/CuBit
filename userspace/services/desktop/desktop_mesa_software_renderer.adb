with Compositor_Damage;
with Compositor_Glyph_Renderer;
with Compositor_Glyph_Target;
with Mesa_Cache;
with Mesa_Binding.Affine;
with Compositor_Affine;
with Compositor_Policy;
with Mesa_Binding;
package body Desktop_Mesa_Software_Renderer with SPARK_Mode,
  Refined_State => (State => (Cache, Text, Choice)) is
   Cache : Mesa_Cache.State;
   package Glyphs renames Compositor_Glyph_Renderer;
   Text : Glyphs.State;
   package Selection renames Compositor_Backend_Selection;
   Choice : Selection.State;
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

   procedure Disable (Safe : out Boolean) with Post => Glyphs.Valid (Text) is
   begin
      Glyphs.Use_Software (Text, Cache, Safe);
      if Safe then
         Mesa_Cache.Shutdown (Cache);
         Safe := Mesa_Cache.Can_Retire (Cache);
      end if;
   end Disable;
   function Selected return Boolean is (True);
   function Full_Output return Boolean is (False);
   function Software_Text return Boolean is (Glyphs.Software_Active (Text))
     with Refined_Global => (Input => Text);
   procedure Begin_Output
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Writer : Compositor_Pool.Ticket; Screen : CuBit.Display_Geometry.Output;
      Secondary : Boolean; Result : out Output_Start;
      Repaint : in out Compositor_Damage.State;
      Writer_Repair : Compositor_Damage.State)
     with Refined_Global => (Input => Cache, In_Out => Choice) is
      pragma Unreferenced (Target, Target_Bytes, Writer, Screen, Secondary);
   begin
      Selection.Begin_Output (Choice);
      Result := (if Mesa_Cache.Can_Retire (Cache) then Started else Start_Unsafe);
   end Begin_Output;
   procedure Draw_Fill
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Area : CuBit.Display_Geometry.Physical_Rectangle; Color : Compositor_Formats.Word;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean)
     with Refined_Global => (In_Out => (Cache, Text)) is
      use type CuBit.Display_Geometry.Pixel_Edge, Compositor_Formats.Word;
      Target_Index : constant Mesa_Cache.Target_Slot := (if Secondary then 1 else 0);
      OK : Boolean;
   begin
      Drawn := False; Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      if Must_Restart then return; end if;
      if Area.Left >= Area.Right or Area.Top >= Area.Bottom then Drawn := True; return; end if;
      if not Mesa_Cache.Attempted (Cache) then Mesa_Cache.Initialize (Cache, True); end if;
      Mesa_Cache.Ensure (Cache, Target_Index, Target, Target_Bytes, OK);
      if OK then
         declare
            Width : constant Compositor_Formats.Word := Compositor_Formats.Word (Area.Right - Area.Left);
            Height : constant Compositor_Formats.Word := Compositor_Formats.Word (Area.Bottom - Area.Top);
            function Fits_Views (Source, Target : Compositor_Formats.Image) return Boolean is
              (Target.Writable = 1 and Compositor_Formats.Word (Area.Right) <= Target.Width and
               Compositor_Formats.Word (Area.Bottom) <= Target.Height);
            procedure Draw_Views (Library : in out Mesa_Binding.Context; Target, Source : System.Address;
                                  Result : out Compositor_Policy.Completion) is
            begin
               Mesa_Binding.Fill_View (Library, Target,
                 Compositor_Formats.Word (Area.Left), Compositor_Formats.Word (Area.Top),
                 Width, Height, Color, Result);
            end Draw_Views;
            procedure Execute is new Mesa_Cache.Render_Checked (Fits_Views, Draw_Views);
         begin
            -- The retained target is the sole operand. No source read occurs.
            Execute (Cache, Target_Index, Target_Index, Drawn);
         end;
      end if;
      Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      if not Drawn and not Must_Restart then
         declare Safe : Boolean; begin Disable (Safe); Must_Restart := not Safe; end;
      end if;
   end Draw_Fill;
   procedure Draw_Backdrop
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
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
      Drawn, Must_Restart : out Boolean) is
      pragma Unreferenced (Target, Target_Bytes, Window, Damage, Color, Secondary);
   begin
      Drawn := False; Must_Restart := False;
   end Draw_Shadow;
   procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean)
     with Refined_Global => (In_Out => (Cache, Text)) is
      Target_Index : constant Mesa_Cache.Target_Slot := (if Secondary then 1 else 0);
      OK, Safe : Boolean;
   begin
      Drawn := False; Repaint := False; Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      if Must_Restart then return; end if;
      if Length = 0 then Drawn := True; return; end if;
      if not Glyphs.Enabled (Text) then return; end if;
      if Glyphs.Software_Active (Text) then
         for I in 1 .. Length loop
            Compositor_Glyph_Target.Paint
              (Text, Cache, Target, Target_Bytes, (0, Items (I).Code, Screen.Scale), Screen,
               (Items (I).Cell.Left, Items (I).Cell.Top), Compositor_Text.Clip (Screen, Items (I).Cell, Damage), Tint, OK);
            if not OK then
               -- Earlier glyphs may already be visible in this target. Disable
               -- failed font/cache preparation before the bounded scene replay.
               Repaint := I > 1;
               Glyphs.Shutdown (Text, Cache, Safe); Must_Restart := not Safe;
               return;
            end if;
            pragma Loop_Invariant (Glyphs.Valid (Text));
         end loop;
         Drawn := True; return;
      end if;
      if not Mesa_Cache.Attempted (Cache) then Mesa_Cache.Initialize (Cache, True); end if;
      Mesa_Cache.Ensure (Cache, Target_Index, Target, Target_Bytes, OK);
      if OK then
         for I in 1 .. Length loop
            Glyphs.Queue (Text, Cache, Target_Index, (0, Items (I).Code, Screen.Scale), Screen,
              (Items (I).Cell.Left, Items (I).Cell.Top), Compositor_Text.Clip (Screen, Items (I).Cell, Damage), Tint, OK);
            exit when not OK;
            pragma Loop_Invariant (Glyphs.Valid (Text));
         end loop;
         if OK then
            Glyphs.Flush (Text, Cache, Drawn);
            Repaint := not Drawn;
         end if;
      end if;
      if not Drawn then
         Disable (Safe);
         Must_Restart := not Safe;
      end if;
   end Draw_Text;
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean)
     with Refined_Global => (In_Out => (Cache, Text)) is
      use Compositor_Formats;
      Target_Index : constant Mesa_Cache.Slot := (if Drag_Target then 1 else 0);
      Source_Index : Mesa_Cache.Source_Slot;
      OK : Boolean;
      Description : constant Draw :=
        (Word (Plan.Source_X), Word (Plan.Source_Y), Word (Plan.Width), Word (Plan.Height),
         Word (Plan.Target_X), Word (Plan.Target_Y), Word (Plan.Width), Word (Plan.Height),
         Word (Plan.Target_X), Word (Plan.Target_Y), Word (Plan.Width), Word (Plan.Height), 0);
   begin
      Drawn := False;
      Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      if Must_Restart then return; end if;
      if not Mesa_Cache.Attempted (Cache) then Mesa_Cache.Initialize (Cache, True); end if;
      Mesa_Cache.Ensure (Cache, Target_Index, Target, Target_Bytes, OK);
      if OK then
         Mesa_Cache.Ensure_Source (Cache, Source, Source_Bytes, Source_Index, OK);
         if OK then Mesa_Cache.Render (Cache, Target_Index, Source_Index, Description, Drawn); end if;
      end if;
      Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      if not Drawn and not Must_Restart then
         declare Safe : Boolean; begin
            Disable (Safe); Must_Restart := not Safe;
         end;
      end if;
   end Draw_Client;
   procedure Draw_Output
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output;
      Surface : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Secondary : Boolean; Drawn, Must_Restart : out Boolean;
      Over : Boolean := False; Straight_Alpha : Boolean := False)
     with Refined_Global => (In_Out => (Cache, Text)) is
      use type Compositor_Formats.Word, System.Address;
      Full : constant Compositor_Affine.Result := Compositor_Affine.Plan (Screen, Surface, Over, Straight_Alpha);
      P : constant Compositor_Affine.Result :=
        (if Full.Visible then Compositor_Affine.Clip (Full.Value, Screen.Width, Screen.Height, Damage)
         else (Visible => False));
      Target_Index : constant Mesa_Cache.Slot := (if Secondary then 1 else 0);
      Source_Index : Mesa_Cache.Source_Slot;
      OK : Boolean;
   begin
      Drawn := False;
      Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      if Must_Restart then return; end if;
      if not P.Visible then Drawn := True; return; end if;
      if not Mesa_Cache.Attempted (Cache) then Mesa_Cache.Initialize (Cache, True); end if;
      Mesa_Cache.Ensure (Cache, Target_Index, Target, Target_Bytes, OK);
      if OK then
         Mesa_Cache.Ensure_Source (Cache, Source, Source_Bytes, Source_Index, OK);
         if OK then
            declare
               Description : constant Compositor_Affine.Draw := P.Value;
               function Fits_Views (Source, Target : Compositor_Formats.Image) return Boolean is
                 (Target.Writable = 1 and Source.Pixels /= Target.Pixels and
                  Target.Width = Compositor_Formats.Word (Screen.Width) and
                  Target.Height = Compositor_Formats.Word (Screen.Height) and
                  Compositor_Affine.Valid (Description, Screen.Width, Screen.Height));
               procedure Draw_Views (Library : in out Mesa_Binding.Context;
                                    Target, Source : System.Address;
                                    Result : out Compositor_Policy.Completion) is
               begin
                  Mesa_Binding.Affine.Render
                    (Library, Target, Source, Description, Screen.Width, Screen.Height, Result);
               end Draw_Views;
               procedure Execute is new Mesa_Cache.Render_Checked (Fits_Views, Draw_Views);
            begin
               Execute (Cache, Target_Index, Source_Index, Drawn);
            end;
         end if;
      end if;
      Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      if not Drawn and not Must_Restart then
         declare Safe : Boolean; begin
            Disable (Safe); Must_Restart := not Safe;
         end;
      end if;
   end Draw_Output;
   procedure Complete_Output
     (Target : System.Address; Writer : Compositor_Pool.Ticket;
      Secondary, Poll : Boolean; Result : out Render_Completion)
     with Refined_Global => (Input => Cache) is
      pragma Unreferenced (Target, Writer, Secondary, Poll);
   begin
      Result := (if Mesa_Cache.Can_Retire (Cache) then Complete else Unsafe);
   end Complete_Output;
   procedure Forget_Source (Pixels : System.Address; Result : out Source_Release)
     with Refined_Global => (In_Out => Cache) is
      Safe : Boolean;
   begin
      Safe := Mesa_Cache.Can_Retire (Cache);
      if Safe then
         Mesa_Cache.Forget_Source (Cache, Pixels);
         Safe := Mesa_Cache.Can_Retire (Cache);
      end if;
      Result := (if Safe then Source_Retired else Source_Unsafe);
   end Forget_Source;
   procedure Forget_Targets (Result : out Target_Release)
     with Refined_Global => (In_Out => Cache) is
      Safe : Boolean;
   begin
      Safe := Mesa_Cache.Can_Retire (Cache);
      if Safe then
         Mesa_Cache.Forget_Targets (Cache);
         Safe := Mesa_Cache.Can_Retire (Cache) and then Mesa_Cache.Targets_Clear (Cache);
      end if;
      Result := (if Safe then Targets_Retired else Targets_Unsafe);
   end Forget_Targets;
end Desktop_Mesa_Software_Renderer;
