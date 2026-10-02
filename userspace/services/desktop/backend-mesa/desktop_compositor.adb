with Compositor_Glyph_Renderer;
with Compositor_Glyph_Target;
with Mesa_Cache;
with Mesa_Binding.Affine;
with Compositor_Affine;
with Compositor_Policy;
with Mesa_Binding;
package body Desktop_Compositor with SPARK_Mode,
  Refined_State => (Engine => (Cache, Text)) is
   Cache : Mesa_Cache.State;
   package Glyphs renames Compositor_Glyph_Renderer;
   Text : Glyphs.State;
   procedure Disable (Safe : out Boolean) with Post => Glyphs.Valid (Text) is
   begin
      Glyphs.Use_Software (Text, Cache, Safe);
      if Safe then
         Mesa_Cache.Shutdown (Cache);
         Safe := Mesa_Cache.Can_Retire (Cache);
      end if;
   end Disable;
   function Selected return Boolean is (True);
   function Software_Text return Boolean is (Glyphs.Software_Active (Text));
   procedure Draw_Text
     (Target : Compositor_Formats.Image; Target_Bytes : Compositor_Formats.Byte_Count;
      Screen : CuBit.Display_Geometry.Output; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Tint : Compositor_Formats.Word; Secondary : Boolean;
      Drawn, Repaint, Must_Restart : out Boolean) is
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
      Drawn, Must_Restart : out Boolean) is
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
      Secondary : Boolean; Drawn, Must_Restart : out Boolean) is
      use type Compositor_Formats.Word, System.Address;
      Full : constant Compositor_Affine.Result := Compositor_Affine.Plan (Screen, Surface);
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
   procedure Forget_Source (Pixels : System.Address; Safe : out Boolean) is
   begin
      Safe := Mesa_Cache.Can_Retire (Cache);
      if Safe then
         Mesa_Cache.Forget_Source (Cache, Pixels);
         Safe := Mesa_Cache.Can_Retire (Cache);
      end if;
   end Forget_Source;
   procedure Forget_Targets (Safe : out Boolean) is
   begin
      Safe := Mesa_Cache.Can_Retire (Cache);
      if Safe then
         Mesa_Cache.Forget_Targets (Cache);
         Safe := Mesa_Cache.Can_Retire (Cache) and then Mesa_Cache.Targets_Clear (Cache);
      end if;
   end Forget_Targets;
end Desktop_Compositor;
